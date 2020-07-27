server <- function(input, output, session) {
  source("table_renders.R", local = TRUE)
  source("plot_renders.R", local = TRUE)
  connection <- reactiveValues(num_servers = 0, builder = NULL, logindat = NULL, conns = NULL, active = FALSE, complete = FALSE, opal_conection = FALSE, server_resource = list(), server_resources = NULL, isTable = NULL)
  lists <- reactiveValues(limma_variables = NULL, limma_labels = NULL, projects = NULL, resources = NULL, vcf_covars = NULL, table_columns = NULL, available_tables = NULL, available_resources = NULL, table_columns_types = NULL)
  glm_results <- reactiveValues(glm_result_table = NULL, glmer_result_table = NULL)
  limma_results <- reactiveValues(result_table = NULL)
  plink_results <- reactiveValues(result_table = NULL)
  vcf_results <- reactiveValues(result_table_gwas = NULL)

  observeEvent(input$connect_server, {
    tryCatch({
      connection$opal_conection <- opal.login(username = input$user, password = input$password, url = input$url)
      lists$projects <- opal.projects(connection$opal_conection)$name
      output$project_selector <- renderUI({
        selectInput("project_selected", "Project", lists$projects)
      })
      toggleElement("add_server")
      toggleElement("remove_server")
      toggleElement("connect_server")
      toggleElement("connect_selected")
      connection$complete <- TRUE
    },
    error = function(w){
      shinyalert("Oops!", "Not able to connect", type = "error")
    })
  })
  
  observeEvent(input$add_server, {
    connection$server_resources <- rbind(connection$server_resources, data.table(server = paste0("server", connection$num_servers + 1), project = input$project_selected, resources = paste(input$resource_selected, collapse = ", "), table = connection$isTable))
    connection$num_servers <- connection$num_servers + 1
  })
  
  observeEvent(input$remove_server, {
    connection$server_resources <- connection$server_resources[-input$server_resources_table_rows_selected,]
    connection$num_servers <- nrow(connection$server_resources)
    connection$server_resources$server <- paste0("server", seq(1:connection$num_servers))
  })
  
  observeEvent(input$connect_selected, {
    withProgress(message = "Connecting to selected studies", value = 0.5, {
      tryCatch({
        # Create all the study servers
        connection$builder <- newDSLoginBuilder()
        for(server_iter in connection$server_resources$server) {
          connection$builder$append(server = server_iter, url = input$url,
                                    user = input$user, password = input$password,
                                    driver = "OpalDriver")
        }
        
        # Login into the servers
        connection$logindata <- connection$builder$build()
        connection$conns <- datashield.login(logins = connection$logindata)
        
        # Load resources and tables
        resources <- data.table(matrix(unlist(strsplit(connection$server_resources$resources, split = ", ")), nrow = nrow(connection$server_resources), byrow = TRUE))
        if(connection$server_resources$table == TRUE) {
          connection$isTable <- TRUE
          for(i in 1:ncol(resources)) {
            aux <- as.character(unlist(resources[, i, with = FALSE]))
            table_info <- connection$server_resources[, table := connection$server_resources[, paste(project, aux, sep = ".")]]
            datashield.assign.table(connection$conns, paste0("table", i), table_info)
            lists$available_tables <- rbind(lists$available_tables, 
                                            cbind(subset(table_info, select = c("server", "table")), 
                                                  table_internal = paste0("table", i), type_resource = "table"))
          }
          lists$table_columns <- ds.colnames("table1", datasources = connection$conns)$server1
          types <- lapply(paste0("table1$", lists$table_columns), function(x) ds.class(x, datasources = connection$conns[1]))
          lists$table_columns_types <- data.frame(variable = lists$table_columns, type = unlist(types))
        }
        else {
          connection$isTable <- FALSE
          for(i in 1:ncol(resources)) {
            aux <- as.character(unlist(resources[, i, with = FALSE]))
            resource_info <- connection$server_resources[, resource := connection$server_resources[, paste(project, aux, sep = ".")]]
            datashield.assign.resource(connection$conns, paste0("resource", i), resource_info)
            lists$available_tables <- rbind(lists$available_tables, 
                                            cbind(subset(resource_info, select = c("server", "resource")), 
                                                  resource_internal = paste0("resource", i)), fill = TRUE)
            resource_type <- unlist(ds.class(paste0("resource", i), 
                                             datasources = connection$conns))
            # c("TidyFileResourceClient", "SQLResourceClient") correspond to resources that have to be coerded to data.frame
            if (any(c("TidyFileResourceClient", "SQLResourceClient") %in% resource_type)){
              expression = paste0("datashield.assign.expr(symbol = '", paste0("resource", i), "', 
                       expr = quote(as.resource.data.frame(", paste0("resource", i), ")), conns = connection$conns)")
              eval(str2expression(expression))
              lists$available_tables <- lists$available_tables[resource_internal == paste0("resource", i), type_resource := "table"]
              expr <- paste0("datashield.assign.expr(connection$conns, symbol = 'table1', 
                                   expr = quote(", paste0("resource", i), "))")
              eval(str2expression(expr))
              
              lists$table_columns <- ds.colnames("table1", datasources = connection$conns)$server1
              types <- lapply(paste0("table1$", lists$table_columns), function(x) ds.class(x, datasources = connection$conns[1]))
              lists$table_columns_types <- data.frame(variable = lists$table_columns, type = unlist(types))
              lists$available_tables[resource == resource_info$resource, table := resource]
              lists$available_tables[resource == resource_info$resource, table_internal := "table1"]
            }
            # "SshResourceClient" correspond to ssh resources, don't need to coerce them
            else if ("SshResourceClient" %in% resource_type){
              break
              lists$available_tables <- lists$available_tables[resource_internal == paste0("resource", i), type_resource := "ssh"]
            }
            # Otherwise coerce to R object
            else {
              expression = paste0("datashield.assign.expr(symbol = '", paste0("resource", i), "', 
                       expr = quote(as.resource.object(", paste0("resource", i), ")), conns = connection$conns)")
              eval(str2expression(expression))
              lists$available_tables <- lists$available_tables[resource_internal == paste0("resource", i), type_resource := "r_obj"]
            }
          }
        }
        connection$active <- TRUE
      }, error = function(w){
        datashield.logout(connection$conns)
        opal.logout(connection$opal_conection)
        shinyalert("Oops!", "Broken resource", type = "error")
      })
    })
    ## IMPLEMENT CONSISTENCY CHECK IF MULTIPLE STUDIES, IF TABLES , ALL STUDIES MUST HAVE SAME COLUMNS!
    
  })
  
  observeEvent(input$project_selected, {
    lists$resources <- tryCatch({
      connection$isTable <- FALSE
      opal.resources(connection$opal_conection, input$project_selected)$name
      }, error = function(w){NULL})
    if (is.null(lists$resources)) {
      lists$resources <- opal.tables(connection$opal_conection, input$project_selected)$name
      connection$isTable <- TRUE
    }
    output$resource_selector <- renderUI({
      selectInput("resource_selected", "Resource", lists$resources, multiple = TRUE)
    })
    connection$complete <- TRUE
  })
  
  observeEvent(input$run_shell, {
    withProgress(message = "Runnink PLINK shell command", {
    plink.arguments <- input$command
    incProgress(0.4)
    tryCatch({
      plink_results$result_table <- ds.PLINK("resource1", plink.arguments, datasources = connection$conns)
      showElement("plink_show_plain")
    }, error = function(w){
      shinyalert("Oops!", "PLINK command yielded errors", type = "error")
    })
    })
  })
  
  output$plink_results_terminal_render <- renderText({
    paste0(plink_results$result_table$server1$plink.out$output, collapse = " \n ")
  })
  
  observeEvent(input$run_limma, {
    withProgress(message = "Performing limma model", {
      limma_formula <- as.formula(paste0("~ ", paste0(input$limma_var, collapse = " + ")))
      incProgress(0.4)
      limma_results$result_table <- ds.limma(model = limma_formula,
                                             Set = "resource1", 
                                             datasources = connection$conns,
                                             sva = input$limma_sva,
                                             annotCols = input$limma_lab,
                                             type.data = input$limma_data_type)
      incProgress(0.8)
    })
    if (length(connection$server_resource) > 1) {
      output$limma_server_select <- renderUI({
        # EL RESOURCE SELECCIONAT MANE QUINA TAULA ES FA EL RENDER!!!! AL TABLE RENDERS
        # limma_results$result_table + INPUT$LIMMA_SERVER_RESULTS AMB UN PASTE0
        selectInput("limma_server_results", "Resource", unlist(connection$server_resource))
      })
    }
  })
  
  onclick('connection_display',
          connection$active <- FALSE
          )
  onclick('connection_display',
          opal.logout(connection$opal_conection)
          )
  onclick('connection_display',
          datashield.logout(connection$conns)
  )
  
  output$userpanel <- renderUI({
    sidebarUserPanel(
      if(!connection$active) {
        div(class = "status_connection",
            span(class = "dot", style = "
                height: 12px;
                width: 12px;
                background-color: red;
                border-radius: 50%;
                display: inline-block;"),
            a("No connection", style = "
                font-size: 15px;
             ")
        )
      }
      else{
        div(class = "status_connection",
            span(class = "dot", style = "
                height: 12px;
                width: 12px;
                background-color: green;
                border-radius: 50%;
                display: inline-block;"),
            a("Connected", style = "
                font-size: 15px;
             "),
            div(id = 'connection_display',
                a(id = 'connection_display', icon("sign-out"), "Disconnect", href = "")
            )
        )
      }
    ) 
  })
  
  output$d_statistics_variable_selector <- renderUI({
    selectInput("d_statistics_variable_selector_value", "Select variable", lists$table_columns)
  })
  
  output$d_statistics_table_selector <- renderUI({
    selectInput("d_statistics_table_selector_value", "Select table", lists$available_tables$table)
  })
  
  output$d_statistics_variable_selector_scatter <- renderUI({
    selectInput("d_statistics_variable_selector_scatter_value", "Select variable", lists$table_columns)
  })
  output$d_statistics_variable_selector_scatter2 <- renderUI({
    selectInput("d_statistics_variable_selector_scatter_value2", "Select variable", lists$table_columns)
  })
  
  output$d_statistics_variable_selector_histogram <- renderUI({
    selectInput("d_statistics_variable_selector_histogram_value", "Select variable", lists$table_columns)
  })
  
  output$d_statistics_variable_selector_heatmap <- renderUI({
    selectInput("d_statistics_variable_selector_heatmap_value", "Select variable", lists$table_columns)
  })
  output$d_statistics_variable_selector_heatmap2 <- renderUI({
    selectInput("d_statistics_variable_selector_heatmap_value2", "Select variable", lists$table_columns)
  })
  
  observeEvent(input$gml_toggle_variables_table, {
    toggleElement("available_variables_type")
  })
  
  observeEvent(input$perform_glm, {
    tryCatch({
      withProgress(message = "Performing GLM", value = 0.5, {
        glm_results$glm_result_table <- ds.glm(formula = as.formula(input$glm_formula), data = "table1", family = input$gml_output_family,
                                               datasources = connection$conns)
      })
    }, error = function(w){
      shinyalert("Oops!", "Error performing the GLM", type = "error")
    })
    
  })
  
  observeEvent(input$trigger_formula_help_glm, {
    shinyalert("Formula structure", "y~a+b+c+d
    Means fit a GLM with y as the outcome variable and a, b, c and d as covariates. By default all models include an intercept (regression constant) term, to exclude it use:
    y~0+a+b+c+d
    The * symbol between two covariates means fit all possible main effects and interactions for and between those two covariates, as example:
    y~a*b", type = "info")
  })
  
  observeEvent(input$gmler_toggle_variables_table, {
    toggleElement("available_variables_type2")
  })
  
  observeEvent(input$perform_glmer, {
    tryCatch({
      withProgress(message = "Performing GLMer", value = 0.5, {
        glm_results$glmer_result_table <- ds.glmerSLMA(formula = as.formula(input$glmer_formula), data = "table1", family = input$gmler_output_family,
                                                       datasources = connection$conns)
      }) 
    }, error = function(w){
      shinyalert("Oops!", "Error performing the GLMer", type = "error")
    })
    
  })
  
  observeEvent(input$trigger_formula_help_glmer, {
    shinyalert("Formula structure", "y~a+b+(1|c)
    Means fit an GLME with y as the outcome variable (e.g. a binary case-control using a logistic regression model or a count or a survival time using a Poisson regression model), a and b as fixed effects, and c as a random effect or grouping factor.
    It is also possible to fit models with random slopes by specifying a model such as
    y~a+b+(1+b|c)
    where the effect of b can vary randomly between groups defined by c. Implicit nesting can be specified with formulas such as: 
    y~a+b+(1|c/d) or y~a+b+(1|c)+(1|c:d)", type = "info")
  })
  
  observe({
    if(input$tabs == "d_statistics") {
      if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
      else if (unique(lists$available_tables$type_resource) == "table") {
        # datashield.assign.expr(connection$conns, "table1", quote(resource1))
        
      }
      else {
        shinyalert("Oops!", "Descriptive analysis only available for tables", type = "error")
      }
      # else {
      #   
      # }
    }
    if(input$tabs == "statistic_models"){
      if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
      else if (unique(lists$available_tables$type_resource) != "table") {
        shinyalert("Oops!", "Statistic models only available for tables", type = "error")
      }
    }
    if(input$tabs == "statistic_models_mixed"){
      if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
      else if (unique(lists$available_tables$type_resource) != "table") {
        shinyalert("Oops!", "Mixed statistic models only available for tables", type = "error")
      }
    }
    if(input$tabs == "limma") {
      if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
      else if (!(any(c("ExpressionSet", "RangedSummarizedExperiment") %in% unlist(ds.class("resource1", connection$conns))))){
        shinyalert("Oops!", "Selected resource(s) is not an Expression Set or Range Summarized Experiment. Can't perform LIMMA", type = "error")
      }
      else {
        withProgress(message = "Loading Limma parameters", value = 0, {
          incProgress(0.2)
            # Take variables from the 1st selected dataset. They should be equal
          lists$limma_variables <- ds.varLabels("resource1", datasources = connection$conns)$server1
          
          ## ds.fvarLabels is crashing for RSE datasets (works for eSets tho)
          
          # lists$limma_labels <- ds.fvarLabels("resource1", datasources = connection$conns)$server1
          incProgress(0.6)
          output$limma_variables_selector <- renderUI({
            selectInput("limma_var", "Variables for the limma", lists$limma_variables, multiple = TRUE)
          })
          incProgress(0.8)
          output$limma_labels_selector <- renderUI({
            selectInput("limma_lab", "Labels for the limma", lists$limma_labels, multiple = TRUE)
          })
          output$limma_sva_selector <- renderUI({
            checkboxInput("limma_sva", "SVA", value = FALSE)
          })
          output$limma_run <- renderUI({
            actionButton("run_limma", "Run limma")
          })
        })
      }
    }
    if(input$tabs == "plink"){
      if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
      else if (!(any(c("SshResourceClient") %in% unlist(ds.class("resource1", connection$conns))))){
        shinyalert("Oops!", "Selected resource(s) is not a PLINK resource ", type = "error")
      }
      else{
        hideElement("plink_show_plain")
      }
    }
    if(input$tabs == "vcf_files"){
      if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
      else if (!(any(c("GdsGenotypeReader", "GWASTools") %in% 
                     unlist(lapply(unique(lists$available_tables$resource_internal), function(x) ds.class(x, connection$conns)))))){
        shinyalert("Oops!", "Selected resource(s) is not an GdsGenotypeReader or GWASTools.", type = "error")
      }
      else if (length(unique(lists$available_tables$server)) > 1) {
        shinyalert("Oops!", "VCF GWAS currently only implemented for 1 study at a time", type = "error")
      }
      else{
        # get colnames
        lists$vcf_covars <- ds.colnames(lists$available_tables[type_resource == "table", resource_internal], 
                                        datasources = connection$conns)
        # render lists to select
        output$vcf_selector_var <- renderUI({
          selectInput("vcf_var", "Variable", lists$vcf_covars$server1)
        })
        output$vcf_selector_cov <- renderUI({
          selectInput("vcf_cov", "Covariable", lists$vcf_covars$server1[!(lists$vcf_covars$server1 %in% input$vcf_var)], multiple = TRUE)
        })
      }
    }
    if(input$tabs == "gwas_plot"){
      if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
      else if (is.null(vcf_results$result_table_gwas) & is.null(plink_results$result_table)) {
        shinyalert("Oops!", "No GWAS analysis performed to display", type = "error")
      }
    }
  })
  
  observeEvent(input$gwas_trigger, {
    tryCatch({
      ds.GenotypeData(x=lists$available_tables[type_resource == "r_obj", resource_internal], 
                      covars = lists$available_tables[type_resource == "table", resource_internal], 
                      columnId = 1, newobj.name = 'gds.Data', datasources = connection$conns)
      resources_match <- TRUE
    }, error = function(w){
      shinyalert("Oops!", "Different individuals between vcf and covar files", type = "error")
      resources_match <- FALSE
    })
    if(resources_match){
      withProgress(message = "Performing GWAS", {
        model <- paste0(input$vcf_var, "~", if(is.null(input$vcf_cov)){1} else{paste0(input$vcf_cov, collapse = "+")})
        vcf_results$result_table_gwas <- ds.GWAS(genoData = 'gds.Data', model = as.formula(model), datasources = connection$conns)
      })
    }
  })
  
  observeEvent(input$stop, {
    browser()
  })
}