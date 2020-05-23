server <- function(input, output, session) {
  source("table_renders.R", local = TRUE)
  source("plot_renders.R", local = TRUE)
  connection <- reactiveValues(builder = NULL, logindat = NULL, conns = NULL, active = FALSE, complete = FALSE, opal_conection = FALSE, isTable = FALSE, server_resource = list())
  lists <- reactiveValues(limma_variables = NULL, limma_labels = NULL, projects = NULL, resources = NULL, vcf_covars = NULL)
  limma_results <- reactiveValues(result_table = NULL)
  plink_results <- reactiveValues(result_table = NULL)
  vcf_results <- reactiveValues(result_table_gwas = NULL)
  manhattan_gwas <- reactiveValues(data = NULL, featureCol = NULL, chrCol = NULL, posCol = NULL, pvalCol = NULL)
  
  observeEvent(input$connect_server,{
    tryCatch({
      if((input$project == "" & input$resource == "") & !connection$complete){
        connection$opal_conection <- opal.login(username = input$user, password = input$password, url = input$url)
        lists$projects <- opal.projects(connection$opal_conection)$name
        output$project_selector <- renderUI({
          selectInput("project_selected", "Project", lists$projects)
        })
        toggleElement("optional_banner")
        toggleElement("project")
        toggleElement("resource")
        toggleElement("selector_optional_table")
      }
      
      else{
        if (is.null(input$resource_selected)) {
          resource <- paste0(list(input$project, input$resource), collapse = ".")
          if (input$selector_optional_table == "Table") {connection$isTable <- TRUE}
        }
        
        withProgress(message = "Connecting to server", {
          connection$builder <- newDSLoginBuilder()
          if (connection$isTable) {
            if(!exists("resource")){
              iter <- 0
              for(res in input$resource_selected) {
                iter <- iter + 1
                server <- paste0("server", iter)
                resource <- paste0(list(input$project_selected, res), collapse = ".")
                connection$server_resource[[server]] <- resource
                connection$builder$append(server = server, url = input$url,
                                          user = input$user, password = input$password,
                                          table = resource, driver = "OpalDriver")
              }
            }
            else {
              connection$builder$append(server = server, url = input$url,
                                        user = input$user, password = input$password,
                                        table = resource, driver = "OpalDriver")
            }
          }
          else {
            if(!exists("resource")){
              iter <- 0
              for(res in input$resource_selected) {
                iter <- iter + 1
                server <- paste0("server", iter)
                resource <- paste0(list(input$project_selected, res), collapse = ".")
                connection$server_resource[[server]] <- resource
                connection$builder$append(server = server, url = input$url,
                                          user = input$user, password = input$password,
                                          resource = resource, driver = "OpalDriver")
              }
            }
            else {
              connection$builder$append(server = server, url = input$url,
                                        user = input$user, password = input$password,
                                        resource = resource, driver = "OpalDriver")
            }
          }
          connection$logindata <- connection$builder$build()
          connection$conns <- datashield.login(logins = connection$logindata, assign = TRUE,
                                               symbol = "client")
          tryCatch({
            class <- ds.class("client", connection$conns)
            if (!any(c("SshResourceClient", "data.frame") %in% unlist(class))) {
              tryCatch({
                datashield.assign.expr(connection$conns, symbol = "resource_opal", 
                                       expr = quote(as.resource.object(client)))
              }, error = function(w){
                datashield.assign.expr(connection$conns, symbol = "resource_opal", 
                                       expr = quote(as.resource.data.frame(client, strict = TRUE)))
              })
            }
            else {datashield.assign.expr(connection$conns, symbol = "resource_opal", 
                                         expr = quote(client))}
            
            connection$active <- TRUE
            
          }, error = function(w){
            datashield.logout(connection$conns)
            opal.logout(connection$opal_conection)
            shinyalert("Oops!", "Broken resource", type = "error")
          })
        })
      }
    }, error = function(w){
      shinyalert("Oops!", "Not able to connect", type = "error")
    })
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
    withProgress(message = "Connecting to server", {
    plink.arguments <- input$command
    incProgress(0.4)
    plink_results$result_table <- ds.PLINK("resource_opal", plink.arguments, datasources = connection$conns)
    showElement("plink_show_plain")
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
                                             Set = "resource_opal", 
                                             datasources = connection$conns,
                                             sva = input$limma_sva,
                                             annotCols = input$limma_lab)
      incProgress(0.8)
    })
    browser()
    if (length(connection$server_resource) > 1) {
      output$limma_server_select <- renderUI({
        # EL RESOURCE SELECCIONAT MANE QUINA TAULA ES FA EL RENDER!!!! AL TABLE RENDERS
        # limma_results$result_table + INPUT$LIMMA_SERVER_RESULTS AMB UN PASTE0
        selectInput("limma_server_results", "Resource", unlist(connection$server_resource))
      })
    }
    
    browser()
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
  
  observe({
    if(input$tabs == "limma") {
      if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
      else if (!(any(c("ExpressionSet", "RangedSummarizedExperiment") %in% unlist(ds.class("resource_opal", connection$conns))))){
        shinyalert("Oops!", "Selected resource(s) is not an Expression Set or Range Summarized Experiment. Can't perform LIMMA", type = "error")
      }
      else {
        withProgress(message = "Loading Limma parameters", value = 0, {
          incProgress(0.2)
            # Take variables from the 1st selected dataset. They should be equal
          lists$limma_variables <- ds.varLabels("resource_opal", datasources = connection$conns)$server1
          lists$limma_labels <- ds.fvarLabels("resource_opal", datasources = connection$conns)$server1
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
      else if (!(any(c("SshResourceClient") %in% unlist(ds.class("resource_opal", connection$conns))))){
        shinyalert("Oops!", "Selected resource(s) is not a PLINK resource ", type = "error")
      }
      else{
        hideElement("plink_show_plain")
      }
    }
    if(input$tabs == "vcf_files"){
      if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
      else if (!(any(c("GdsGenotypeReader", "GWASTools") %in% unlist(ds.class("resource_opal", connection$conns))))){
        shinyalert("Oops!", "Selected resource(s) is not an GdsGenotypeReader or GWASTools.", type = "error")
      }
      else if (length(connection$server_resource) > 1) {
        shinyalert("Oops!", "VCF GWAS currently only implemented for 1 resource at a time", type = "error")
      }
      else{
        new_res <- gsub('.{4}$', '', connection$server_resource$server1)
        tryCatch({
          datashield.assign.resource(connection$conns, symbol = "covars.vcf", 
                                     resource = list(server1 = new_res))
          datashield.assign.expr(connection$conns, symbol = "covars", 
                                 expr = quote(as.resource.data.frame(covars.vcf)))
          lists$vcf_covars <- ds.colnames("covars", datasources = connection$conns)
        }, error = function(w){
          shinyalert("Oops!", paste0("Could not find covariables resource: ", new_res), type = "error")
        })
        
        output$vcf_ct_selector <- renderUI({
          selectInput("vcf_ct_var", "Covariable", lists$vcf_covars$server1)
        })
        
        output$vcf_selector_var <- renderUI({
          selectInput("vcf_var", "Covariable", lists$vcf_covars$server1)
        })
        output$vcf_selector_cov <- renderUI({
          selectInput("vcf_cov", "Covariable", lists$vcf_covars$server1[!(lists$vcf_covars$server1 %in% input$vcf_var)], multiple = TRUE)
        })
      }
    }
    if(input$tabs == "gwas_plot"){
      browser()
      if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
      else if (is.null(vcf_results$result_table_gwas) & is.null(plink_results$result_table)) {
        shinyalert("Oops!", "No GWAS analysis performed to display", type = "error")
      }
      else{
        if(!is.null(vcf_results$result_table_gwas)){
          manhattan_gwas$data <- vcf_results$result_table_gwas
          manhattan_gwas$featureCol <- 2
          manhattan_gwas$chrCol <- 3
          manhattan_gwas$posCol <- 4
          manhattan_gwas$pvalCol <- 11
        }
        else{
          manhattan_gwas$data <- plink_results$result_table
          manhattan_gwas$featureCol <- 2
          manhattan_gwas$chrCol <- 1
          manhattan_gwas$posCol <- 3
          manhattan_gwas$pvalCol <- 9
        }
      }
    }
  })
  
  observeEvent(input$gwas_trigger, {
    tryCatch({
      ds.GenotypeData(x='resource_opal', covars = 'covars', columnId = 1, newobj.name = 'gds.Data', datasources = connection$conns)
      resources_match <- TRUE
    }, error = function(w){
      shinyalert("Oops!", "Different individuals between vcf and covar files", type = "error")
      resources_match <- FALSE
    })
    if(resources_match){
      model <- paste0(input$vcf_var, "~", if(is.null(input$vcf_cov)){1} else{paste0(input$vcf_cov, collapse = "+")})
      vcf_results$result_table_gwas <- ds.GWAS(genoData = 'gds.Data', model = as.formula(model), datasources = connection$conns)
    }
  })
  
  observeEvent(input$stop, {
    browser()
  })
}