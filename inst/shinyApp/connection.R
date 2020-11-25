lapply(1:max_servers, function(x){
  observeEvent(input[[paste0("info_opal_", x)]], {
    url <- input[[paste0("url", x)]]
    project <- input[[paste0("project_selected", x)]]
    res <- input[[paste0("resource_selected", x)]]
    if(input[[paste0("tbl_res", x)]] == TRUE){
      is_table <- "TABLES"
    }
    else{
      is_table <- "RESOURCES"
    }
    for(i in res){
      url_builder <- paste0(url, "ui/index.html#!project;name=", project, ";tab=", is_table, 
                            ";path=", paste(project, i, sep = "."))
      browseURL(url_builder)
    }
  })
})

observeEvent(input$add, {
  tabIndex(tabIndex() + 1)
  appendTab("tabset1", tabPanel(paste0("Server", tabIndex()), 
                                fluidRow(
                                  column(6,
                                         h3("URL"),
                                         textInput(paste0("url", tabIndex()), "Opal server", value = "https://opal-demo.obiba.org/")
                                  ),
                                  column(6,
                                         h3("Credentials"),
                                         conditionalPanel(
                                           condition = paste0("input.pat_switch", tabIndex(), "== true"),
                                           passwordInput(paste0("pat", tabIndex()), "Personal Access Token"),
                                         ),
                                         conditionalPanel(
                                           condition = paste0("input.pat_switch" , tabIndex(), "== false"),
                                           textInput(paste0("user",tabIndex()), "User", value = "administrator"),
                                           tags$head(tags$script(HTML(paste0(
                                             '$(document).keyup(function(event) {
                                              if ($("#password', tabIndex(), '").is(":focus") && (event.keyCode == 13)) {
                                                  $("#connect_server', tabIndex(), '").click();
                                              };
                                              if ($("#pat', tabIndex(), '").is(":focus") && (event.keyCode == 13)) {
                                                  $("#connect_server', tabIndex(), '").click();
                                              }
                                              });'
                                           )))),
                                           passwordInput(paste0("password", tabIndex()), "Password", value = "password")
                                         ),
                                         materialSwitch(inputId = paste0("pat_switch", tabIndex()), label = "Use Personal Access Token", status = "primary")
                                  )
                                ),
                                hr(),
                                fluidRow(
                                  column(12,
                                         hidden(switchInput(
                                           inputId = paste0("tbl_res", tabIndex()), value = TRUE, #label = "Tables or resources?",
                                           onLabel = "Tables", offLabel = "Resources",
                                           onStatus = "primary", offStatus = "primary"
                                         ))
                                  )
                                ),
                                fluidRow(
                                  column(6,
                                         uiOutput(paste0("project_selector", tabIndex())),
                                  ),
                                  column(6,
                                         uiOutput(paste0("resource_selector", tabIndex())),
                                  )
                                ),
                                fluidRow(
                                  column(6,actionButton(paste0("connect_server", tabIndex()), "Connect"))
                                ),
                                hidden(actionButton(paste0("add_server", tabIndex()), "Add selected item(s)")),
                                hidden(actionButton(paste0("info_opal_", tabIndex()), "Further information of selection"))
  ), select=TRUE)
})

lapply(1:max_servers, function(x){
  observeEvent(input[[paste0("connect_server", x)]], {
    builder <- newDSLoginBuilder()
    if(input[[paste0("pat_switch", x)]]){# If user selects to use Personal Access Token
      builder$append(server = paste0("server", x), url = input[[paste0("url", x)]],
                     token = input[[paste0("pat", x)]],
                     driver = "OpalDriver")
      connection$creds <- rbind(connection$creds, data.table(server = paste0("Server", x),
                                                             url = input[[paste0("url", x)]],
                                                             user = NA,
                                                             pass = NA,
                                                             token = input[[paste0("pat", x)]]),
                                fill = TRUE)
    }
    else{# User uses regular user and password method
      builder$append(server = paste0("server", x), url = input[[paste0("url", x)]],
                     user = input[[paste0("user", x)]], password = input[[paste0("password", x)]],
                     driver = "OpalDriver")
      connection$creds <- rbind(connection$creds, data.table(server = paste0("Server", x),
                                                             url = input[[paste0("url", x)]],
                                                             user = input[[paste0("user", x)]],
                                                             pass = input[[paste0("password", x)]],
                                                             token = NA),
                                fill = TRUE)
    }
    logindata <- builder$build()
    conns <- datashield.login(logins = logindata)
    
    tab_key <- paste0("tab", x)
    res_key <- paste0("res", x)
    
    tables_resources[[tab_key]] <- tryCatch(data.table(str_split(dsListTables(conns[[paste0("server", x)]]), "[.]", simplify = TRUE, 2), "table"),
                                                     error = function(w) { data.table() })
    tables_resources[[res_key]] <- tryCatch(data.table(str_split(dsListResources(conns[[paste0("server", x)]]), "[.]", simplify = TRUE, 2), "resource"),
                                                     error = function(w) { data.table() })
    # lists$tab_res <- rbind(tables, resources)
    if (nrow(tables_resources[[tab_key]])) {
      colnames(tables_resources[[tab_key]]) <- c("project", "res", "type")
      projects_tab <- unique(tables_resources[[tab_key]]$project)
    } else {
      projects_tab <- data.table()
    }
    if (nrow(tables_resources[[res_key]])) {
      colnames(tables_resources[[res_key]]) <- c("project", "res", "type")
      projects_res <- unique(tables_resources[[res_key]]$project)
    } else {
      projects_res <- data.table()
    }
    
    datashield.logout(conns)
    
    output[[paste0("project_selector", x)]] <- renderUI({
      if(input[[paste0("tbl_res", x)]] == TRUE){# TABLES
        selectInput(paste0("project_selected", x), "Project", projects_tab, selected = NULL)
      }
      else{# RESOURCES
        selectInput(paste0("project_selected", x), "Project", projects_res, selected = NULL)
      }
    })
    toggleElement(paste0("add_server", x))
    toggleElement(paste0("remove_server", x))
    toggleElement(paste0("connect_server", x))
    showElement("connect_selected")
    showElement("remove_item")
    toggleElement(paste0("tbl_res", x))
    toggleElement(paste0("info_opal_", x))
  })
})

lapply(1:max_servers, function(x){
  observeEvent(input[[paste0("project_selected", x)]], {
    tryCatch({
      if(input[[paste0("tbl_res", x)]] == TRUE){# TABLES
        resources <- tables_resources[[paste0("tab", x)]][project == input[[paste0("project_selected", x)]]]$res
      }
      else{#  RESOURCES
        resources <- tables_resources[[paste0("res", x)]][project == input[[paste0("project_selected", x)]]]$res
      }
      output[[paste0("resource_selector", x)]] <- renderUI({
        if(input[[paste0("tbl_res", x)]] == TRUE){# TABLES
          selectInput(paste0("resource_selected", x), "Table", resources, multiple = TRUE)
        }
        else{#  RESOURCES
          selectInput(paste0("resource_selected", x), "Resource", resources, multiple = TRUE)
        }
      })
    }, error = function(w){})
  })
})


observeEvent(input$remove, {
  if(tabIndex() > 1){
    removeTab("tabset1", target=paste0("Server", tabIndex()))
    tabIndex(tabIndex() - 1)
  }
})

observeEvent(input$remove_item, {
  if(is.null(input$server_resources_table_rows_selected)){
    showNotification("Please select a row to remove", duration = 2, closeButton = FALSE, type = "error")
  }
  else{
    connection$server_resources <- connection$server_resources[-input$server_resources_table_rows_selected,]
  }
})

lapply(1:max_servers, function(x){
  observeEvent(input[[paste0("add_server", x)]], {
    if(input[[paste0("tbl_res", x)]] == TRUE){# TABLES
      if(is.null(input[[paste0("resource_selected", x)]])){
        showNotification("Please select a table", duration = 2, closeButton = FALSE, type = "error")
      }
      else{
        if(nrow(merge(connection$server_resources, data.frame(server = paste0("Server", x),
                                                              study_server = paste0("Study", x),
                                                              project = input[[paste0("project_selected", x)]], 
                                                              resources = NA, 
                                                              table = paste(input[[paste0("resource_selected", x)]])))) == 0){
          connection$server_resources <- rbind(connection$server_resources, data.frame(server = paste0("Server", x), 
                                                                                       study_server = paste0("Study", x),
                                                                                       project = input[[paste0("project_selected", x)]], 
                                                                                       resources = NA, 
                                                                                       table = paste(input[[paste0("resource_selected", x)]])))
        }
        else{
          showNotification("Duplicate tables not allowed", duration = 2, closeButton = FALSE, type = "error")
        }
      }
      
    }
    else{# RESOURCES
      if(is.null(input[[paste0("resource_selected", x)]])){
        showNotification("Please select a resource", duration = 2, closeButton = FALSE, type = "error")
      }
      else{
        if(nrow(merge(connection$server_resources, data.frame(server = paste0("Server", x), 
                                                              study_server = paste0("Study", x),
                                                              project = input[[paste0("project_selected", x)]], 
                                                              resources = paste(input[[paste0("resource_selected", x)]]),
                                                              table = NA))) == 0){
          connection$server_resources <- rbind(connection$server_resources, data.frame(server = paste0("Server", x), 
                                                                                       study_server = paste0("Study", x),
                                                                                       project = input[[paste0("project_selected", x)]], 
                                                                                       resources = paste(input[[paste0("resource_selected", x)]]),
                                                                                       table = NA))
        }
        else{
          showNotification("Duplicate resources not allowed", duration = 2, closeButton = FALSE, type = "error")
        }
      }
    }
  })
})


observeEvent(input$connect_selected, {
  
  withProgress(message = "Connecting to selected studies", value = 0.5, {
    tryCatch({
      # Create all the study servers
      connection$builder <- newDSLoginBuilder()
      creds <- merge(connection$creds, connection$server_resources)
      creds$server <- creds$study_server
      creds$study_server <- NULL
      creds$project <- NULL
      creds$resources <- NULL
      creds$table <- NULL
      creds <- unique(creds)
      for(i in 1:nrow(creds)) {
        if(!is.na(creds[i, ]$token)){# Personal Access Token
          connection$builder$append(server = creds[i, ]$server, url = creds[i, ]$url,
                                    token = creds[i, ]$token,
                                    driver = "OpalDriver")
        }
        else{# User uses regular user and password method
          connection$builder$append(server = creds[i, ]$server, url = creds[i, ]$url,
                                    user = creds[i, ]$user, password = creds[i, ]$pass,
                                    driver = "OpalDriver")
        }
      }
      # Login into the servers
      connection$logindata <- connection$builder$build()
      connection$conns <- datashield.login(logins = connection$logindata)
      
    }, error = function(w){shinyalert("Oops!", "Could not connect to the servers", type = "error")})
    
    tryCatch({
      # Load resources and tables
      resources <- connection$server_resources
      resources <- as.data.table(resources)
      for(i in 1:nrow(resources)){
        if(is.na(resources[i, resources])){ # Tables
          server_index <- which(resources$study_server[i] == names(connection$conns))
          name <- paste0(   resources[i,]$project, ".", resources[i,]$table)
          
          datashield.assign.expr(connection$conns[server_index], make.names(paste0(name, ".t")), as.symbol('1'))
          datashield.assign.table(connection$conns[server_index], make.names(paste0(name, ".t")), name)

          lists$available_tables <- rbind(lists$available_tables, c(name = make.names(paste0(name, ".t")), server_index = server_index,
                                                  server = resources$study_server[i], type_resource = "table"))
        }
        else{ # Resources
          server_index <- which(resources$study_server[i] == names(connection$conns))
          name <- paste0(resources[i,]$project, ".", resources[i,]$resources)
          
          datashield.assign.expr(connection$conns[server_index], make.names(paste0(name, ".r")), as.symbol('1'))
          datashield.assign.resource(connection$conns[server_index], make.names(paste0(name, ".r")), name)
          name <- make.names(paste0(name, ".r"))
          resource_type <- unlist(ds.class(name, datasources = connection$conns[server_index]))
        
          # c("TidyFileResourceClient", "SQLResourceClient") correspond to resources that have to be coerded to data.frame
          if (any(c("TidyFileResourceClient", "SQLResourceClient") %in% resource_type)){
            expression = paste0("datashield.assign.expr(symbol = '", paste0(str_sub(name, end=-2), "t"), "', 
                       expr = quote(as.resource.data.frame(", name, ")), conns = connection$conns[", server_index, "])")
            eval(str2expression(expression))
            # lists$available_tables <- lists$available_tables[resource_internal == paste0("resource", i), type_resource := "table"]
            # expr <- paste0("datashield.assign.expr(connection$conns, symbol = 'table1', 
            #                        expr = quote(", paste0("resource", i), "))")
            # eval(str2expression(expr))
            lists$available_tables <- rbind(lists$available_tables, c(name = paste0(str_sub(name, end=-2), "t"), server_index = server_index,
                                                                      server = resources$study_server[i], type_resource = "table"))
          }
          # "SshResourceClient" correspond to ssh resources, don't need to coerce them
          else if ("SshResourceClient" %in% resource_type){
            # break
            lists$available_tables <- rbind(lists$available_tables, c(name = name, server_index = server_index,
                                                                      server = resources$study_server[i], type_resource = "ssh"))
          }
          # Otherwise coerce to R object
          else {
            expression = paste0("datashield.assign.expr(symbol = '", name, "', 
                       expr = quote(as.resource.object(", name, ")), conns = connection$conns[", server_index, "])")
            eval(str2expression(expression))
            resource_type <- unlist(ds.class(name, datasources = connection$conns[server_index]))
            if("GdsGenotypeReader" %in% resource_type) {
              lists$available_tables <- rbind(lists$available_tables, c(name = name, server_index = server_index,
                                                                        server = resources$study_server[i], type_resource = "r_obj_vcf"))
            }
            else if("ExpressionSet" %in% resource_type) {
              lists$available_tables <- rbind(lists$available_tables, c(name = name, server_index = server_index,
                                                                        server = resources$study_server[i], type_resource = "r_obj_eset"))
              # expr <- paste0("datashield.assign.expr(connection$conns[", server_index, "], symbol = '", paste0(str_sub(name, end=-2), "t"), "', 
              #                      expr = quote(", name, "))")
              # eval(str2expression(expr))
              # lists$available_tables <- rbind(lists$available_tables, c(name = paste0(str_sub(name, end=-2), "t"), server_index = server_index,
              #                                                           server = resources$study_server[i], type_resource = "table"))
            }
            else if("RangedSummarizedExperiment" %in% resource_type) {
              lists$available_tables <- rbind(lists$available_tables, c(name = name, server_index = server_index,
                                                                        server = resources$study_server[i], type_resource = "r_obj_rse"))
              # expr <- paste0("datashield.assign.expr(connection$conns[", server_index, "], symbol = '", paste0(str_sub(name, end=-2), "t"), "', 
              #                      expr = quote(", name, "))")
              # eval(str2expression(expr))
              # lists$available_tables <- rbind(lists$available_tables, c(name = paste0(str_sub(name, end=-2), "t"), server_index = server_index,
              #                                                           server = resources$study_server[i], type_resource = "table"))
            }
            else {
              lists$available_tables <- rbind(lists$available_tables, c(name = name, server_index = server_index,
                                                                        server = resources$study_server[i], type_resource = "r_obj"))
            }
          }
        }
      }
      
      lists$available_tables <- data.table(lists$available_tables)
      
      ## HERE IMPLEMENTAR SEGONS EL lists$available_tables QUINS TABS ES MOSTREN!
      ## REVISAR PER TANT ELS SHIY ALERTS DELS TABS PK IA NO FARA FALTA MIRAR QUE COLLONS TENIM
      ## CADA COP PER SI MOSTRAR UN ERROR O NO A LA TAB, DAQUESTA FORMA TOTES LES TABS QUE ENSENYEM
      ## LES PODEM FER SERVIR XAXI PISTAXI
      
      ## table_columns_a. Accepts "table"
      if(any(unique(lists$available_tables$type_resource) %in% c("table"))) {
        show(selector = "ul li:eq(1)")
      }
      
      ## d_statistics. Accepts "table", "r_obj_eset", "r_obj_rse"
      if(any(unique(lists$available_tables$type_resource) %in% c("table"))) {
        show(selector = "ul li:eq(2)")
      }
      
      ## statistics_model. Accepts "table"
      if(any(unique(lists$available_tables$type_resource) %in% c("table"))) {
        show(selector = "ul li:eq(3)")
      }
      
      ## genomics. Accepts "ssh" and "r_obj_vcf"
      if(any(unique(lists$available_tables$type_resource) %in% c("ssh", "r_obj_vcf"))) {
        show(selector = "ul li:eq(4)")
      }
      
      ## omics. Accepts 
      if(any(unique(lists$available_tables$type_resource) %in% c("r_obj_rse", "r_obj_eset"))) {
        show(selector = "ul li:eq(7)")
      }
      
      connection$active <- TRUE
      
    }, error = function(w){
      datashield.logout(connection$conns)
      # opal.logout(connection$opal_conection)
      shinyalert("Oops!", "Broken resource", type = "error")
    })
  })
  ## IMPLEMENT CONSISTENCY CHECK IF MULTIPLE STUDIES, IF TABLES , ALL STUDIES MUST HAVE SAME COLUMNS!
})

