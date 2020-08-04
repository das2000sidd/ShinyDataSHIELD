observeEvent(input$connect_server, {
  tryCatch({
    builder <- newDSLoginBuilder()
    builder$append(server = "server1", url = input$url,
                   user = input$user, password = input$password,
                   driver = "OpalDriver")
    logindata <- builder$build()
    conns <- datashield.login(logins = logindata)
    tables <- data.table(str_split(dsListTables(conns$server1), "[.]", simplify = TRUE, 2), "table")
    resources <- data.table(str_split(dsListResources(conns$server1), "[.]", simplify = TRUE, 2), "resource")
    lists$tab_res <- rbind(tables, resources)
    colnames(lists$tab_res) <- c("project", "res", "type")
    lists$projects <- unique(lists$tab_res$project)
    
    datashield.logout(conns)

    output$project_selector <- renderUI({
      selectInput("project_selected", "Project", lists$projects)
    })
    toggleElement("add_server")
    toggleElement("remove_server")
    toggleElement("connect_server")
    toggleElement("connect_selected")
  },
  error = function(w){
    datashield.logout(conns)
    shinyalert("Oops!", "Not able to connect", type = "error")
  })
})

observeEvent(input$project_selected, {
  lists$resources <- lists$tab_res[project == input$project_selected]$res
    
  output$resource_selector <- renderUI({
    selectInput("resource_selected", "Resource", lists$resources, multiple = TRUE)
  })
})

observeEvent(input$add_server, {
  if(lists$tab_res[project == input$project_selected & res == input$resource_selected[1]]$type == "table"){
    connection$isTable <- TRUE
  }
  else {
    connection$isTable <- FALSE
  }
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
        # connection$isTable <- TRUE
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
        # connection$isTable <- FALSE
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
            # break
            lists$available_tables <- lists$available_tables[resource_internal == paste0("resource", i), type_resource := "ssh"]
          }
          # Otherwise coerce to R object
          else {
            expression = paste0("datashield.assign.expr(symbol = '", paste0("resource", i), "', 
                       expr = quote(as.resource.object(", paste0("resource", i), ")), conns = connection$conns)")
            eval(str2expression(expression))
            resource_type <- unlist(ds.class(paste0("resource", i), 
                                             datasources = connection$conns))
            if("GdsGenotypeReader" %in% resource_type) {
              lists$available_tables <- lists$available_tables[resource_internal == paste0("resource", i), type_resource := "r_obj_vcf"]
            }
            else if("ExpressionSet" %in% resource_type) {
              lists$available_tables <- lists$available_tables[resource_internal == paste0("resource", i), type_resource := "r_obj_eset"]
              expr <- paste0("datashield.assign.expr(connection$conns, symbol = 'table1', 
                                   expr = quote(", paste0("resource", i), "))")
              eval(str2expression(expr))
              
              lists$resource_variables <- ds.varLabels(paste0("resource", i), datasources = connection$conns)$server1
              lists$table_columns <- lists$resource_variables
            }
            else if("RangedSummarizedExperiment" %in% resource_type) {
              lists$available_tables <- lists$available_tables[resource_internal == paste0("resource", i), type_resource := "r_obj_rse"]
              expr <- paste0("datashield.assign.expr(connection$conns, symbol = 'table1', 
                                   expr = quote(", paste0("resource", i), "))")
              eval(str2expression(expr))
              
              lists$resource_variables <- ds.varLabels(paste0("resource", i), datasources = connection$conns)$server1
              lists$table_columns <- lists$resource_variables
            }
            else {
              lists$available_tables <- lists$available_tables[resource_internal == paste0("resource", i), type_resource := "r_obj"]
            }
            
          }
        }
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

