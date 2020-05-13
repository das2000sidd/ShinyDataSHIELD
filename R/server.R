server <- function(input, output, session) {
  source("table_renders.R", local = TRUE)
  connection <- reactiveValues(builder = NULL, logindat = NULL, conns = NULL, active = FALSE, complete = FALSE)
  lists <- reactiveValues(limma_variables = NULL, limma_labels = NULL, projects = NULL, resources = NULL)
  limma_results <- reactiveValues(result_table = NULL)
  
  observeEvent(input$connect_server,{
    tryCatch({
      if((input$project == "" & input$resource == "") & !connection$complete){
        opal_conection <- opal.login(username = input$user, password = input$password, url = input$url)
        lists$projects <- opal.projects(opal_conection)$name
        opal.logout(opal_conection)
        
        output$project_selector <- renderUI({
          selectInput("project_selected", "Project", lists$projects)
        })
        
        toggleElement("optional_banner")
        toggleElement("project")
        toggleElement("resource")
      }
      
      else{
        if (!is.null(input$resource_selected)) {
          resource <- paste0(list(input$project_selected, input$resource_selected), collapse = ".")
        }
        else {
          resource <- paste0(list(input$project, input$resource), collapse = ".")
        }
        withProgress(message = "Connecting to server", {
          connection$builder <- newDSLoginBuilder()
          connection$builder$append(server = "opal_server", url = input$url,
                                    user = input$user, password = input$password,
                                    resource = resource, driver = "OpalDriver")
          connection$logindata <- connection$builder$build()
          connection$conns <- datashield.login(logins = connection$logindata, assign = TRUE,
                                               symbol = "client")
          tryCatch({
            datashield.assign.expr(connection$conns, symbol = "resource_opal", 
                                   expr = quote(as.resource.object(client)))
            connection$active <- TRUE
          }, error = function(w){
            datashield.logout(connection$conns)
            shinyalert("Oops!", "Broken resource", type = "error")
          })
        })
      }
    }, error = function(w){
      shinyalert("Oops!", "Not able to connect", type = "error")
    })
  })
  
  observe({
    if(!is.null(input$project_selected)){
      opal_conection <- opal.login(username = input$user, password = input$password, url = input$url)
      lists$resources <- opal.resources(opal_conection, input$project_selected)$name
      opal.logout(opal_conection)
      
      output$resource_selector <- renderUI({
        selectInput("resource_selected", "Resource", lists$resources)
      })
      connection$complete <- TRUE
    }
  })
  
  observeEvent(input$run_shell, {
    plink.arguments <- input$command
    browser()
    ans.plink <- ds.PLINK("resource_opal", plink.arguments, datasources = connection$conns)
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
  })
  
  onclick('connection_display',
          datashield.logout(connection$conns),
          connection$active <- FALSE
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
      else {
        withProgress(message = "Loading Limma parameters", value = 0, {
          incProgress(0.2)
          lists$limma_variables <- ds.varLabels("resource_opal", datasources = connection$conns)
          lists$limma_labels <- ds.fvarLabels("resource_opal", datasources = connection$conns)
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
  })
}