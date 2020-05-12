server <- function(input, output, session) {
  source("table_renders.R", local = TRUE)
  connection <- reactiveValues(builder = NULL, logindat = NULL, conns = NULL, active = FALSE)
  lists <- reactiveValues(limma_variables = NULL, limma_labels = NULL)
  limma_results <- reactiveValues(result_table = NULL)
  
  observeEvent(input$connect_server, {
    tryCatch({
      connection$builder <- newDSLoginBuilder()
      connection$builder$append(server = "opal_server", url = input$url,
                                user = input$user, password = input$password,
                                resource = input$resource, driver = "OpalDriver")
      connection$logindata <- connection$builder$build()
      connection$conns <- datashield.login(logins = connection$logindata, assign = TRUE,
                                           symbol = "client")
      datashield.assign.expr(connection$conns, symbol = "resource_opal", 
                             expr = quote(as.resource.object(client)))
      connection$active <- TRUE
    }, error = function(w){
      shinyalert("Oops!", "Not able to connect", type = "error")
    })
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