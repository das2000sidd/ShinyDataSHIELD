server <- function(input, output, session) {
  source("table_renders.R", local = TRUE)
  connection <- reactiveValues(builder = NULL, logindat = NULL, conns = NULL)
  lists <- reactiveValues(limma_variables = NULL, limma_labels = NULL)
  limma_results <- reactiveValues(result_table = NULL)
  
  observeEvent(input$connect, {
    connection$builder <- newDSLoginBuilder()
    connection$builder$append(server = "study1", url = input$url,
                   user = input$user, password = input$password,
                   resource = input$resource, driver = "OpalDriver")
    connection$logindata <- connection$builder$build()
    connection$conns <- datashield.login(logins = connection$logindata, assign = TRUE,
                              symbol = "client")
  })
  
  observeEvent(input$connect_limma, {
    tryCatch({
      withProgress(message = "Connecting to the resource", {
        builder <- newDSLoginBuilder()
        builder$append(server = "study1", url = input$url_l,
                       user = input$user_l, password = input$password_l,
                       resource = input$resource_l, driver = "OpalDriver")
        logindata <- builder$build()
        incProgress(0.2)
        connection$conns <- datashield.login(logins = logindata, assign = TRUE,
                                             symbol = "limma_connect")
        
        datashield.assign.expr(connection$conns, symbol = "limma_resource", 
                               expr = quote(as.resource.object(limma_connect)))
        incProgress(0.4)
        lists$limma_variables <- ds.varLabels("limma_resource", datasources = connection$conns)
        lists$limma_labels <- ds.fvarLabels("limma_resource", datasources = connection$conns)
        incProgress(0.6)
        output$limma_variables_selector <- renderUI({
          add_busy_spinner(spin = "fading-circle")
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
    }, error = function(w){
      shinyalert("Oops!", "Not able to connect", type = "error")
    })
  })
  
  observeEvent(input$run_shell, {
    browser()
    ans <- ds.PLINK("client", input$command)
    browser()
  })
  
  observeEvent(input$run_limma, {
    withProgress(message = "Performing limma model", {
      limma_formula <- as.formula(paste0("~ ", paste0(input$limma_var, collapse = " + ")))
      incProgress(0.4)
      limma_results$result_table <- ds.limma(model = limma_formula,
                                             Set = "limma_resource", 
                                             datasources = connection$conns,
                                             sva = input$limma_sva,
                                             annotCols = input$limma_lab)
      incProgress(0.8)
      datashield.logout(connection$conns)
    })
  })
}