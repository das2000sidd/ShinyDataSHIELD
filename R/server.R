server <- function(input, output, session) {
  # source("R/table_renders.R", local = TRUE)
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
    builder <- newDSLoginBuilder()
    builder$append(server = "study1", url = input$url_l,
                   user = input$user_l, password = input$password_l,
                   resource = input$resource_l, driver = "OpalDriver")
    logindata <- builder$build()
    connection$conns <- datashield.login(logins = logindata, assign = TRUE,
                                         symbol = "res")
    
    datashield.assign.expr(connection$conns, symbol = "methy", 
                           expr = quote(as.resource.object(res)))
    
    lists$limma_variables <- ds.varLabels("methy", datasources = connection$conns)
    lists$limma_labels <- ds.fvarLabels("methy", datasources = connection$conns)
  })
  
  observeEvent(input$run_shell, {
    browser()
    ans <- ds.PLINK("client", input$command)
    browser()
  })
  
  observeEvent(input$run_limma, {
    limma_formula <- as.formula(paste0("~ ", paste0(input$limma_var, collapse = " + ")))
    
    limma_results$result_table <- ds.limma(model = limma_formula,
                                           Set = "methy", 
                                           datasources = connection$conns,
                                           sva = input$limma_sva,
                                           annotCols = input$limma_lab)

    datashield.logout(connection$conns)
  })
  
  output$limma_variables_selector <- renderUI({
    selectInput("limma_var", "limma variables", lists$limma_variables, multiple = TRUE)
  })
  
  output$limma_labels_selector <- renderUI({
    selectInput("limma_lab", "limma labels", lists$limma_labels, multiple = TRUE)
  })
  
  output$limma_results_table <- renderDT({
    as.data.table(limma_results$result_table)
  })
}


