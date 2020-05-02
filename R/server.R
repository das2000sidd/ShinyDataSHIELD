server <- function(input, output, session) {
  connection <- reactiveValues(conns = NULL)
  
  
  observeEvent(input$connect, {
    builder <- newDSLoginBuilder()
    builder$append(server = "study1", url = input$url,
                   user = input$user, password = input$password,
                   resource = input$resource, driver = "OpalDriver")
    logindata <- builder$build()
    connection$conns <- datashield.login(logins = logindata, assign = TRUE,
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
  })
  
  observeEvent(input$run_shell, {
    browser()
    ans <- ds.PLINK("client", input$command)
    browser()
  })
  
  observeEvent(input$run_limma, {
    datashield.assign.expr(connection$conns, symbol = "methy", 
                           expr = quote(as.resource.object(res)))
    
    ans.limma <- ds.limma(model = ~ diagnosis + Sex,
                          Set = "methy", 
                          datasources = connection$conns)
    
    lapply(ans.limma, head)
  })
}


