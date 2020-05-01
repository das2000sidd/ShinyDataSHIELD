server <- function(input, output, session) {
  observeEvent(input$connect, {
    builder <- newDSLoginBuilder()
    builder$append(server = "study1", url = input$url,
                   user = input$user, password = input$password,
                   resource = input$resource, driver = "OpalDriver")
    logindata <- builder$build()
    conns <- datashield.login(logins = logindata, assign = TRUE,
                              symbol = "client")
  })
  observeEvent(input$run_shell, {
    ans <- ds.PLINK("client", input$command)
  })
}


