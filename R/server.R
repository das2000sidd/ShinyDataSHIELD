server <- function(input, output, session) {
  source("table_renders.R", local = TRUE)
  source("plot_renders.R", local = TRUE)
  connection <- reactiveValues(num_servers = 0, builder = NULL, logindat = NULL, conns = NULL, active = FALSE, complete = FALSE, opal_conection = FALSE, server_resource = list(), server_resources = NULL, isTable = NULL)
  lists <- reactiveValues(resource_variables = NULL, limma_labels = NULL, projects = NULL, resources = NULL, vcf_covars = NULL, table_columns = NULL, available_tables = NULL, available_resources = NULL, table_columns_types = NULL)
  glm_results <- reactiveValues(glm_result_table = NULL, glmer_result_table = NULL)
  limma_results <- reactiveValues(result_table = NULL)
  plink_results <- reactiveValues(result_table = NULL)
  vcf_results <- reactiveValues(result_table_gwas = NULL)

  source("connection.R", local = TRUE)
  source("descriptive_stats.R", local = TRUE)
  source("statistics_models.R", local = TRUE)
  source("genomics.R", local = TRUE)
  source("omics.R", local = TRUE)
  
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
  
  observeEvent(input$stop, {
    browser()
  })
}