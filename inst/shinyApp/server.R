server <- function(input, output, session) {
  connection <- reactiveValues(creds = NULL, builder = NULL, logindat = NULL, conns = NULL, active = FALSE, server_resource = list(), server_resources = NULL, isTable = NULL)
  lists <- reactiveValues(tab = NULL, res = NULL, resource_variables = list(), limma_labels = list(), projects_tab = NULL, projects_res = NULL, resources = NULL, vcf_covars = list(), table_columns = list(), available_tables = NULL, available_resources = NULL, table_columns_types = NULL)
  glm_results <- reactiveValues(glm_result_table = NULL, glmer_result_table = NULL)
  limma_results <- reactiveValues(result_table = NULL)
  plink_results <- reactiveValues(result_table = NULL)
  vcf_results <- reactiveValues(result_table_gwas = NULL)
  plots <- reactiveValues(ds_scatter_plot = NULL)
  tabIndex <- reactiveVal(1)
  tables_resources <- reactiveValues()
  max_servers <- 10 # Change this to allow more servers, couldnt manage to create the observeEvents dynamically :(
  
  source("table_renders.R", local = TRUE)
  source("plot_renders.R", local = TRUE)
  source("connection.R", local = TRUE)
  source("descriptive_stats.R", local = TRUE)
  source("statistics_models.R", local = TRUE)
  source("genomics.R", local = TRUE)
  source("omics.R", local = TRUE)
  source("download_handlers.R", local = TRUE)
  source("table_columns.R", local = TRUE)
  
  source("ggeditLiteModule.R", local = TRUE)
  ggeditLiteServer("manhattan_edit", "genomics_manhattan_vcf_plot")
  ggeditLiteServer("d_statistics_boxplot_plot_edit", "ds_boxplot_plot")
  
  js$disableTab("summary")
  js$disableTab("col_tables")
  js$disableTab("s_plot")
  js$disableTab("h_plot")
  js$disableTab("hm_plot")
  js$disableTab("box_plot")
  js$disableTab("glm")
  js$disableTab("mixed_model")
  js$disableTab("plink")
  js$disableTab("plink_plot")
  js$disableTab("gwas")
  js$disableTab("gwas_plot")
  js$disableTab("limma")
  
  
  format_num <- function(col) {
    if (is.numeric(col))
      round(col, digits = 4)# sprintf('%1.2f', col)
    else
      col
  }
  
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
  
  onStop(function() isolate(datashield.logout(connection$conns)))
  
  observeEvent(input$stop, {
    browser()
  })
}