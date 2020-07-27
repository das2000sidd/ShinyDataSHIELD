output$d_statistics_variable_selector <- renderUI({
  selectInput("d_statistics_variable_selector_value", "Select variable", lists$table_columns)
})

output$d_statistics_table_selector <- renderUI({
  selectInput("d_statistics_table_selector_value", "Select table", lists$available_tables$table)
})

output$d_statistics_variable_selector_scatter <- renderUI({
  selectInput("d_statistics_variable_selector_scatter_value", "Select variable", lists$table_columns)
})
output$d_statistics_variable_selector_scatter2 <- renderUI({
  selectInput("d_statistics_variable_selector_scatter_value2", "Select variable", lists$table_columns)
})

output$d_statistics_variable_selector_histogram <- renderUI({
  selectInput("d_statistics_variable_selector_histogram_value", "Select variable", lists$table_columns)
})

output$d_statistics_variable_selector_heatmap <- renderUI({
  selectInput("d_statistics_variable_selector_heatmap_value", "Select variable", lists$table_columns)
})
output$d_statistics_variable_selector_heatmap2 <- renderUI({
  selectInput("d_statistics_variable_selector_heatmap_value2", "Select variable", lists$table_columns)
})

observe({
  if(input$tabs == "d_statistics") {
    if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
    else if (any(unique(lists$available_tables$type_resource) == c("table", "r_obj_eset", "r_obj_rse"))) {
      # datashield.assign.expr(connection$conns, "table1", quote(resource1))
      
    }
    else {
      shinyalert("Oops!", "Descriptive analysis only available for tables, RSE or eSets", type = "error")
    }
    # else {
    #   
    # }
  }
})