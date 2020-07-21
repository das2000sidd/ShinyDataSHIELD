output$d_statistics_scatter_plot <- renderPlot({
  tryCatch({
    browser()
    ds.scatterPlot(x = paste0(input$d_statistics_table_selector_scatter_value, "$", input$d_statistics_variable_selector_scatter_value), 
                   y = paste0(input$d_statistics_table_selector_scatter_value, "$", input$d_statistics_variable_selector_scatter_value2),
                   datasources = connection$conns)
  }, error = function(w){})
  
})

output$gwas_manhattan <- renderPlot(
  manhattan(manhattan_gwas$data, featureCol = manhattan_gwas$featureCol, chrCol = manhattan_gwas$chrCol,
            posCol = manhattan_gwas$posCol, pvalCol = manhattan_gwas$pvalCol)
)