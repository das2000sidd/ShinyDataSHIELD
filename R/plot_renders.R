output$d_statistics_scatter_plot <- renderPlot({
  tryCatch({
    ds.scatterPlot(x = paste0("table1$", input$d_statistics_variable_selector_scatter_value), 
                   y = paste0("table1$", input$d_statistics_variable_selector_scatter_value2),
                   type = input$d_statistics_scatter_type,
                   datasources = connection$conns)
  }, error = function(w){})
  
})

output$d_statistics_histogram_plot <- renderPlot({
  tryCatch({
    ds.histogram(x = paste0("table1$", input$d_statistics_variable_selector_histogram_value),
                 type = input$d_statistics_histogram_type,
                 datasources = connection$conns)
  }, error = function(w){})
})

output$d_statistics_heatmap_plot <- renderPlot({
  tryCatch({
    ds.heatmapPlot(x = paste0("table1$", input$d_statistics_variable_selector_heatmap_value),
                   y = paste0("table1$", input$d_statistics_variable_selector_heatmap_value2),
                   type = input$d_statistics_heatmap_type,
                   datasources = connection$conns)
  }, error = function(w){})
})

output$gwas_manhattan <- renderPlot(
  manhattan(manhattan_gwas$data, featureCol = manhattan_gwas$featureCol, chrCol = manhattan_gwas$chrCol,
            posCol = manhattan_gwas$posCol, pvalCol = manhattan_gwas$pvalCol)
)