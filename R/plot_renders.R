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

output$manhattan <- renderPlot({
  data <- vcf_results$result_table_gwas$server1
  featureCol <- 2
  chrCol <- 3
  posCol <- 4
  pvalCol <- 11

  manhattan(data, featureCol = featureCol, chrCol = chrCol,
            posCol = posCol, pvalCol = pvalCol)
})

output$manhattan2 <- renderPlot({
  data <- plink_results$result_table$server1$results
  featureCol <- 2
  chrCol <- 1
  posCol <- 3
  pvalCol <- 9
  
  manhattan(data, featureCol = featureCol, chrCol = chrCol,
            posCol = posCol, pvalCol = pvalCol)
})

