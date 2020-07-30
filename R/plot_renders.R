output$d_statistics_scatter_plot <- renderPlot({
  tryCatch({
    hide("d_statistics_scatter_plot_error")
    plots$ds_scatter_plot <- function(){
      ds.scatterPlot(x = paste0("table1$", input$d_statistics_variable_selector_scatter_value), 
                     y = paste0("table1$", input$d_statistics_variable_selector_scatter_value2),
                     type = input$d_statistics_scatter_type,
                     datasources = connection$conns)
    }
    plots$ds_scatter_plot()
  }, error = function(w){show("d_statistics_scatter_plot_error")})
})

output$d_statistics_histogram_plot <- renderPlot({
  tryCatch({
    hide("d_statistics_histogram_plot_error")
    plots$ds_histogram_plot <- function(){
      ds.histogram(x = paste0("table1$", input$d_statistics_variable_selector_histogram_value),
                   type = input$d_statistics_histogram_type,
                   datasources = connection$conns)
    }
    plots$ds_histogram_plot()
  }, error = function(w){show("d_statistics_histogram_plot_error")})
})

output$d_statistics_heatmap_plot <- renderPlot({
  tryCatch({
    hide("d_statistics_heatmap_plot_error")
    plots$ds_heatmap_plot <- function(){
      ds.heatmapPlot(x = paste0("table1$", input$d_statistics_variable_selector_heatmap_value),
                     y = paste0("table1$", input$d_statistics_variable_selector_heatmap_value2),
                     type = input$d_statistics_heatmap_type,
                     datasources = connection$conns)
    }
    plots$ds_heatmap_plot()
  }, error = function(w){show("d_statistics_heatmap_plot_error")})
})

output$manhattan <- renderCachedPlot({
  data <- vcf_results$result_table_gwas$server1
  featureCol <- 2
  chrCol <- 3
  posCol <- 4
  pvalCol <- 11

  plots$genomics_manhattan_vcf_plot <- manhattan(data, featureCol = featureCol, chrCol = chrCol,
            posCol = posCol, pvalCol = pvalCol)
  plots$genomics_manhattan_vcf_plot
},
cacheKeyExpr = plink_results$result_table$server1$results
)

output$manhattan2 <- renderCachedPlot({
  data <- plink_results$result_table$server1$results
  featureCol <- 2
  chrCol <- 1
  posCol <- 3
  pvalCol <- 9
  plots$genomics_manhattan_plink_plot <- manhattan(data, featureCol = featureCol, chrCol = chrCol,
                                                    posCol = posCol, pvalCol = pvalCol)
  plots$genomics_manhattan_plink_plot
},
cacheKeyExpr = plink_results$result_table$server1$results
)

