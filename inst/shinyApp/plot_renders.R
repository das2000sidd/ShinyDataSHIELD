output$d_statistics_scatter_plot <- renderPlot({
  tryCatch({
    if(is.null(input$d_statistics_variable_selector_scatter_value_approach)){type <- "combine"} else{
      type <- input$d_statistics_variable_selector_scatter_value_approach
    }
    
    plots$ds_scatter_plot <- function(){
      ds.scatterPlot(x = paste0("tables_descriptive$", input$d_statistics_variable_selector_scatter_value), 
                     y = paste0("tables_descriptive$", input$d_statistics_variable_selector_scatter_value2),
                     type = type,
                     datasources = connection$conns[
                       as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
                     ])
    }
    plots$ds_scatter_plot()
  }, error = function(w){})
})

output$d_statistics_histogram_plot <- renderPlot({
  tryCatch({
    if(is.null(input$d_statistics_variable_selector_histogram_value_approach)){type <- "combine"} else{
      type <- input$d_statistics_variable_selector_histogram_value_approach
    }
    
    plots$ds_histogram_plot <- function(){
      ds.histogram(x = paste0("tables_descriptive$", input$d_statistics_variable_selector_histogram_value),
                   type = type,
                   datasources = connection$conns[
                     as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
                   ])
    }
    plots$ds_histogram_plot()
  }, error = function(w){})
})

output$d_statistics_heatmap_plot <- renderPlot({
  tryCatch({
    if(is.null(input$d_statistics_variable_selector_heatmap_value_approach)){type <- "combine"} else{
      type <- input$d_statistics_variable_selector_heatmap_value_approach
    }
    
    plots$ds_heatmap_plot <- function(){
      ds.heatmapPlot(x = paste0("tables_descriptive$", input$d_statistics_variable_selector_heatmap_value),
                     y = paste0("tables_descriptive$", input$d_statistics_variable_selector_heatmap_value2),
                     type = type,
                     datasources = connection$conns[
                       as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
                     ])
    }
    plots$ds_heatmap_plot()
  }, error = function(w){})
})

output$d_statistics_boxplot_plot <- renderPlot({
  tryCatch({
    # browser()
    if(is.null(input$d_statistics_variable_selector_boxplot_value_approach)){type <- "pooled"} else{
      type <- input$d_statistics_variable_selector_boxplot_value_approach
    }
    ds.boxPlot(x = "tables_descriptive", 
               variables  = input$d_statistics_variable_selector_boxplot_value,
               group = input$d_statistics_variable_selector_boxplot_value2, 
               group2 = input$d_statistics_variable_selector_boxplot_value3,
               type = type,
               datasources = connection$conns[
                 as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
               ])
  }, error = function(w){})
})

output$manhattan <- renderCachedPlot({
  data <- do.call("rbind", vcf_results$result_table_gwas)
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
  data <- plink_results$result_table[[1]]$results
  featureCol <- 2
  chrCol <- 1
  posCol <- 3
  pvalCol <- 9
  plots$genomics_manhattan_plink_plot <- manhattan(data, featureCol = featureCol, chrCol = chrCol,
                                                    posCol = posCol, pvalCol = pvalCol)
  plots$genomics_manhattan_plink_plot
},
cacheKeyExpr = plink_results$result_table[[1]]$results
)

