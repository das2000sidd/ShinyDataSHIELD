output$descriptive_summary_download <- downloadHandler(
  filename = "descriptive_summary.csv",
  content = function(file) {
    write.csv(
      tryCatch({
        if(ds.class(paste0("table1$", input$d_statistics_variable_selector_value), datasources = connection$conns) == "factor") {
          taula <- ds.table1D(paste0("table1$", input$d_statistics_variable_selector_value), datasources = connection$conns)$counts
          data.table(Values = rownames(taula), Count = taula[,1])
        }
        else {
          Quantiles <- ds.quantileMean(paste0("table1$", input$d_statistics_variable_selector_value), datasources = connection$conns)
          data.table(Quantiles = names(Quantiles), Value = round(Quantiles, digits = 4))
        }
      }, error = function(w){})
      , file, row.names = FALSE)
  }
)

output$glm_results_table_download <- downloadHandler(
  filename = "glm_results_table.csv",
  content = function(file) {
    write.csv(glm_results$glm_result_table$coefficients, 
              file)
  }
)

output$glmer_results_table_download <- downloadHandler(
  filename = "glmer_results_table.csv",
  content = function(file) {
    write.csv(eval(str2expression(paste0("glm_results$glmer_result_table$output.summary$", input$glmer_table_server, "$coefficients"))), 
              file)
  }
)

output$plink_results_table_download <- downloadHandler(
  filename = "plink_results_table.csv",
  content = function(file) {
    write.csv(plink_results$result_table$server1$results, 
              file, row.names = FALSE)
  }
)

output$vcf_results_table_download <- downloadHandler(
  filename = "vcf_results_table.csv",
  content = function(file) {
    write.csv(vcf_results$result_table_gwas$server1, 
              file, row.names = FALSE)
  }
)

output$limma_results_table_download <- downloadHandler(
  filename = "limma_results_table.csv",
  content = function(file) {
    write.csv({
      exp <- paste0("res <- rbind(", paste0("as.data.table(limma_results$result_table$", unique(lists$available_tables$server), ")", 
                                            collapse = ","), ")")
      eval(str2expression(exp))
      res
    },file, row.names = FALSE)
  }
)

output$d_statistics_scatter_plot_download <- downloadHandler(
  filename = "d_statistics_scatter_plot.png",
  content = function(file) {
    png(file = file)
    plots$ds_scatter_plot()
    dev.off()
  }
)

output$d_statistics_histogram_plot_download <- downloadHandler(
  filename = "d_statistics_histogram_plot.png",
  content = function(file) {
    png(file = file)
    plots$ds_histogram_plot()
    dev.off()
  }
)

output$d_statistics_heatmap_plot_download <- downloadHandler(
  filename = "d_statistics_heatmap_plot.png",
  content = function(file) {
    png(file = file)
    plots$ds_heatmap_plot()
    dev.off()
  }
)

output$genomics_manhattan_plink_plot_download <- downloadHandler(
  filename = "genomics_manhattan_plink_plot.png",
  content = function(file) {
    ggsave(file, plot = last_plot())
  }
)

output$genomics_manhattan_vcf_plot_download <- downloadHandler(
  filename = "genomics_manhattan_vcf_plot.png",
  content = function(file) {
    ggsave(file, plot = last_plot())
  }
)