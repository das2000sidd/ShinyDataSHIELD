output$limma_results_table <- renderDT({
  as.data.table(limma_results$result_table)
})

output$plink_results_table <- renderDT({
  as.data.table(plink_results$result_table$server1$results)
})

output$vcf_ct_counts <- renderDT({
  tryCatch({ds.table1D(paste0("covars$", input$vcf_ct_var), datasources = connection$conns)$counts}, error = function(w){})
  
})

output$vcf_ct_perc <- renderDT({
  tryCatch({ds.table1D(paste0("covars$", input$vcf_ct_var), datasources = connection$conns)$percentages}, error = function(w){})
})

output$vcf_results <- renderDT({
  as.data.table(vcf_results$result_table$server1)
})