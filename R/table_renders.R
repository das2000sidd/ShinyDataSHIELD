output$server_resources_table <- renderDT(
  connection$server_resources, options=list(columnDefs = list(list(visible=FALSE, targets=c(0, 1, 4))))
)

output$limma_results_table <- renderDT({
  as.data.table(limma_results$result_table)
})

output$plink_results_table <- renderDT({
  as.data.table(plink_results$result_table$server1$results)
})

output$vcf_ct_counts <- renderDT(
  tryCatch({
    if(length(ds.asFactor(input.var.name = paste0("covars$", input$vcf_ct_var), datasources = connection$conns)$all.unique.levels) > 6){
      hideElement("vcf_ct_perc")
      hideElement("vcf_perc")
      Quantiles <- ds.summary(paste0("covars$", input$vcf_ct_var), datasources = connection$conns)$server1$`quantiles & mean`
      data.table(Quantiles = names(Quantiles), Value = Quantiles)
    }
    else{
      showElement("vcf_ct_perc")
      showElement("vcf_perc")
      taula = ds.table1D(paste0("covars$", input$vcf_ct_var), datasources = connection$conns)$counts
      data.table(Values = rownames(taula), Count = taula)
    }
    
    }, error = function(w){}), options=list(columnDefs = list(list(visible=FALSE, targets=c(0))))
)

output$vcf_ct_perc <- renderDT({
  tryCatch({ds.table1D(paste0("covars$", input$vcf_ct_var), datasources = connection$conns)$percentages}, error = function(w){})
})

output$vcf_results <- renderDT({
  as.data.table(vcf_results$result_table_gwas$server1)
})