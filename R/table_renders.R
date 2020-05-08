output$limma_results_table <- renderDT({
  as.data.table(limma_results$result_table)
})