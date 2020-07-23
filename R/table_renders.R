output$descriptive_summary <- renderDT(
  tryCatch({
    if(ds.class(paste0(lists$available_tables[table == input$d_statistics_table_selector_value]$table_internal, "$", 
                       input$d_statistics_variable_selector_value), datasources = connection$conns) == "factor") {
      ## DS.TABLE1D OUTPUTS STATISTICS FOR THE STUDIES COMBINED, FIND HOW TO SELECT BETWEEN STUDIES!
      taula = ds.table1D(paste0(lists$available_tables[table == input$d_statistics_table_selector_value]$table_internal, "$", 
                                input$d_statistics_variable_selector_value), 
                         datasources = connection$conns)$counts
      data.table(Values = rownames(taula), Count = taula)
    }
    else {
      Quantiles <- ds.summary(paste0(lists$available_tables[table == input$d_statistics_table_selector_value]$table_internal, "$", 
                                     input$d_statistics_variable_selector_value), 
                              datasources = connection$conns)
      Quantiles <- eval(str2expression(paste0("Quantiles$", 
                                              lists$available_tables[table == input$d_statistics_table_selector_value]$server,
                                              "$`quantiles & mean`")))
      data.table(Quantiles = names(Quantiles), Value = Quantiles)
    }
  }, error = function(w){}), options=list(columnDefs = list(list(visible=FALSE, targets=c(0))))
)

output$server_resources_table <- renderDT(
  connection$server_resources, options=list(columnDefs = list(list(visible=FALSE, targets=c(0, 1, 4))),
                                            paging = FALSE, searching = FALSE)
)

output$available_variables_type <- renderDT(
  lists$table_columns_types, options=list(columnDefs = list(list(visible=FALSE, targets=c(0))),
                                          paging = FALSE, searching = FALSE)
)

output$available_variables_type2 <- renderDT(
  lists$table_columns_types, options=list(columnDefs = list(list(visible=FALSE, targets=c(0))),
                                          paging = FALSE, searching = FALSE)
)

output$glm_results_table <- renderDT(
  glm_results$glm_result_table$coefficients, options=list(paging = FALSE, searching = FALSE,
                                                          rowCallback = JS(
                                                            "function(row, data) {",
                                                            "for (i = 1; i < data.length; i++) {",
                                                            "if (data[i]>1 | data[i]<1){",
                                                            "$('td:eq('+i+')', row).html(data[i].toExponential(1));",
                                                            "}",
                                                            "}",
                                                            "}"))
)

output$glmer_results_table <- renderDT(
  glm_results$glmer_result_table$SLMA.pooled.ests.matrix, options=list(paging = FALSE, searching = FALSE,
                                                          rowCallback = JS(
                                                            "function(row, data) {",
                                                            "for (i = 1; i < data.length; i++) {",
                                                            "if (data[i]>1 | data[i]<1){",
                                                            "$('td:eq('+i+')', row).html(data[i].toExponential(1));",
                                                            "}",
                                                            "}",
                                                            "}"))
)

output$limma_results_table <- renderDT({
  as.data.table(limma_results$result_table$server1)
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

output$vcf_results <- renderDT(
  as.data.table(vcf_results$result_table_gwas$server1)
)