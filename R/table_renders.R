output$descriptive_summary <- renderDT(
  tryCatch({
    if(ds.class(paste0("table1$", input$d_statistics_variable_selector_value), datasources = connection$conns) == "factor") {
      taula <- ds.table1D(paste0("table1$", input$d_statistics_variable_selector_value), datasources = connection$conns)$counts
      data.table(Values = rownames(taula), Count = taula[,1])
    }
    else {
      Quantiles <- ds.quantileMean(paste0("table1$", input$d_statistics_variable_selector_value), datasources = connection$conns)
      data.table(Quantiles = names(Quantiles), Value = round(Quantiles, digits = 4))
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
  tryCatch({round(glm_results$glm_result_table$coefficients, digits = 4)}, error = function(w){NULL}), 
  options=list(paging = FALSE, searching = FALSE)
)

output$glmer_results_table <- renderDT({
  glmer_table <- eval(str2expression(paste0("glm_results$glmer_result_table$output.summary$", input$glmer_table_server, "$coefficients")))
  tryCatch({round(glmer_table, digits = 4)}, error = function(w){NULL})
}
  , 
  options=list(paging = FALSE, searching = FALSE)
)

output$limma_results_table <- renderDT({
  exp <- paste0("res <- rbind(", paste0("as.data.table(limma_results$result_table$", unique(lists$available_tables$server), ")", 
                                 collapse = ","), ")")
  eval(str2expression(exp))
  as.data.table(lapply(as.data.table(res), format_num))
},
  options = list(columnDefs = list(list(visible=FALSE, targets=c(0))))
)

output$plink_results_table <- renderDT(
  as.data.table(lapply(as.data.table(plink_results$result_table$server1$results), format_num)),
  options = list(columnDefs = list(list(visible=FALSE, targets=c(0))))
)

# output$vcf_ct_counts <- renderDT(
#   tryCatch({
#     if(length(ds.asFactor(input.var.name = paste0("covars$", input$vcf_ct_var), datasources = connection$conns)$all.unique.levels) > 6){
#       hideElement("vcf_ct_perc")
#       hideElement("vcf_perc")
#       Quantiles <- ds.summary(paste0("covars$", input$vcf_ct_var), datasources = connection$conns)$server1$`quantiles & mean`
#       data.table(Quantiles = names(Quantiles), Value = Quantiles)
#     }
#     else{
#       showElement("vcf_ct_perc")
#       showElement("vcf_perc")
#       taula = ds.table1D(paste0("covars$", input$vcf_ct_var), datasources = connection$conns)$counts
#       data.table(Values = rownames(taula), Count = taula)
#     }
#     
#     }, error = function(w){}), options=list(columnDefs = list(list(visible=FALSE, targets=c(0))))
# )

# output$vcf_ct_perc <- renderDT({
#   tryCatch({ds.table1D(paste0("covars$", input$vcf_ct_var), datasources = connection$conns)$percentages}, error = function(w){})
# })

output$vcf_results <- renderDT(
  as.data.table(lapply(as.data.table(vcf_results$result_table_gwas$server1), format_num)),
  options = list(columnDefs = list(list(visible=FALSE, targets=c(0))))
)