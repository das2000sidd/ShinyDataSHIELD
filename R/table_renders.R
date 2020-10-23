output$available_tables_render <- renderDT(
  lists$available_tables[type_resource == "table"], options=list(columnDefs = list(list(visible=FALSE, targets=c(0,2,4))),
                                                                 paging = FALSE, searching = FALSE)
)

output$available_tables_sm_render <- renderDT(
  lists$available_tables[type_resource == "table"], options=list(columnDefs = list(list(visible=FALSE, targets=c(0,2,4))),
                                                                 paging = FALSE, searching = FALSE)
)

output$available_tables_ssh_render <- renderDT(
  lists$available_tables[type_resource == "ssh"], options=list(columnDefs = list(list(visible=FALSE, targets=c(0,2,4))),
                                                                 paging = FALSE, searching = FALSE),
  selection = "single"
)

output$available_tables_vcf_render <- renderDT(
  lists$available_tables[type_resource %in% c("table", "r_obj_vcf")], options=list(columnDefs = list(list(visible=FALSE, targets=c(0,2,4))),
                                                                 paging = FALSE, searching = FALSE)
)

output$available_tables_lim_render <- renderDT(
  lists$available_tables[type_resource %in% c("r_obj_rse", "r_obj_eset")], options=list(columnDefs = list(list(visible=FALSE, targets=c(0,2,4))),
                                                                 paging = FALSE, searching = FALSE)
)

output$descriptive_summary <- renderDT(
  tryCatch({
    if(is.null(input$d_statistics_variable_selector_value_approach)){type <- "combine"} else{
      type <- input$d_statistics_variable_selector_value_approach
    }
    
    if(ds.class(paste0("tables_descriptive$", input$d_statistics_variable_selector_value), datasources = connection$conns[
      as.numeric(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected[1], 2])
    ]) == "factor") {
      taula <- ds.table1D(paste0("tables_descriptive$", input$d_statistics_variable_selector_value), datasources = connection$conns[
        as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
      ], type = type)$counts
      if(type == "combine"){
        table <- taula
        colnames(table) <- "Counts"
      }
      else{
        table <- data.frame(matrix(unlist(taula), nrow=length(taula), byrow=T))
        rownames(table) <- names(taula)
        colnames(table) <- rownames(taula[[1]])
      }
      table
    }
    else {
      Quantiles <- ds.quantileMean(paste0("tables_descriptive$", input$d_statistics_variable_selector_value), datasources = connection$conns[
        as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected, 2]))
      ], type = type)
      table <- data.frame(matrix(unlist(Quantiles), nrow=length(Quantiles), byrow=T))
      rownames(table) <- names(Quantiles)
      colnames(table) <- names(Quantiles[[1]])
      # as.data.table(lapply(as.data.table(table), format_num))
      round(table, digits = 4)
    }
  }, error = function(w){}), options=list(columnDefs = list(list(visible=FALSE, targets=c())))
)

output$server_resources_table <- renderDT(
  connection$server_resources, options=list(columnDefs = list(list(visible=FALSE, targets=c(0))),
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
  if(is.list(glm_results$glmer_result_table$output.summary[[input$glmer_results_select_value]])){
    glmer_table <- eval(str2expression(paste0("glm_results$glmer_result_table$output.summary$", input$glmer_results_select_value, "$coefficients")))
  }
  else{glmer_table <- try(eval(str2expression(paste0("glm_results$glmer_result_table$output.summary$", input$glmer_results_select_value))))}
  
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
  as.data.table(lapply(as.data.table(plink_results$result_table[[1]]$results), format_num)),
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
  tryCatch({as.data.table(lapply(as.data.table(do.call("rbind", vcf_results$result_table_gwas)), format_num))}, 
           error = function(w){NULL}),
  options = list(columnDefs = list(list(visible=FALSE, targets=c(0))))
)