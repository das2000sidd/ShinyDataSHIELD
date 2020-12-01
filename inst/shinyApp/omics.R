observeEvent(input$select_tables_lim, {
  if(length(input$available_tables_lim_render_rows_selected) > 0){
    different_study_server <- TRUE
    same_cols_1 <- TRUE
    same_cols_2 <- TRUE
    if(length(input$available_tables_lim_render_rows_selected) > 1){
      same_cols_1 <- all(lapply(input$available_tables_lim_render_rows_selected, function(i){
        res<-all(match(lists$resource_variables[[as.character(lists$available_tables[type_resource %in% c("r_obj_rse", "r_obj_eset")][i,1])]], 
                       lists$resource_variables[[as.character(lists$available_tables[type_resource %in% c("r_obj_rse", "r_obj_eset")][1,1])]]))
        if(is.na(res)){FALSE} else{res}
      }))
      
      same_cols_2 <- all(lapply(input$available_tables_lim_render_rows_selected, function(i){
        res<-all(match(lists$limma_labels[[as.character(lists$available_tables[type_resource %in% c("r_obj_rse", "r_obj_eset")][i,1])]], 
                       lists$limma_labels[[as.character(lists$available_tables[type_resource %in% c("r_obj_rse", "r_obj_eset")][1,1])]]))
        if(is.na(res)){FALSE} else{res}
      }))
      different_study_server <- nrow(unique(lists$available_tables[input$available_tables_lim_render_rows_selected,3])) ==
        length(input$available_tables_lim_render_rows_selected) 
    }

    if(same_cols_1 & same_cols_2 & different_study_server){
      datashield.rm(connection$conns, "resource_lim")
      for(i in input$available_tables_lim_render_rows_selected){
        lists$available_tables[type_resource %in% c("r_obj_rse", "r_obj_eset")][i,2]
        
        datashield.assign.expr(connection$conns[as.numeric(lists$available_tables[type_resource %in% c("r_obj_rse", "r_obj_eset")][i,2])],
                               "resource_lim", as.symbol(
                                 as.character(lists$available_tables[type_resource %in% c("r_obj_rse", "r_obj_eset")][i,1])
                               ))
      }
      js$enableTab("limma")
      updateTabsetPanel(session, "limma_t",
                        selected = "limma")
    }
    else{
      shinyalert("Oops!", 
                 if(!same_cols_1 | !same_cols_2){
                   "Selected resources do not share the same columns, can't pool unequal resources"
                 }else{
                   "Selected resources are not on different study servers, can't pool resources on the same study server."
                 }
                 , type = "error")
      js$disableTab("limma")
      updateTabsetPanel(session, "limma_t",
                        selected = "limma_a")
    }
  }
})

output$limma_variables_selector_condition <- renderUI({
  selectInput("limma_var_feature", "Condition for the limma", c("", lists$resource_variables))
})
output$limma_variables_selector_covars <- renderUI({
  selectInput("limma_var_covars", "Covariables for the limma", 
              lists$resource_variables[lists$resource_variables != input$limma_var_feature], multiple = TRUE)
})
output$limma_labels_selector <- renderUI({
  selectInput("limma_lab", "Labels for the limma", lists$limma_labels, multiple = TRUE)
})
output$limma_sva_selector <- renderUI({
  checkboxInput("limma_sva", "SVA", value = FALSE)
})
output$limma_run <- renderUI({
  actionButton("run_limma", "Run limma")
})

observeEvent(input$run_limma, {
  withProgress(message = "Performing limma model", {
    tryCatch({
      limma_formula <- as.formula(paste0(input$limma_var_feature, " ~ ", paste0(input$limma_var_covars, collapse = " + ")))
      incProgress(0.4)
      limma_results$result_table <- ds.limma(model = limma_formula,
                                             Set = "resource_lim", 
                                             datasources = connection$conns[
                                               unique(as.numeric(unlist(lists$available_tables[type_resource %in% c("r_obj_rse", "r_obj_eset")][input$available_tables_lim_render_rows_selected, 2])))
                                             ],
                                             sva = input$limma_sva,
                                             annotCols = input$limma_lab,
                                             type.data = input$limma_data_type)
      # browser()
      incProgress(0.8)
      showElement("limma_results_table_download")
    }, error = function(w){
      shinyalert("Oops!", "Error when performing the limma", type = "error")
      hideElement("limma_results_table_download")
    })
    
  })
  if (length(connection$server_resource) > 1) {
    output$limma_server_select <- renderUI({
      # EL RESOURCE SELECCIONAT MANE QUINA TAULA ES FA EL RENDER!!!! AL TABLE RENDERS
      # limma_results$result_table + INPUT$LIMMA_SERVER_RESULTS AMB UN PASTE0
      selectInput("limma_server_results", "Resource", unlist(connection$server_resource))
    })
  }
})

observe({
  if(input$tabs == "limma") {
    
    tables_available <- lists$available_tables[type_resource %in% c("r_obj_rse", "r_obj_eset")]
    if(length(lists$resource_variables) == 0){
      withProgress(message = "Reading column names from available tables", value = 0, {
        for(i in 1:nrow(tables_available)){
          lists$resource_variables[[as.character(tables_available[i,1])]] <- ds.varLabels(as.character(tables_available[i,1]), datasources = connection$conns[as.numeric(tables_available[i,2])])[[1]]
          incProgress(i/nrow(tables_available))
        }
      })
    }
    
    if(length(lists$limma_labels) == 0){
      withProgress(message = "Reading column names from available tables", value = 0, {
        for(i in 1:nrow(tables_available)){
          lists$limma_labels[[as.character(tables_available[i,1])]] <- ds.fvarLabels(as.character(tables_available[i,1]), datasources = connection$conns[as.numeric(tables_available[i,2])])[[1]]
          incProgress(i/nrow(tables_available))
        }
      })
    }
    
    output$available_tables_lim <- renderUI({
      dataTableOutput("available_tables_lim_render")
    })
  }
})