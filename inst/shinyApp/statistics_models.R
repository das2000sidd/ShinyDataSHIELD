observeEvent(input$select_tables_sm, {
  if(length(input$available_tables_sm_render_rows_selected) > 0){
    same_cols <- TRUE
    different_study_server <- TRUE
    if(length(input$available_tables_sm_render_rows_selected) > 1){
      same_cols <- all(lapply(input$available_tables_sm_render_rows_selected, function(i){
        res<-all(match(lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][i,1])]], 
                       lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_sm_render_rows_selected[1],1])]]))
        if(is.na(res)){FALSE} else{res}
      }))
      different_study_server <- nrow(unique(lists$available_tables[input$available_tables_sm_render_rows_selected,3])) ==
        length(input$available_tables_sm_render_rows_selected)
      }
    if(!same_cols | !different_study_server){
      shinyalert("Oops!",
                 if(!same_cols){
                   "Selected tables do not share the same columns, can't pool unequal tables."
                 }else{
                   "Selected tables are not on different study servers, can't pool tables on the same study server."
                 }
                 , type = "error")
      js$disableTab("glm")
      js$disableTab("mixed_model")
      updateTabsetPanel(session, "statistic_models_t",
                        selected = "a_tables_sm")
    }
    else{
      datashield.rm(connection$conns, "tables_sm")
      for(i in input$available_tables_sm_render_rows_selected){
        lists$available_tables[type_resource == "table"][i,2]
        
        datashield.assign.expr(connection$conns[as.numeric(lists$available_tables[type_resource == "table"][i,2])],
                               "tables_sm", as.symbol(
                                 as.character(lists$available_tables[type_resource == "table"][i,1])
                               ))
      }
      withProgress(message = "Getting the column types for selected tables", value = 0, {
        lists$table_columns_types <- NULL
        for(var in lists$table_columns[[1]]){
          type <- ds.class(paste0("tables_sm$", var), 
                           connection$conns[as.numeric(lists$available_tables[input$available_tables_sm_render_rows_selected,2][1])])[[1]]
          lists$table_columns_types <- cbind(lists$table_columns_types, rbind(var, paste(type, collapse = ", ")))
          incProgress(1/length(lists$table_columns[[1]]))
        }
      })
      lists$table_columns_types <- as.data.table(t(lists$table_columns_types))
      colnames(lists$table_columns_types) <- c("variable", "type")
      js$enableTab("glm")
      js$enableTab("mixed_model")
      if(length(input$available_tables_sm_render_rows_selected)>1){
        showElement("glm_approach")
      }
      else{
        hideElement("glm_approach")
      }
      updateTabsetPanel(session, "statistic_models_t",
                        selected = "glm")
    }
  }
})

observeEvent(input$gml_toggle_variables_table, {
  toggleElement("available_variables_type")
})

observeEvent(input$perform_glm, {
  approach <- input$glm_approach
  if(is.null(approach)){approach <- "Pooled"}
  if(approach == "Pooled"){
    tryCatch({
      withProgress(message = "Performing GLM", value = 0.5, {
        glm_results$glm_result_table <- ds.glm(formula = as.formula(input$glm_formula), data = "tables_sm", family = input$gml_output_family,
                                               datasources = connection$conns[
                                                 as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_sm_render_rows_selected, 2]))
                                               ])
      })
      showElement("glm_results_table_download")
    }, error = function(w){
      shinyalert("Oops!", "Check whether the variables are properly written and/or your dependent variable fits the output family", type = "error")
      hideElement("glm_results_table_download")
    })
  }
  else{
    tryCatch({
      withProgress(message = "Performing GLM", value = 0.5, {
        glm_results$glm_result_table <- ds.glmSLMA(formula = as.formula(input$glm_formula), data = "tables_sm", family = input$gml_output_family,
                                               datasources = connection$conns[
                                                 as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_sm_render_rows_selected, 2]))
                                               ])
      })
      showElement("glm_results_table_download")
    }, error = function(w){
      shinyalert("Oops!", "Check whether the variables are properly written and/or your dependent variable fits the output family", type = "error")
      hideElement("glm_results_table_download")
    })
  }
})

observeEvent(input$trigger_formula_help_glm, {
  shinyalert("Formula structure", "y~a+b+c+d
    Means fit a GLM with y as the outcome variable and a, b, c and d as covariates. By default all models include an intercept (regression constant) term, to exclude it use:
    y~0+a+b+c+d
    The * symbol between two covariates means fit all possible main effects and interactions for and between those two covariates, as example:
    y~a*b", type = "info")
})

observeEvent(input$gmler_toggle_variables_table, {
  toggleElement("available_variables_type2")
})

observeEvent(input$perform_glmer, {
  tryCatch({
    withProgress(message = "Performing GLMer", value = 0.5, {
      browser()
      glm_results$glmer_result_table <- ds.glmerSLMA(formula = as.formula(input$glmer_formula), data = "tables_sm", family = input$gmler_output_family,
                                                     datasources = connection$conns[
                                                       as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_sm_render_rows_selected, 2]))
                                                     ])
    })
    output$glmer_results_select <- renderUI({
      selectInput("glmer_results_select_value", "Select results to display", names(glm_results$glmer_result_table$output.summary))
    })
    showElement("glmer_results_table_download")
    if(length(lists$available_tables$server) > 1) {
      showElement("glmer_table_server")
    }
  }, error = function(w){
    shinyalert("Oops!", "Error performing the GLMer", type = "error")
    hideElement("glmer_results_table_download")
    hideElement("glmer_table_server")
  })
  
})

output$glmer_server_select <- renderUI({
  hidden(selectInput("glmer_table_server", "Select study server", str_replace(lists$available_tables$server, "server", "study")))
})

observeEvent(input$trigger_formula_help_glmer, {
  shinyalert("Formula structure", "y~a+b+(1|c)
    Means fit an GLME with y as the outcome variable (e.g. a binary case-control using a logistic regression model or a count or a survival time using a Poisson regression model), a and b as fixed effects, and c as a random effect or grouping factor.
    It is also possible to fit models with random slopes by specifying a model such as
    y~a+b+(1+b|c)
    where the effect of b can vary randomly between groups defined by c. Implicit nesting can be specified with formulas such as: 
    y~a+b+(1|c/d) or y~a+b+(1|c)+(1|c:d)", type = "info")
})

observe({
  if(input$tabs == "statistic_models") {
    # Get column names from available tables
    tables_available <- lists$available_tables[type_resource == "table"]
    if(length(lists$tables_columns) == 0){
      withProgress(message = "Reading column names from available tables", value = 0, {
        for(i in 1:nrow(tables_available)){
          lists$table_columns[[as.character(tables_available[i,1])]] <- ds.colnames(as.character(tables_available[i,1]), datasources = connection$conns[as.numeric(tables_available[i,2])])[[1]]
          incProgress(i/nrow(tables_available))
        }
      })
    }
    output$available_tables_sm <- renderUI({
      dataTableOutput("available_tables_sm_render")
    })
  }
})