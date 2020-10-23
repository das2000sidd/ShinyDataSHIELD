observeEvent(input$select_tables_sm, {
  if(length(input$available_tables_sm_render_rows_selected) > 0){
    same_cols <- TRUE
    if(length(input$available_tables_sm_render_rows_selected) > 1){
      same_cols <- all(lapply(input$available_tables_sm_render_rows_selected, function(i){
        res<-all(match(lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][i,1])]], 
                       lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_sm_render_rows_selected[1],1])]]))
        if(is.na(res)){FALSE} else{res}
      }))}
    if(!same_cols){
      shinyalert(shinyalert("Oops!", "Selected tables do not share the same columns, can't pool unequal tables.", type = "error"))
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
    }
    js$enableTab("glm")
    js$enableTab("mixed_model")
    updateTabsetPanel(session, "statistic_models_t",
                      selected = "glm")
  }
})

observeEvent(input$gml_toggle_variables_table, {
  toggleElement("available_variables_type")
})

observeEvent(input$perform_glm, {
  browser()
  tryCatch({
    withProgress(message = "Performing GLM", value = 0.5, {
      glm_results$glm_result_table <- ds.glm(formula = as.formula(input$glm_formula), data = "tables_sm", family = input$gml_output_family,
                                             datasources = connection$conns[
                                               as.numeric(unlist(lists$available_tables[type_resource == "table"][input$available_tables_sm_render_rows_selected, 2]))
                                             ])
    })
    showElement("glm_results_table_download")
  }, error = function(w){
    shinyalert("Oops!", "Error performing the GLM", type = "error")
    hideElement("glm_results_table_download")
  })
  
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
      for(i in 1:nrow(tables_available)){
        lists$table_columns[[as.character(tables_available[i,1])]] <- ds.colnames(as.character(tables_available[i,1]), datasources = connection$conns[as.numeric(tables_available[i,2])])[[1]]
      }
    }
    output$available_tables_sm <- renderUI({
      dataTableOutput("available_tables_sm_render")
    })
  }
})