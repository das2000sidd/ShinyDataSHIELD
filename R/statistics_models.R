observeEvent(input$gml_toggle_variables_table, {
  toggleElement("available_variables_type")
})

observeEvent(input$perform_glm, {
  tryCatch({
    withProgress(message = "Performing GLM", value = 0.5, {
      glm_results$glm_result_table <- ds.glm(formula = as.formula(input$glm_formula), data = "table1", family = input$gml_output_family,
                                             datasources = connection$conns)
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
      glm_results$glmer_result_table <- ds.glmerSLMA(formula = as.formula(input$glmer_formula), data = "table1", family = input$gmler_output_family,
                                                     datasources = connection$conns)
    })
    showElement("glmer_results_table_download")
  }, error = function(w){
    shinyalert("Oops!", "Error performing the GLMer", type = "error")
    hideElement("glmer_results_table_download")
  })
  
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
  if(input$tabs == "statistic_models"){
    if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
    else if (!any(unique(lists$available_tables$type_resource) == c("table"))) {#, "r_obj_eset", "r_obj_rse"))) {
      shinyalert("Oops!", "Statistic models only available for tables", type = "error")
    }
  }
  if(input$tabs == "statistic_models_mixed"){
    if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
    else if (!any(unique(lists$available_tables$type_resource) == c("table"))) {#, "r_obj_eset", "r_obj_rse"))) {
      shinyalert("Oops!", "Mixed statistic models only available for tables", type = "error")
    }
  }
})