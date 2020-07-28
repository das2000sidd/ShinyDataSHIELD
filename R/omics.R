observeEvent(input$run_limma, {
  withProgress(message = "Performing limma model", {
    limma_formula <- as.formula(paste0("~ ", paste0(input$limma_var, collapse = " + ")))
    incProgress(0.4)
    limma_results$result_table <- ds.limma(model = limma_formula,
                                           Set = "resource1", 
                                           datasources = connection$conns,
                                           sva = input$limma_sva,
                                           annotCols = input$limma_lab,
                                           type.data = input$limma_data_type)
    incProgress(0.8)
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
    if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
    else if (!(any(c("ExpressionSet", "RangedSummarizedExperiment") %in% unlist(ds.class("resource1", connection$conns))))){
      shinyalert("Oops!", "Selected resource(s) is not an Expression Set or Range Summarized Experiment. Can't perform LIMMA", type = "error")
    }
    else {
      withProgress(message = "Loading Limma parameters", value = 0, {
        incProgress(0.2)
        # Take variables from the 1st selected dataset. They should be equal
        # lists$resource_variables <- ds.varLabels("resource1", datasources = connection$conns)$server1
        
        ## ds.fvarLabels is crashing for RSE datasets ( ds.varLabels works for eSets tho )
        
        # lists$limma_labels <- ds.fvarLabels("resource1", datasources = connection$conns)$server1
        incProgress(0.6)
        output$limma_variables_selector <- renderUI({
          selectInput("limma_var", "Variables for the limma", lists$resource_variables, multiple = TRUE)
        })
        incProgress(0.8)
        output$limma_labels_selector <- renderUI({
          selectInput("limma_lab", "Labels for the limma", lists$limma_labels, multiple = TRUE)
        })
        output$limma_sva_selector <- renderUI({
          checkboxInput("limma_sva", "SVA", value = FALSE)
        })
        output$limma_run <- renderUI({
          actionButton("run_limma", "Run limma")
        })
      })
    }
  }
})