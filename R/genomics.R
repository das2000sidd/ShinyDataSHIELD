observeEvent(input$run_shell, {
  withProgress(message = "Runnink PLINK shell command", {
    plink.arguments <- input$command
    incProgress(0.4)
    tryCatch({
      plink_results$result_table <- ds.PLINK("resource1", plink.arguments, datasources = connection$conns)
      showElement("plink_show_plain")
    }, error = function(w){
      shinyalert("Oops!", "PLINK command yielded errors", type = "error")
    })
  })
})

output$plink_results_terminal_render <- renderText({
  paste0(plink_results$result_table$server1$plink.out$output, collapse = " \n ")
})

observeEvent(input$gwas_trigger, {
  resources_match <- TRUE
  tryCatch({
    ds.GenotypeData(x=lists$available_tables[type_resource == "r_obj_vcf", resource_internal], 
                    covars = lists$available_tables[type_resource == "table", resource_internal], 
                    columnId = 1, newobj.name = 'gds.Data', datasources = connection$conns)
  }, error = function(w){
    shinyalert("Oops!", "Different individuals between vcf and covar files", type = "error")
    resources_match <- FALSE
  })
  if(resources_match){
    withProgress(message = "Performing GWAS", {
      model <- paste0(input$vcf_var, "~", if(is.null(input$vcf_cov)){1} else{paste0(input$vcf_cov, collapse = "+")})
      vcf_results$result_table_gwas <- ds.GWAS(genoData = 'gds.Data', model = as.formula(model), datasources = connection$conns)
    })
  }
})

observe({
  if(input$tabs == "plink"){
    if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
    else if (!(any(c("SshResourceClient") %in% unlist(ds.class("resource1", connection$conns))))){
      shinyalert("Oops!", "Selected resource(s) is not a PLINK resource ", type = "error")
    }
  }
  if(input$tabs == "vcf_files"){
    if (!connection$active) {shinyalert("Oops!", "Not connected", type = "error")}
    else if (!(any("GdsGenotypeReader" %in% 
                   unlist(lapply(unique(lists$available_tables$resource_internal), function(x) ds.class(x, connection$conns)))))){
      shinyalert("Oops!", "Selected resource(s) is not an GdsGenotypeReader or GWASTools.", type = "error")
    }
    else if (length(unique(lists$available_tables$server)) > 1) {
      shinyalert("Oops!", "VCF GWAS currently only implemented for 1 study at a time", type = "error")
    }
    else{
      # get colnames
      lists$vcf_covars <- ds.colnames(lists$available_tables[type_resource == "table", resource_internal], 
                                      datasources = connection$conns)
      # render lists to select
      output$vcf_selector_var <- renderUI({
        selectInput("vcf_var", "Variable", lists$vcf_covars$server1)
      })
      output$vcf_selector_cov <- renderUI({
        selectInput("vcf_cov", "Covariable", lists$vcf_covars$server1[!(lists$vcf_covars$server1 %in% input$vcf_var)], multiple = TRUE)
      })
    }
  }
})