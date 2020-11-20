# PLINK

observeEvent(input$select_tables_ssh, {
  if(length(input$available_tables_ssh_render_rows_selected) > 0){
    datashield.rm(connection$conns, "resource_ssh")
    i <- input$available_tables_ssh_render_rows_selected
    datashield.assign.expr(connection$conns[as.numeric(lists$available_tables[type_resource == "ssh"][i,2])],
                           "resource_ssh", as.symbol(
                             as.character(lists$available_tables[type_resource == "ssh"][i,1])
                           ))
    js$enableTab("plink")
    updateTabsetPanel(session, "plink_t",
                      selected = "plink")
  }
})

observeEvent(input$run_shell, {
  withProgress(message = "Running PLINK shell command", {
    plink.arguments <- input$command
    incProgress(0.4)
    tryCatch({
      plink_results$result_table <- ds.PLINK("resource_ssh", plink.arguments, datasources = connection$conns)
      js$enableTab("plink_plot")
      showElement("plink_show_plain")
      showElement("plink_results_table_download")
    }, error = function(w){
      shinyalert("Oops!", "PLINK command yielded errors", type = "error")
      js$disableTab("plink_plot")
      hideElement("plink_show_plain")
      hideElement("plink_results_table_download")
    })
  })
})

output$plink_results_terminal_render <- renderText({
  paste0(plink_results$result_table[[1]]$plink.out$output, collapse = " \n ")
})

# BIOCONDUCTOR

observeEvent(input$select_tables_vcf, {
  if(length(input$available_tables_vcf_render_rows_selected) > 0){
    datashield.rm(connection$conns, "resource_vcf")
    datashield.rm(connection$conns, "resource_vcf_covar")
    # Check that for each server there is a "table" and a "r_obj_vcf" selected
    for(srv in unique(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][input$available_tables_vcf_render_rows_selected,]$server)){
      obj_in_srv <- lists$available_tables[type_resource %in% c("table", "r_obj_vcf") & server == srv]
      if(nrow(obj_in_srv) == 2 & all(obj_in_srv$type_resource %in% c("table", "r_obj_vcf"))){check <- TRUE}
      else{check <- FALSE}
    }
    # Check all covars tables have the same colnames
    same_cols <- all(lapply(input$available_tables_vcf_render_rows_selected, function(i){
      if(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][i,4] != "table"){return(TRUE)}
      res<-all(match(lists$vcf_covars[[as.character(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][i,1])]], 
                     lists$vcf_covars[[as.character(lists$available_tables[type_resource %in% c("table")][1,1])]]))
      if(is.na(res)){FALSE} else{res}
    }))
    if(check & same_cols){
      for(i in input$available_tables_vcf_render_rows_selected){
        if(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][i, 4] == "table"){
          datashield.assign.expr(connection$conns[as.numeric(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][i,2])],
                                 "resource_vcf_covar", as.symbol(
                                   as.character(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][i,1])
                                 ))
        }
        else if(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][i, 4] == "r_obj_vcf"){
          datashield.assign.expr(connection$conns[as.numeric(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][i,2])],
                                 "resource_vcf", as.symbol(
                                   as.character(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][i,1])
                                 ))
        }
      }
      js$enableTab("gwas")
      updateTabsetPanel(session, "vcf_files_t",
                        selected = "gwas")
    }
    else{
      shinyalert("Oops!", if(!check & same_cols){"Select for each server a VCF resource and a covars table"}
                 else if(check & !same_cols){"Covariate columns do not match betwee servers"}
                 else{"Select for each server a VCF resource and a covars table that matches between servers"}, 
                 type = "error")
    }
  }
})

output$vcf_selector_var <- renderUI({
  selectInput("vcf_var", "Variable", lists$vcf_covars[[1]])
})
output$vcf_selector_cov <- renderUI({
  selectInput("vcf_cov", "Covariable", lists$vcf_covars[[1]][!(lists$vcf_covars[[1]] %in% input$vcf_var)], multiple = TRUE)
})

observeEvent(input$gwas_trigger, {
  resources_match <- TRUE
  tryCatch({
      ds.GenotypeData(x="resource_vcf", covars = "resource_vcf_covar", 
                    columnId = 1, newobj.name = 'gds.Data', datasources = connection$conns[
                    unique(as.numeric(unlist(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][input$available_tables_vcf_render_rows_selected, 2])))
                    ])
  }, error = function(w){
    shinyalert("Oops!", "Different individuals between VCF and covar files", type = "error")
    resources_match <- FALSE
  })
  if(resources_match){
    withProgress(message = "Performing GWAS", {
      model <- paste0(input$vcf_var, "~", if(is.null(input$vcf_cov)){1} else{paste0(input$vcf_cov, collapse = "+")})
      tryCatch({
        vcf_results$result_table_gwas <- ds.GWAS(genoData = 'gds.Data', model = as.formula(model), datasources = connection$conns[
          unique(as.numeric(unlist(lists$available_tables[type_resource %in% c("table", "r_obj_vcf")][input$available_tables_vcf_render_rows_selected, 2])))
        ])
        js$enableTab("gwas_plot")
        showElement("vcf_results_table_download")
      }, error = function(w){
        shinyalert("Oops!", "Error when performing GWAS", type = "error")
        hideElement("vcf_results_table_download")
      })
    })
  }
})

observe({
  if(input$tabs == "plink"){
    output$available_tables_ssh <- renderUI({
      dataTableOutput("available_tables_ssh_render")
    })
  }
  if(input$tabs == "vcf_files"){
    output$available_tables_vcf <- renderUI({
      dataTableOutput("available_tables_vcf_render")
    })
    
    # if(!(any("GdsGenotypeReader" %in% 
    #                unlist(lapply(unique(lists$available_tables$resource_internal), function(x) ds.class(x, connection$conns)))))){
    #   shinyalert("Oops!", "Selected resource(s) is not an GdsGenotypeReader or GWASTools.", type = "error")
    #   updateTabItems(session, "tabs", "server_connect")
    # }
    # else if (length(unique(lists$available_tables$server)) > 1) {
    #   shinyalert("Oops!", "VCF GWAS currently only implemented for 1 study at a time", type = "error")
    #   updateTabItems(session, "tabs", "server_connect")
    # }
    # else{
    #   # get colnames
    
    # Get column names from available tables
    tables_available <- lists$available_tables[type_resource %in% c("table")]
    if(length(lists$vcf_covars) == 0){
      withProgress(message = "Reading column names from available tables", value = 0, {
        for(i in 1:nrow(tables_available)){
          lists$vcf_covars[[as.character(tables_available[i,1])]] <- ds.colnames(as.character(tables_available[i,1]), datasources = connection$conns[as.numeric(tables_available[i,2])])[[1]]
          incProgress(i/nrow(tables_available))
        }
      })
    }
  }
})