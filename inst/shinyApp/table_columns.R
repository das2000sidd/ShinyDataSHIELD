observeEvent(input$select_tables_cols, {
  if(length(input$available_tables_cols_render_rows_selected) > 0){
    same_cols <- TRUE
    different_study_server <- TRUE
    if(length(input$available_tables_cols_render_rows_selected) > 1){
      same_cols <- all(lapply(input$available_tables_cols_render_rows_selected, function(i){
        res<-all(match(lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][i,1])]], 
                       lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_cols_render_rows_selected[1],1])]]))
        if(is.na(res)){FALSE} else{res}
      }))
      different_study_server <- nrow(unique(lists$available_tables[input$available_tables_cols_render_rows_selected,3])) ==
        length(input$available_tables_cols_render_rows_selected)
    }
    if(!same_cols | !different_study_server){
      shinyalert("Oops!",
                 if(!same_cols){
                   "Selected tables do not share the same columns, can't pool unequal tables."
                 }else{
                   "Selected tables are not on different study servers, can't pool tables on the same study server."
                 }
                 , type = "error")
      js$disableTab("col_tables")
      updateTabsetPanel(session, "table_columns_selection",
                        selected = "a_tables")
    }
    else{
      withProgress(message = "Getting the column types for selected tables", value = 0, {
        lists$table_columns_types <- NULL
        for(var in lists$table_columns[[1]]){
          type <- ds.class(paste0(lists$available_tables[input$available_tables_cols_render_rows_selected,1][1],
                                  "$", var), connection$conns[as.numeric(lists$available_tables[input$available_tables_cols_render_rows_selected,2][1])])[[1]]
          lists$table_columns_types <- cbind(lists$table_columns_types, rbind(var, paste(type, collapse = ", ")))
          incProgress(1/length(lists$table_columns[[1]]))
        }
      })
      lists$table_columns_types <- as.data.table(t(lists$table_columns_types))
      colnames(lists$table_columns_types) <- c("variable", "type")
      js$enableTab("col_tables")
      updateTabsetPanel(session, "table_columns_selection",
                        selected = "col_tables")
    }
  }
})

proxy = dataTableProxy('a')
observeEvent(input$jsValue, {
  change <- data.table(input$jsValue)
  row <- as.numeric(change[1,1]) + 1
  column <- as.numeric(change[2,1])
  new_class <- as.character(change[4,1])
  table_name <- lists$available_tables[input$available_tables_cols_render_rows_selected,1]
  variable <- as.character(lists$table_columns_types[row, 1])
  tables_available <- lists$available_tables[type_resource == "table"]
  tryCatch({
    withProgress(message = "Studying class change feasibility", value = 0, {
      for(index in input$available_tables_cols_render_rows_selected){
        # ds.dataFrame(x = as.character(table_name[index]), stringsAsFactors = FALSE, newobj = as.character(table_name[index]),
        #              datasources = connection$conns[as.numeric(tables_available[index,2])])
        complete_name <- paste(table_name[index], variable, sep = "$")
        incProgress(0.2 / length(input$available_tables_cols_render_rows_selected))
        # Get new variable (with class updated)
        if(new_class == "factor"){
          ds.asFactor(complete_name, variable, datasources = connection$conns[as.numeric(tables_available[index,2])])
        } else if(new_class == "character"){
          ds.asCharacter(complete_name, variable, datasources = connection$conns[as.numeric(tables_available[index,2])])
        } else if(new_class == "numeric"){
          ds.asNumeric(complete_name, variable, datasources = connection$conns[as.numeric(tables_available[index,2])])
        }
        incProgress(0.2 / length(input$available_tables_cols_render_rows_selected))
        # Create auxiliary tables without the column 'variable', one table is the columns on the left, the other the
        # columns on the right
        extreme_left <- FALSE
        extreme_right <- FALSE
        # Left creation
        if(row != 1){
          DSI::datashield.assign.expr(connection$conns[as.numeric(tables_available[index,2])], 
                                      "aux_col_no_variable_left", 
                                      as.symbol(paste0(table_name[index], "[,", 1,":", row-1,"]")))
        } else{
          extreme_left <- TRUE
        }
        # Right creation
        if(row != nrow(lists$table_columns_types)){
          DSI::datashield.assign.expr(connection$conns[as.numeric(tables_available[index,2])], 
                                      "aux_col_no_variable_right", 
                                      as.symbol(paste0(table_name[index], "[,", row+1,":", 
                                                       nrow(lists$table_columns_types),"]")))
        } else{
          extreme_right <- TRUE
        }
        incProgress(0.3 / length(input$available_tables_cols_render_rows_selected))
        # Remove original table
        ds.rm(as.character(table_name[index]), connection$conns[as.numeric(tables_available[index,2])])
        incProgress(0.3 / length(input$available_tables_cols_render_rows_selected))
        # Merge auxiliary table with column with new class and assign same name as the original table
        if(extreme_left){
          ds.cbind(c(variable, "aux_col_no_variable_right"), newobj = as.character(table_name[index]), 
                   datasources = connection$conns[as.numeric(tables_available[index,2])])
          ds.rm("aux_col_no_variable_right", datasources = connection$conns[as.numeric(tables_available[index,2])])
        } else if(extreme_right){
          ds.cbind(c("aux_col_no_variable_left", variable), newobj = as.character(table_name[index]), 
                   datasources = connection$conns[as.numeric(tables_available[index,2])])
          ds.rm("aux_col_no_variable_left", datasources = connection$conns[as.numeric(tables_available[index,2])])
        } else{
          ds.cbind(c("aux_col_no_variable_left", variable, "aux_col_no_variable_right"), newobj = as.character(table_name[index]), 
                   datasources = connection$conns[as.numeric(tables_available[index,2])])
          ds.rm("aux_col_no_variable_left", datasources = connection$conns[as.numeric(tables_available[index,2])])
          ds.rm("aux_col_no_variable_right", datasources = connection$conns[as.numeric(tables_available[index,2])])
          ds.rm(variable, datasources = connection$conns[as.numeric(tables_available[index,2])])
        }
      }
    })
    
    lists$table_columns_types[row, column] <<- new_class#  DT::coerceValue(new_class, lists$table_columns_types[row, column, with = FALSE])
    replaceData(proxy, lists$table_columns_types, resetPaging = FALSE)
    
  }, error = function(w){
    showNotification("Class change not allowed", duration = 2, closeButton = FALSE, type = "error")
  })

})

# proxy = dataTableProxy('a')
observeEvent(input$column_types_table_cell_edit, {
  browser()
  info = input$column_types_table_cell_edit
  i = info$row
  j = info$col
  v = info$value
  exposom$lod_candidates[i, j] <<- DT::coerceValue(v, as.numeric(exposom$lod_candidates[i, j]))
  replaceData(proxy, exposom$lod_candidates, resetPaging = FALSE)
})

observe({
  if(input$tabs == "table_columns_a") {
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
    output$available_tables_cols <- renderUI({
      dataTableOutput("available_tables_cols_render")
    })
  }
})