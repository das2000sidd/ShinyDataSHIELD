observeEvent(input$select_tables_descr_stats, {
  if(length(input$available_tables_render_rows_selected) > 0){
    same_cols <- TRUE
    different_study_server <- TRUE
    if(length(input$available_tables_render_rows_selected) > 1){
      same_cols <- all(lapply(input$available_tables_render_rows_selected, function(i){
        res<-all(match(lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][i,1])]], 
                       lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected[1],1])]]))
        if(is.na(res)){FALSE} else{res}
      }))
      different_study_server <- nrow(unique(lists$available_tables[input$available_tables_render_rows_selected,3])) ==
                                  length(input$available_tables_render_rows_selected)
      }
    if(!same_cols | !different_study_server){
      shinyalert("Oops!",
                 if(!same_cols){
                   "Selected tables do not share the same columns, can't pool unequal tables."
                 }else{
                   "Selected tables are not on different study servers, can't pool tables on the same study server."
                 }
                 , type = "error")
      js$disableTab("summary")
      js$disableTab("s_plot")
      js$disableTab("h_plot")
      js$disableTab("hm_plot")
      updateTabsetPanel(session, "d_statistics_t",
                        selected = "a_tables")
    }
    else{
      datashield.rm(connection$conns, "tables_descriptive")
      for(i in input$available_tables_render_rows_selected){
        lists$available_tables[type_resource == "table"][i,2]
        
        datashield.assign.expr(connection$conns[as.numeric(lists$available_tables[type_resource == "table"][i,2])],
                               "tables_descriptive", as.symbol(
                                 as.character(lists$available_tables[type_resource == "table"][i,1])
                               ))
      }
      js$enableTab("summary")
      js$enableTab("s_plot")
      js$enableTab("h_plot")
      js$enableTab("hm_plot")
      updateTabsetPanel(session, "d_statistics_t",
                        selected = "summary")
    }
  }
})

output$d_statistics_variable_selector <- renderUI({
  if(length(input$available_tables_render_rows_selected) > 1){
    output$d_statistics_variable_selector_approach <- renderUI({
      selectInput("d_statistics_variable_selector_value_approach", "Select approach", c("combine", "split"))
    })
    selectInput("d_statistics_variable_selector_value", "Select variable", 
                lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected[1],1])]]
                )
  }
  else{
    selectInput("d_statistics_variable_selector_value", "Select variable", 
                lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected,1])]]
                )
  }
})

# output$d_statistics_table_selector <- renderUI({
#   selectInput("d_statistics_table_selector_value", "Select table", lists$available_tables$table)
# })

output$d_statistics_variable_selector_scatter <- renderUI({
  if(length(input$available_tables_render_rows_selected) > 1){
    output$d_statistics_variable_selector_scatter_approach <- renderUI({
      selectInput("d_statistics_variable_selector_scatter_value_approach", "Select approach", c("combine", "split"))
    })
    selectInput("d_statistics_variable_selector_scatter_value", "Select variable", 
                lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected[1],1])]]
    )
  }
  else{
    selectInput("d_statistics_variable_selector_scatter_value", "Select variable", 
                lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected,1])]]
    )
  }
})
output$d_statistics_variable_selector_scatter2 <- renderUI({
  selectInput("d_statistics_variable_selector_scatter_value2", "Select variable", 
              lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected[1],1])]]
             )
})

output$d_statistics_variable_selector_histogram <- renderUI({
  if(length(input$available_tables_render_rows_selected) > 1){
    output$d_statistics_variable_selector_histogram_approach <- renderUI({
      selectInput("d_statistics_variable_selector_histogram_value_approach", "Select approach", c("combine", "split"))
    })
    selectInput("d_statistics_variable_selector_histogram_value", "Select variable", 
                lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected[1],1])]]
    )
  }
  else{
    selectInput("d_statistics_variable_selector_histogram_value", "Select variable", 
                lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected,1])]]
    )
  }
})

output$d_statistics_variable_selector_heatmap <- renderUI({
  if(length(input$available_tables_render_rows_selected) > 1){
    output$d_statistics_variable_selector_heatmap_approach <- renderUI({
      selectInput("d_statistics_variable_selector_heatmap_value_approach", "Select approach", c("combine", "split"))
    })
    selectInput("d_statistics_variable_selector_heatmap_value", "Select variable", 
                lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected[1],1])]]
    )
  }
  else{
    selectInput("d_statistics_variable_selector_heatmap_value", "Select variable", 
                lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected,1])]]
    )
  }
})
output$d_statistics_variable_selector_heatmap2 <- renderUI({
  selectInput("d_statistics_variable_selector_heatmap_value2", "Select variable", 
              lists$table_columns[[as.character(lists$available_tables[type_resource == "table"][input$available_tables_render_rows_selected[1],1])]]
  )
})

observe({
  if(input$tabs == "d_statistics") {
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
    output$available_tables <- renderUI({
      dataTableOutput("available_tables_render")
    })
  }
})