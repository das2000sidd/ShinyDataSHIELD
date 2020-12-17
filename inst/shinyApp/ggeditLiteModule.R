ggeditLiteUI <- function(id) {
  fluidRow(
    column(12,
           actionButton(NS(id, "alto"),"Plot editor"),
           bsModal(NS(id, "yep"), "Plot editor", NS(id, "alto"),
                   # uiOutput(NS(id, "ggeditLitePlot_ui")),
                   plotOutput(NS(id, "ggeditLitePlot")),
                   h4("Edit options:"),
                   fluidRow(column(4,
                                   uiOutput(NS(id, "text_size_ui")),
                                   uiOutput(NS(id, "line_size_ui")),
                                   uiOutput(NS(id, "x_axis_angle_ui")),
                                   uiOutput(NS(id, "x_limits_ui")),
                                   uiOutput(NS(id, "y_limits_ui"))
                                   ),
                            column(4,
                                   uiOutput(NS(id, "title_ui")),
                                   uiOutput(NS(id, "subtitle_ui")),
                                   uiOutput(NS(id, "caption_ui")),
                                   uiOutput(NS(id, "x_title_ui")),
                                   uiOutput(NS(id, "y_title_ui"))
                                   ),
                            column(4,
                                   prettyCheckbox(
                                     inputId = NS(id, "x_axis"), label = "X axis", icon = icon("check"), value = T
                                   ),
                                   prettyCheckbox(
                                     inputId = NS(id, "y_axis"), label = "Y axis", icon = icon("check"), value = T
                                   ),
                                   prettyCheckbox(
                                     inputId = NS(id, "minor_grid"), label = "Minor grid", icon = icon("check"), value = T
                                   ),
                                   prettyCheckbox(
                                     inputId = NS(id, "major_grid"), label = "Major grid", icon = icon("check"), value = T
                                   ),
                                   textInput(NS(id, "palette"), "Colour palette", "Default")
                                   )),
                   actionButton(NS(id, "ala"),"Update plot"),
                   downloadButton(NS(id, "plot_download"), "Download modified plot"),
                   size = "large"
           )
           )
  )
}

ggeditLiteServer <- function(id, plot_name) {
  plot_modified <- reactiveVal(ggplot())
  moduleServer(id, function(input, output, session) {
    observeEvent(input$alto, {
      plot_modified(plots[[plot_name]])
      output$text_size_ui <- renderUI({
        numericInput(NS(id, "text_size"), "Text size", plot_modified()$theme$text$size)
      })
      output$line_size_ui <- renderUI({
        numericInput(NS(id, "line_size"), "Line size", plot_modified()$theme$line$size)
      })
      output$x_axis_angle_ui <- renderUI({
        numericInput(NS(id, "x_axis_angle"), "X axis angle", 
                     if(is.null(plot_modified()$theme$axis.text.x$angle)){0} 
                     else{plot_modified()$theme$axis.text.x$angle})
      })
      output$x_limits_ui <- renderUI({
        xlims_max <- plot_modified()$plot_env$xlims
        if(!is.null(xlims_max)){
          if(!is.null(input$x_limits)){xlims <- input$x_limits}else{xlims <- xlims_max}
          sliderInput(NS(id, "x_limits"), "X limits", min = xlims_max[1], max = xlims_max[2], value = xlims)
        }
      })
      output$y_limits_ui <- renderUI({
        ylims_max <- ggplot_build(plot_modified())$layout$panel_params[[1]]$y.range # plot_modified()$plot_env$ylims
        if(!is.null(ylims_max)){
          if(!is.null(input$y_limits)){ylims <- input$y_limits}else{ylims <- ylims_max}
          sliderInput(NS(id, "y_limits"), "Y limits", min = ylims_max[1], max = ylims_max[2], value = ylims)
        }
      })
      output$title_ui <- renderUI({
        textInput(NS(id, "title"), "Title", plot_modified()$labels$title)
      })
      output$subtitle_ui <- renderUI({
        textInput(NS(id, "subtitle"), "Subtitle", plot_modified()$labels$subtitle)
      })
      output$caption_ui <- renderUI({
        textInput(NS(id, "caption"), "Caption", plot_modified()$labels$caption)
      })
      output$x_title_ui <- renderUI({
        textInput(NS(id, "x_title"), "X Title", plot_modified()$labels$x)
      })
      output$y_title_ui <- renderUI({
        textInput(NS(id, "y_title"), "Y Title", plot_modified()$labels$y)
      })
    })
    
    
    
    output$ggeditLitePlot <- renderPlot({plot_modified()})
    
    observeEvent(input$ala, {
      if(!input$x_axis){
        plot_modified(plot_modified() + theme(axis.text.x=element_blank(),
                                              axis.ticks.x=element_blank()))
      }
      else{
        plot_modified(plot_modified() + theme(axis.text.x=element_text(),
                                              axis.ticks.x=element_line()))
      }
      if(!input$y_axis){
        plot_modified(plot_modified() + theme(axis.text.y=element_blank(),
                                              axis.ticks.y=element_blank()))
      }
      else{
        plot_modified(plot_modified() + theme(axis.text.y=element_text(),
                                              axis.ticks.y=element_line()))
      }
      if(!input$minor_grid){
        plot_modified(plot_modified() + theme(panel.grid.minor = element_blank()))
      }
      else{
        plot_modified(plot_modified() + theme(panel.grid.minor = element_line(size = (input$line_size/2))))
      }
      if(!input$major_grid){
        plot_modified(plot_modified() + theme(panel.grid.major = element_blank()))
      }
      else{
        plot_modified(plot_modified() + theme(panel.grid.major = element_line()))
      }
      browser()
      plot_modified(plot_modified() + 
                      ggplot2::theme(text = ggplot2::element_text(size=if(is.null(input$text_size) | is.na(input$text_size)){NULL}
                                                                       else{input$text_size}),
                                     line = ggplot2::element_line(size=if(is.null(input$line_size) | is.na(input$line_size)){NULL}
                                                                  else{input$line_size}),
                                     axis.text.x = ggplot2::element_text(angle=input$x_axis_angle)) +
                      ggplot2::labs(title = input$title,
                                    subtitle = input$subtitle,
                                    caption = input$caption) +
                      ggplot2::xlab(input$x_title) +
                      ggplot2::ylab(input$y_title) +
                      tryCatch({ggplot2::xlim(input$x_limits)}, error = function(w){NULL}) +
                      tryCatch({ggplot2::ylim(input$y_limits)}, error = function(w){NULL}) +
                      tryCatch({scale_fill_brewer(palette=input$palette)}, warning = function(w){NULL}) +
                      tryCatch({scale_color_brewer(palette=input$palette)}, warning = function(w){NULL})
                      )
    })
    output$plot_download <- downloadHandler(
      filename = "plot.png",
      content = function(file) {
        ggsave(file, plot = last_plot())
      }
    )
  })
}