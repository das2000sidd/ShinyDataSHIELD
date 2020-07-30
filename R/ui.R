library(DSI)
library(DSOpal)
library(dsBaseClient)
library(dsOmicsClient)
library(shinydashboard)
library(shiny)
library(shinyalert)
library(DT)
library(data.table)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(shinycssloaders)

jscode <- '
$(document).keyup(function(event) {
    if ($("#password").is(":focus") && (event.keyCode == 13)) {
        $("#connect_server").click();
    }
});
'

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    uiOutput("userpanel"),
    menuItem("Connect to server", tabName = "server_connect", icon = icon("dashboard")),
    menuItem("Descriptive statistics", tabName = "d_statistics", icon = icon("dashboard")),
    menuItem("Statistic models", tabName = "statistic_models", icon = icon("dashboard")),
    menuItem("Mixed statistic models", tabName = "statistic_models_mixed", icon = icon("dashboard")),
    menuItem("Genomics", tabName = "genomics", icon = icon("dashboard"),
             menuSubItem("Analysis with BioConductor", tabName = "vcf_files",icon = icon("dashboard")),
             menuSubItem("Analysis with PLINK", tabName = "plink",icon = icon("dashboard"))
             ),
    menuItem("Omics", tabName = "omics", icon = icon("dashboard"),
             menuSubItem("LIMMA", tabName = "limma", icon = icon("dashboard"))
             )
  )
)

body <- dashboardBody(
  useShinyalert(),
  useShinyjs(),
  tabItems(
    tabItem(tabName = "server_connect",
            tabPanel('server_connect',
                     fluidRow(
                       column(6,
                              #textInput("server", "Server"),
                              h3("URL"),
                              textInput("url", "Opal server", value = "https://opal-demo.obiba.org/")
                       ),
                       column(6,
                              h3("Credentials"),
                              textInput("user", "User", value = "administrator"),
                              tags$head(tags$script(HTML(jscode))),
                              passwordInput("password", "Password", value = "password")
                       )
                     ),
                     hr(),
                     fluidRow(
                       column(6,
                              uiOutput("project_selector"),
                       ),
                       column(6,
                              uiOutput("resource_selector"),
                       )
                     ),
                     hidden(actionButton("add_server", "Add study")),
                     hidden(actionButton("remove_server", "Remove selected study")),
                     dataTableOutput("server_resources_table"),
                     fluidRow(
                       column(6,
                              actionButton("connect_server", "Connect"),
                              hidden(actionButton("connect_selected", "Connect"))
                       )
                     )
            )
    ),
    tabItem(tabName = "d_statistics",
            fluidRow(
              tabBox(width = 12,
                     tabPanel("Summary",
                              uiOutput("d_statistics_variable_selector"),
                              dataTableOutput("descriptive_summary"),
                              downloadButton("descriptive_summary_download", "Download")
                     ),
                     tabPanel("Scatter plot",
                              uiOutput("d_statistics_variable_selector_scatter"),
                              uiOutput("d_statistics_variable_selector_scatter2"),
                              selectInput("d_statistics_scatter_type", "Plot type", c("combine", "split")),
                              hidden(h5(id = "d_statistics_scatter_plot_error", "One of the variables not numerical,
                                        a Scatter plot can't be generated")),
                              withSpinner(plotOutput("d_statistics_scatter_plot"))
                     ),
                     tabPanel("Histogram",
                              uiOutput("d_statistics_variable_selector_histogram"),
                              selectInput("d_statistics_histogram_type", "Plot type", c("combine", "split")),
                              hidden(h5(id = "d_statistics_histogram_plot_error", "The variables is not numerical,
                                        a Histogram can't be generated")),
                              withSpinner(plotOutput("d_statistics_histogram_plot"))
                     ),
                     tabPanel("Heatmap",
                              uiOutput("d_statistics_variable_selector_heatmap"),
                              uiOutput("d_statistics_variable_selector_heatmap2"),
                              selectInput("d_statistics_heatmap_type", "Plot type", c("combine", "split")),
                              hidden(h5(id = "d_statistics_heatmap_plot_error", "One of the variables is not numerical,
                                        a Heatmap can't be generated")),
                              withSpinner(plotOutput("d_statistics_heatmap_plot"))
                     )
              )
            )
    ),
    tabItem(tabName = "statistic_models",
            tabPanel('statistic_models',
                     fluidRow(
                       column(6,
                              textInput("glm_formula", "Input GLM formula:"),
                              actionButton("trigger_formula_help_glm", "Formula input help"),
                              actionButton("perform_glm", "Perform GLM")
                       ),
                       column(6,
                              selectInput("gml_output_family", "Output family:", c("gaussian", "poisson", "binomial")),
                              actionButton("gml_toggle_variables_table","Toggle variables table")
                       )
                     ),
                     dataTableOutput("available_variables_type"),
                     dataTableOutput("glm_results_table"),
                     hidden(downloadButton("glm_results_table_download", "Download GLM results"))
                     
            )
    ),
    tabItem(tabName = "statistic_models_mixed",
            tabPanel('statistic_models_mixed',
                     fluidRow(
                       column(6,
                              textInput("glmer_formula", "Input GLMer formula:"),
                              actionButton("trigger_formula_help_glmer", "Formula input help"),
                              actionButton("perform_glmer", "Perform GLMer")
                       ),
                       column(6,
                              selectInput("gmler_output_family", "Output family:", c("poisson", "binomial")),
                              actionButton("gmler_toggle_variables_table","Toggle variables table")
                       )
                     ),
                     dataTableOutput("available_variables_type2"),
                     dataTableOutput("glmer_results_table"),
                     hidden(downloadButton("glmer_results_table_download", "Download GLMer results"))
            )
    ),
    tabItem(tabName = "plink",
            fluidRow(
              tabBox(width = 12,
                     tabPanel("PLINK",
                              textInput("command", "PLINK Shell command", width = "100%"),
                              h5("NOTE: we avoid –out to indicate the output file"),
                              h5("NOTE: No need to input plink as in a shell command"),
                              code("plink < >"),
                              h5("can be inputed as"),
                              code("< >"),
                              h5(""),
                              actionButton("run_shell", "Run Shell command"),
                              hidden(actionButton("plink_show_plain", "Show PLINK terminal output")),
                              dataTableOutput("plink_results_table"),
                              hidden(downloadButton("plink_results_table_download", "Download PLINK results")),
                              bsModal("plink_results_terminal", "PLINK Terminal output", "plink_show_plain",
                                      verbatimTextOutput("plink_results_terminal_render")
                              )
                     ),
                     tabPanel("Manhattan Plot",
                              withSpinner(plotOutput("manhattan2"))
                     )
              )
            )
    ),
    tabItem(tabName = "vcf_files",
            fluidRow(
              tabBox(width = 12,
                tabPanel("GWAS",
                  uiOutput("vcf_selector_var"),
                  uiOutput("vcf_selector_cov"),
                  actionButton("gwas_trigger", "Perform GWAS"),
                  dataTableOutput("vcf_results"),
                  hidden(downloadButton("vcf_results_table_download", "Download GWAS results"))
                ),
                tabPanel("Manhattan Plot",
                  withSpinner(plotOutput("manhattan"))
                )
              )
            )
    ),
    tabItem(tabName = "limma",
            tabPanel('limma commands',
                     fluidRow(
                       column(6,
                              uiOutput("limma_variables_selector_feature"),
                              uiOutput("limma_variables_selector_covars"),
                              uiOutput("limma_sva_selector"),
                              uiOutput("limma_run")
                       ),
                       column(6,
                              uiOutput("limma_labels_selector"),
                              selectInput("limma_data_type", "Data type:", c("RNAseq", "microarray"))
                       )
                     ),
                     fluidRow(
                       column(12,
                              # uiOutput("limma_server_select"),
                              dataTableOutput("limma_results_table"),
                              hidden(downloadButton("limma_results_table_download", "Download LIMMA results"))
                     )
                     )
            )
    )
  )
)
# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "DataSHIELD"),
  sidebar,
  body
)

