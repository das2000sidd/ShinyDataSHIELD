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

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    uiOutput("userpanel"),
    menuItem("Connect to server", tabName = "server_connect", icon = icon("dashboard")),
    menuItem("Descriptive statistics", tabName = "d_statistics", icon = icon("dashboard")),
    menuItem("Genomics", tabName = "genomics", icon = icon("dashboard"),
             menuSubItem("Analysis with BioConductor", tabName = "vcf_files",icon = icon("dashboard")),
             menuSubItem("Analysis with PLINK", tabName = "plink",icon = icon("dashboard")),
             menuSubItem("GWAS plot", tabName = "gwas_plot",icon = icon("dashboard"))),
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
                              textInput("url", "Opal server")
                       ),
                       column(6,
                              h3("Credentials"),
                              textInput("user", "User"),
                              passwordInput("password", "Password")
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
                              uiOutput("d_statistics_table_selector"),
                              uiOutput("d_statistics_server_selector"),
                              uiOutput("d_statistics_variable_selector"),
                              actionButton("trigger_d_statistics", "lesgo"),
                              dataTableOutput("descriptive_summary")
                     ),
                     tabPanel("Scatter plot",
                              uiOutput("d_statistics_table_selector_scatter"),
                              uiOutput("d_statistics_variable_selector_scatter"),
                              uiOutput("d_statistics_variable_selector_scatter2"),
                              actionButton("trigger_d_statistics_scatter", "lesgo"),
                              actionButton("d_statistics_scatter_plot_trigger", "lesgoplot"),
                              uiOutput("d_statistics_scatter_plot_ui")
                     )
              )
            )
    ),
    tabItem(tabName = "plink",
            tabPanel('p link commands',
                     fluidRow(
                       column(12,
                              textInput("command", "PLINK Shell command", width = "100%"),
                              h5("NOTE: we avoid –out to indicate the output file"),
                              h5("NOTE: No need to input plink as in a shell command"),
                              code("plink < >"),
                              h5("can be inputed as"),
                              code("< >"),
                              h5(""),
                              actionButton("run_shell", "Run Shell command"),
                              actionButton("plink_show_plain", "Show PLINK terminal output"),
                              dataTableOutput("plink_results_table"),
                              bsModal("plink_results_terminal", "PLINK Terminal output", "plink_show_plain",
                                      verbatimTextOutput("plink_results_terminal_render")
                                      )
                     )
                     )
            )
    ),
    tabItem(tabName = "vcf_files",
            fluidRow(
              tabBox(width = 12,
                tabPanel("Contingency table",
                  uiOutput("vcf_ct_selector"),
                  h3("Counts"),
                  dataTableOutput("vcf_ct_counts"),
                  h3(id = "vcf_perc", "Percentages"),
                  dataTableOutput("vcf_ct_perc")
                ),
                tabPanel("GWAS",
                  uiOutput("vcf_selector_var"),
                  uiOutput("vcf_selector_cov"),
                  actionButton("gwas_trigger", "Perform GWAS"),
                  dataTableOutput("vcf_results")
                )
              )
            )
    ),
    tabItem(tabName = "gwas_plot",
            fluidRow(
              withSpinner(plotOutput("gwas_manhattan"))
            )
    ),
    tabItem(tabName = "limma",
            tabPanel('limma commands',
                     fluidRow(
                       column(6,
                              uiOutput("limma_variables_selector"),
                              uiOutput("limma_labels_selector"),
                              uiOutput("limma_sva_selector")
                       )
                     ),
                     fluidRow(
                       column(12,
                              uiOutput("limma_run")
                       )
                     ),
                     
                     fluidRow(
                       column(12,
                              uiOutput("limma_server_select"),
                              dataTableOutput("limma_results_table")
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

