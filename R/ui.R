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
             menuSubItem("VCF files", tabName = "vcf_files",icon = icon("dashboard")),
             menuSubItem("Plink files", tabName = "plink",icon = icon("dashboard")),
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
                       column(4,
                              #textInput("server", "Server"),
                              h3("URL"),
                              textInput("url", "Url")
                       ),
                       column(4,
                              h3("Credentials"),
                              textInput("user", "User"),
                              passwordInput("password", "Password")
                       ),
                       h3(id = "optional_banner", "Optional"),
                       column(4,
                              textInput("project", "Project"),
                              textInput("resource", "Resource"),
                              selectInput("selector_optional_table", "Type", c("Table", "Resource"))
                       )
                     ),
                     hr(),
                     fluidRow(
                       column(6,
                              uiOutput("project_selector"),
                              uiOutput("project_selector_button"),
                       ),
                       column(6,
                              uiOutput("resource_selector"),
                              uiOutput("resource_selector_button")
                       )
                     ),
                     fluidRow(
                       column(6,
                              actionButton("connect_server", "Connect")
                       )
                     )
            )
    ),
    tabItem(tabName = "d_statistics",
            tabPanel("d_statistics",
                     fluidRow(
                       column(12,
                              h1("hola")
                     )
                     )
              
            )
    ),
    tabItem(tabName = "plink",
            tabPanel('p link commands',
                     fluidRow(
                       column(12,
                              textInput("command", "Shell command"),
                              h5("NOTE: we avoid –out to indicate the output file"),
                              h5("NOTE: No need to input plink as in a shell command (plink < > can be inputed as < >)"),
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
                  h3("Percentages"),
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

