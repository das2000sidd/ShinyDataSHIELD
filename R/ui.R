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

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    uiOutput("userpanel"),
    menuItem("Connect to server", tabName = "server_connect", icon = icon("dashboard")),
    menuItem("PLINK", tabName = "plink", icon = icon("dashboard")),
    menuItem("LIMMA", tabName = "limma", icon = icon("dashboard"))
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
    tabItem(tabName = "plink",
            tabPanel('p link commands',
                     fluidRow(
                       column(12,
                              textInput("command", "Shell command"),
                              actionButton("run_shell", "Run Shell command")
                     )
                     )
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

