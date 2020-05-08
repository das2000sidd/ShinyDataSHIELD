library(DSI)
library(DSOpal)
library(dsBaseClient)
library(dsOmicsClient)
library(shinydashboard)
library(shiny)
library(shinyalert)
library(DT)
library(data.table)
library(shinybusy)

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("PLINK", tabName = "plink", icon = icon("dashboard")),
    menuItem("LIMMA", tabName = "limma", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  useShinyalert(),
  tabItems(
    tabItem(tabName = "plink",
            tabPanel('p link commands',
                     fluidRow(
                       column(6,
                              #textInput("server", "Server"),
                              textInput("url", "Url")
                       ),
                       column(6,
                              textInput("user", "User"),
                              passwordInput("password", "Password")
                       )
                       ),
                     fluidRow(
                       column(6,
                              textInput("resource", "Resource"),
                              actionButton("connect", "Connect")
                     )
                     ),
                     hr(style = "border-color: grey;"),
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
                              #textInput("server", "Server"),
                              textInput("url_l", "Url")
                       ),
                       column(6,
                              textInput("user_l", "User"),
                              passwordInput("password_l", "Password")
                       )
                     ),
                     fluidRow(
                       column(6,
                              textInput("resource_l", "Resource"),
                              actionButton("connect_limma", "Connect")
                       )
                     ),
                     hr(style = "border-color: grey;"),
                     
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

