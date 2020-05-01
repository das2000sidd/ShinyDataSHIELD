library(DSI)
library(DSOpal)
library(dsBaseClient)
library(dsOmicsClient)
library(shinydashboard)
library(shiny)
library(shinyalert)

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "plink", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
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
    )
  )
)
# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "rexposome"),
  sidebar,
  body
)

