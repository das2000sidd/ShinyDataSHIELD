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
    menuItem("p link test", tabName = "plink", icon = icon("dashboard")),
    menuItem("limma test", tabName = "limma", icon = icon("dashboard"))
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
                       column(12,
                              textInput("command", "Limma command"),
                              actionButton("run_limma", "Run limma")
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

