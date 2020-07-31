pkg_list <- c("shiny", "shinyBS", "shinyjs", "shinyalert", "shinydashboard", "shinycssloaders", 
              "DT", "data.table", "DSI", "DSOpal", "devtools")

for(pkg in pkg_list){
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE )
  }
}

install.packages('dsBaseClient', repos=c(getOption('repos'), 'http://cran.obiba.org'), dependencies=TRUE)

devtools::install_github("isglobal-brge/dsOmicsClient")