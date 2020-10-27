#' @title Launch ShinyDataSHIELD
#' @export

app <- function() {
  library(shiny)
  runApp('inst/shinyApp')
}