#' @title Launch ShinyDataSHIELD
#' @export

app <- function() {
  library(shiny)
  runApp(system.file('shinyApp', package = "ShinyDataSHIELD"))
}
