#' launches the shinyApp
#' @name launchApp
#' @export launchApp
#' @return shiny application object
#' @import shiny

launchApp <- function() {
  library(shiny)
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
