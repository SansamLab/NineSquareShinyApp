
#' launches the shinyApp
#' @name launchApp
#' @export launchApp
#'
#' @return shiny application object
#'
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()

library(shiny)

launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
