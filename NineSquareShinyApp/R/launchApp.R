
#' launches the shinyApp
#'
#' @export launchApp
#'
#' @return shiny application object
#'
#' @example \dontrun {launchApp()}
#'
#' @import shiny
#'


# wrapper for shiny::shinyApp()

library(shiny)

launchApp <- function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
