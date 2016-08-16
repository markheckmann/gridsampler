#' Run gridsampler app
#'
#' This function starts the gridsampler shiny app.
#' @param display.mode \code{auto} by default, can also be \code{showcase}.
#' See \link[shiny]{runApp}.
#' @param launch.browser Boolean, set \code{TRUE} to open the app in the browser.
#' See \link[shiny]{runApp}.
#' @return A shiny app
#' @export
#' @import shiny
#' @import shinythemes
#' @examples
#' \dontrun{
#' gridsampler()
#' }
gridsampler <- function(display.mode = "auto",
                        launch.browser = getOption("shiny.launch.browser", interactive())) {
  appDir <- paste0(system.file(package = "gridsampler"), "/shiny")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `gridsampler`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = display.mode, launch.browser = launch.browser)
}
