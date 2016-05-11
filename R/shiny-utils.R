#' Run gridsampler app
#'
#' This function starts the gridsampler shiny app.
#' @return A shiny app
#' @export
#' @import shiny
#' @examples
#' \dontrun{
#' gridsampler()
#' }
gridsampler <- function() {
  appDir <- system.file("shiny", package = "gridsampler")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `gridsampler`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
