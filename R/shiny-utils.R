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
  appDir <- paste0(system.file(package = "gridsampler"), "/shiny")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `gridsampler`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
