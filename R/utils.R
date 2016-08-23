#' Run gridsampler app
#'
#' This function starts the gridsampler shiny app.
#' @param display.mode \code{auto} by default, can also be \code{showcase}.
#' See \link[shiny]{runApp}.
#' @param launch.browser Boolean, set \code{TRUE} to open the app in the browser.
#' See \link[shiny]{runApp}.
#' @export
#' @import shiny
#' @import shinythemes
#' @examples
#' \dontrun{
#' gridsampler()
#' }
gridsampler <- function(display.mode = "auto",
                        launch.browser = getOption("shiny.launch.browser", interactive())) {
  appDir <- system.file("shiny", package = "gridsampler")
  if (appDir == "") {
    stop("Could not find shiny directory. Try re-installing `gridsampler`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = display.mode, launch.browser = launch.browser)
}


#' Adjusted sampling function
#'
#' @inheritParams stats::sample
#' @keywords internal
#'
sample_new <- function(x, size, replace = FALSE, prob = NULL)
{
  if (length(x) == 1)
    rep(x, size)
  else
    sample(x=x, size=size, replace = replace, prob = prob)
}
