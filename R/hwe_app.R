#' Shiny app of the Hardy Weinberg Equillibrium calculations
#'
#' App to easily calculate and visualize the HWE.
#'
#' @export

hwe_app <- function() {
  appDir <- system.file("shiny-examples", "hwe_calc", package = "daDoctoR")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `daDoctoR`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
