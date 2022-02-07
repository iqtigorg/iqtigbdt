#' Run interactive examples for a funnelplot and the analysis of sensitivity and
#' specificity
#'
#' Interactive versions of the \href{https://iqtig.shinyapps.io/funnel_plot/}{funnelplot}
#' and the \href{https://iqtig.shinyapps.io/sensitivity_specificity}{analysis of sensitivity and specificity}
#' are available online.
#'
#' @param example Either \code{"funnelplot"} for an interactive version of the
#' funnelplot or \code{"sensitivity_specificity"} for the interactive analysis
#' of sensitivity and specificity.
#'
#' @references Spiegelhalter, D J  (2005),
#' Funnel plots for comparing institutional performance,
#' Statistics in Medicine 24(8):1185--1202
#'
#' @examples
#' if (interactive()) {
#'   run_example("funnelplot")
#' }
#' @export

run_example <- function(example) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "iqtigbdt"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'"
    )

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
    !example %in% validExamples) {
    stop(
      "Please run `run_example()` with a valid example app as an argument.\n",
      validExamplesMsg,
      call. = FALSE
    )
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", example, package = "iqtigbdt")
  shiny::runApp(appDir, display.mode = "normal")
}
