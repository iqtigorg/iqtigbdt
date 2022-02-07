#' Decide if a given hospital decision criteria is to be classified as outlier
#' or non-outlier
#'
#' Decide if a given hospital decision criteria is to be classified as outlier
#' or non-outlier
#'
#' @param dc A vector of computed decision criteria as returned
#' by \code{\link{compute_rate_dc}} and \code{\link{compute_oe_dc}}
#' @param threshold_value Threshold value for the classification of the decision
#'  criteria. For \code{pathway = "reporting"}, it is the ratio of utilities
#'  \eqn{\frac{\mu}{\mu + \eta},} with  \eqn{\mu} denoting the utility of a correct
#'  outlier classification and \eqn{\eta} as utility of a correct non-outlier
#' classification. For \code{pathway = "change"}, it is the number of patients
#' with event of interest above the number which is tolerated by the reference
#' area. For \code{pathway = "naive"}, it is the reference value of
#' the indicator
#' @param digits Number of digits the result is rounded to before classification
#' @param pathway The pathway used for calculating the expected loss (either
#'  \code{"reporting"} for the pathway of reporting or \code{"change"} for
#'  the pathway of change)
#' @param alternative direction of the alternative (a character vector with
#'  elements \code{"greater"} or \code{"less"}). The default value is
#'  \code{"less"}.
#'
#' @return A vector of binary classification decisions indicating whether a
#' result is an outlier (\code{TRUE}) or non-outlier (\code{FALSE})
#'
#' @export
#'
#' @examples
#' classify_dc(
#'   dc = 0.048, threshold_value = 0.05
#' )
classify_dc <- function(dc, threshold_value, digits = 8,
                        pathway = "reporting",
                        alternative = "greater") {
  assert_numeric_vector(dc)
  assert_numeric_vector(threshold_value, xmin = 0)
  assert_numeric_vector(digits, xmin = 0, xmax = 15)
  assert_choices(alternative, c("greater", "less"))
  assert_choices(pathway, c("reporting", "change", "naive"))
  # empty input gives empty output
  if (min(
    length(dc), length(threshold_value), length(digits),
    length(alternative), length(pathway)
  ) == 0L) {
    return(logical())
  }

  if (pathway == "reporting") {
    round(dc, digits) <= threshold_value
  } else if (pathway == "change") {
    round(dc, digits) > threshold_value
  } else if (pathway == "naive") {
    if (alternative == "less") {
      round(dc, digits) < threshold_value
    } else if(alternative == "greater") {
      round(dc, digits) > threshold_value
    }
  } else {
    NA
  }
}
