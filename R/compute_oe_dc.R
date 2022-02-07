#' Compute the decision criteria (dc) for classifying hospital results based on
#' binary quality indicators with risk adjustment
#'
#' Compute the decision criteria (dc) for classifying hospital (or other
#' healthcare providers) results based on binary quality indicators with risk
#' adjustment.
#' For the computation of the decision criteria, the number of "interesting"
#' events O is assumed to follow a Poisson distribution
#' with parameter \eqn{\theta \cdot e}, with \eqn{e} as the number of expected
#' events. The ratio O/E is usually referred to as standardized
#' mortality/morbidity ratio (SMR). The prior distribution of \eqn{\theta} is
#'  a Gamma-distribution with parameters \code{a = priori_a} and
#' \code{b = priori_b}. For risk adjusted indicators and the pathway of
#' reporting, the utility function is based on a
#' generalized 0-1 utility, with \eqn{\mu} denoting the utility of a correct
#' outlier classification and \eqn{\eta} as utility of a correct non-outlier
#' classification. In this case, the decision criteria is equivalent to the
#' posterior probability \deqn{\mbox{Pr}(\theta \le R|o,e).} For risk
#' adjusted O/E-indicators, only the decision criteria for the pathway of
#' reporting is currently implemented.
#'
#' @param o the observed number of events (a vector of non-negative integers)
#' @param e the expected number of events (a vector of non-negative numbers)
#' @param t the reference value
#' @param alternative direction of the alternative (a character vector with
#'  elements \code{"greater"} or \code{"less"}). The default value is
#'  \code{"less"}.
#' @param prior_a value for parameter a in the Gamma-distributed prior
#' @param prior_b value for parameter b in the Gamma-distributed prior
#' @param value_if_e0 The computed decision criteria for the case
#' \code{o = e = 0} (a numeric of length one or \code{NULL}).  The default
#' value is \code{NA_real_}. If equal to \code{NULL}, then the computed decision
#'  criteria is equal to 1
#' @param warn_if_e0 logical of length one.  If \code{TRUE} (the default), then
#'  print a warning if the combination \code{o = e = 0} occurs in the input.
#'
#' @return A vector of computed decision criteria
#'
#' @export
#' @importFrom stats pgamma
#' @references Hengelbrock J, Rauh J, Cederbaum J, Kähler M, Höhle M (2021),
#' Hospital profiling using Bayesian decision theory,
#' medRxiv 2021.06.23.21259367; doi: https://doi.org/10.1101/2021.06.23.21259367
#'
#' @examples
#' compute_oe_dc(
#'   o = 1, e = 0.5, t = 1.5,
#'   alternative = "less"
#' )
compute_oe_dc <- function(o, e, t,
                          alternative = "greater",
                          prior_a = 0.5, prior_b = 0,
                          value_if_e0 = NA, warn_if_e0 = TRUE) {
  assert_numeric_vector(o)
  assert_numeric_vector(e)
  assert_integral(o, 0L)
  assert_numeric_vector(t, xmin = 0)
  assert_choices(alternative, c("greater", "less"))
  assert_numeric_vector(prior_a, xmin = 0)
  assert_numeric_vector(prior_b, xmin = 0)
  if (!is.null(value_if_e0)) {
    assert_numeric_vector(value_if_e0)
    assert_scalar(value_if_e0)
  }
  assert_logical(warn_if_e0)
  assert_scalar(warn_if_e0)

  # empty input gives empty output
  if (min(
    length(o), length(e), length(t),
    length(alternative), length(prior_a), length(prior_b)
  ) == 0L) {
    return(numeric())
  }

  # the length of the output:
  len <- max(
    length(o), length(e), length(t), length(alternative)
  )

  # recycle all arguments that appear in ifelse statements to output length:
  alternative <- rep(alternative, length.out = len)
  # recycle n to output length, for two reasons:
  # 1. as argument to pbinom, to ensure correct recycling there
  # 2. to correctly check whether o <= n
  e <- rep(e, length.out = len)

  pbg <- pgamma(t, prior_a + o, prior_b + e)

  result <-
    ifelse(alternative == "greater",
           {
             pbg # the probability of x > o
           },
           ifelse(alternative == "less",
                  {
                    1 - pbg # the probability of x <= o
                  },
                  NA
           )
    )

  if (!is.null(value_if_e0)) {
    zeros <- !is.na(e) & (e == 0L) & !is.na(o) & (o == 0L)
    if (any(zeros, na.rm = TRUE)) {
      if (warn_if_e0) warning("e = 0 encountered.")
      result[zeros] <- value_if_e0
    }
  }
  result
}
