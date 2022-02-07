#' Compute the decision criteria (dc) for classifying hospital results based on
#' binary quality indicators without risk adjustment
#'
#' Compute the decision criteria (dc) for classifying hospital (or other
#' healthcare providers) results based on binary quality indicators without risk
#' adjustment.
#' For the computation of the decision criteria, the number of "interesting"
#' events O is assumed to follow a binomial distribution with success
#' probability \eqn{\theta}, \eqn{n} number of trials and with a
#' Beta-distributed prior with parameters \code{a = priori_a} and
#' \code{b = priori_b}. This type of indicator is called a "rate indicator".
#' In case of \code{pathway = "reporting"}, the utility function is based on a
#' generalized 0-1 utility, with \eqn{\mu} denoting the utility of a correct
#' outlier classification and \eqn{\eta} as utility of a correct non-outlier
#' classification. In this case, the decision criteria is equivalent to the
#' posterior probability \deqn{\mbox{Pr}(\theta \le R|o,n).}
#' In case of \code{pathway = "change"}, the utility of a correct outlier
#' classification depends upon the parameter \eqn{\theta} and the number of
#' patients \eqn{n}, with the decision criteria calculated as
#' \deqn{\int_{0}^{1} (\theta-R)_+ \cdot n \cdot f(\theta, o, n) d\theta.}
#'
#' @param o the observed number of events (a vector of non-negative integers)
#' @param n total number of cases (a vector of non-negative integers that
#'  satisfies \code{o <= n})
#' @param t the reference value (a vector of numbers
#'  between 0 and 1)
#' @param alternative direction of the alternative (a character vector with
#'  elements \code{"greater"} or \code{"less"}). The default value is
#'  \code{"less"}.
#' @param prior_a value for parameter a in the Beta-distributed prior
#' @param prior_b value for parameter b in the Beta-distributed prior
#' @param pathway The pathway for which the decision criteria is calculated
#' (either \code{"reporting"} or \code{"change"})
#' @param value_if_n0 The computed decision criteria for the case
#' \code{o = n = 0} (a numeric of length one or \code{NULL}).  The default
#' value is \code{NA_real_}. If equal to \code{NULL}, then the computed decision
#'  criteria is equal to 1
#' @param warn_if_n0 logical of length one.  If \code{TRUE} (the default), then
#'  print a warning if the combination \code{o = n = 0} occurs in the input.
#'
#' @return A vector of computed decision criteria
#'
#' @export
#' @importFrom stats pbeta
#' @references Hengelbrock J, Rauh J, Cederbaum J, Kähler M, Höhle M (2021),
#' Hospital profiling using Bayesian decision theory,
#' medRxiv 2021.06.23.21259367; doi: https://doi.org/10.1101/2021.06.23.21259367
#'
#' @examples
#' compute_rate_dc(
#'   o = 1, n = 2, t = 0.95,
#'   alternative = "less"
#' )
compute_rate_dc <- function(o, n, t,
                            alternative = "greater",
                            prior_a = 0.5, prior_b = 0.5,
                            pathway = "reporting",
                            value_if_n0 = NA, warn_if_n0 = TRUE) {
  assert_numeric_vector(o)
  assert_numeric_vector(n)
  assert_numeric_vector(t, xmin = 0)
  assert_integral(o, 0L, n)
  assert_integral(n)
  assert_choices(alternative, c("greater", "less"))
  assert_choices(pathway, c("reporting", "change"))
  assert_numeric_vector(prior_a, xmin = 0)
  assert_numeric_vector(prior_b, xmin = 0)
  if (!is.null(value_if_n0)) {
    assert_numeric_vector(value_if_n0)
    assert_scalar(value_if_n0)
  }
  assert_logical(warn_if_n0)
  assert_scalar(warn_if_n0)

  # empty input gives empty output
  if (min(
    length(o), length(n), length(t),
    length(alternative), length(prior_a), length(prior_b)
  ) == 0L) {
    return(numeric())
  }

  # the length of the output:
  len <- max(
    length(o), length(n), length(t), length(alternative)
  )

  # recycle all arguments that appear in ifelse statements to output length:
  alternative <- rep(alternative, length.out = len)
  # recycle n to output length, for two reasons:
  # 1. as argument to pbinom, to ensure correct recycling there
  # 2. to correctly check whether o <= n
  n <- rep(n, length.out = len)

  if (pathway == "reporting") {
    pbg <- pbeta(t, prior_a + o, prior_b + n - o)

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
  } else { # pathway change:
    o <- ifelse(alternative == "less", n - o, o)
    t <- ifelse(alternative == "less", 1 - t, t)

    ap <- prior_a + o
    bp <- prior_b + n - o

    # compute decision criteria
    result <- n * (ap / (ap + bp) * pbeta(t, ap + 1, bp, lower.tail = FALSE) -
                  t * pbeta(t, ap, bp, lower.tail = FALSE))

  }

  if (!is.null(value_if_n0)) {
    zeros <- !is.na(n) & (n == 0L) & !is.na(o) & (o == 0L)
    if (any(zeros, na.rm = TRUE)) {
      if (warn_if_n0) warning("n = 0 encountered.")
      result[zeros] <- value_if_n0
    }
  }
  result
}
