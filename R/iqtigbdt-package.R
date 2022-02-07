#' iqtigbdt: Classify quality indicators using Bayesian decision theory (bdt)
#'
#' This package implements three methods for the binary classification of quality
#' indicator results as outlier or non-outlier: a naive classification based
#' on comparing a point estimate with a reference area; a method for the pathway
#' of external reporting; and a method for the pathway of change. The methods are
#' described in detail in Hengelbrock et al. (2021). Furthermore, the package
#' provides an interactive example for a funnelplot
#' (\code{run_example("funnelplot")}) and the analysis of specificity and
#' sensitivity (\code{run_example("specificity_sensitivity")}),
#' as discussed in Hengelbrock et al. (2021). The Shiny-Apps are also available
#' online (see \href{https://iqtig.shinyapps.io/funnel_plot/}{funnelplot}
#' and \href{https://iqtig.shinyapps.io/sensitivity_specificity}{analysis of sensitivity and specificity}).
#'
#' @references Hengelbrock J, Rauh J, Cederbaum J, Kähler M, Höhle M (2021),
#' Hospital profiling using Bayesian decision theory,
#' medRxiv 2021.06.23.21259367; doi: https://doi.org/10.1101/2021.06.23.21259367
#' @docType package
#' @keywords package
#' @name iqtigbdt-package

NULL

# See eliminating R CMD check notes in https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html
