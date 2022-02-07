#' Stop with an error message
#'
#' @param msg input that should stop and cause an error message
#'
#' @noRd
raise_error <- function(msg) {
  sc <- as.list(sys.call(-2))
  if (length(sc) > 0) {
    calling_function <- sc[[1]]
    fm2 <- paste0(" (in function ", calling_function, ")")
  } else {
    fm2 <- ""
  }
  stop(msg, fm2, ".", call. = FALSE)
}

#' Generate a string from the value of a variable
#'
#' @param x input that should be converted to a string
#' @return input as string
#'
#' @noRd
vs <- function(x) {
  value <- deparse(x)
  if (length(value) > 1) {
    value <- paste0(value[1], "...")
  }
  value
}

#' Check whether argument is a numeric vector
#'
#' @param x input vector
#' @param xmin lower bound
#' @param xmax upper bound
#'
#' @noRd
assert_numeric_vector <- function(x, xmin = NULL, xmax = NULL) {
  if (!(is.numeric(x) || (all(is.na(x)) && is.logical(x)))) {
    raise_error(paste0(
      deparse(substitute(x)), " = ", deparse(x),
      " is not a numeric vector"
    ))
  }
  if (!is.null(xmin) && any(x < xmin, na.rm = TRUE)) {
    raise_error(paste0(
      deparse(substitute(x)), " = ", deparse(x),
      " is smaller than ", deparse(substitute(xmin))
    ))
  }
  if (!is.null(xmax) && any(x > xmax, na.rm = TRUE)) {
    raise_error(paste0(
      deparse(substitute(x)), " = ", deparse(x),
      " is larger than ", deparse(substitute(xmax))
    ))
  }
}

#' Check whether argument is a vector of integers
#'
#' @param x input vector
#' @param xmin lower bound
#' @param xmax upper bound
#'
#' @noRd
assert_integral <- function(x, xmin = NULL, xmax = NULL) {
  if (any(!(is.na(x) | (x == as.integer(x))))) {
    raise_error(paste0(
      deparse(substitute(x)), " = ", vs(x),
      " is not integral"
    ))
  }
  if (!is.null(xmin) && any(x < xmin, na.rm = TRUE)) {
    raise_error(paste0(
      deparse(substitute(x)), " = ", vs(x),
      " is smaller than ", deparse(substitute(xmin))
    ))
  }
  if (!is.null(xmax) && any(x > xmax, na.rm = TRUE)) {
    raise_error(paste0(
      deparse(substitute(x)), " = ", vs(x),
      " is larger than ", deparse(substitute(xmax))
    ))
  }
}

#' Check whether x is a scalar (i.e. has length one)
#'
#' @param x input value
#' @param xmin lower bound
#' @param xmax upper bound
#'
#' @noRd
assert_scalar <- function(x) {
  if (length(x) != 1) {
    raise_error(paste0(
      deparse(substitute(x)), " = ", vs(x),
      " does not have length one (length is ", length(x), ")"
    ))
  }
}

#' Check whether argument belongs to a set of choices
#'
#' @param x input value
#' @param choices the set of choices
#'
#' @noRd
assert_choices <- function(x, choices) {
  if (any(!(is.na(x) | x %in% choices))) {
    raise_error(paste0(
      deparse(substitute(x)), " = ", vs(x),
      " is not in ", deparse(substitute(choices))
    ))
  }
}

#' Check whether argument is logical
#'
#' @param x input value
#'
#' @noRd
assert_logical <- function(x) {
  if (!is.logical(x)) {
    raise_error(paste0(
      deparse(substitute(x)), " = ", vs(x),
      " is not logical"
    ))
  }
}
