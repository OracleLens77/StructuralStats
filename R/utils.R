#' @title Internal input checks
#' @description Validates that two vectors of shares are numeric, equal
#'   length, and non-negative. Used internally by the structural-shift
#'   functions in this package.
#' @param d_base Numeric vector of shares in the base period.
#' @param d_current Numeric vector of shares in the current period.
#' @return Invisibly TRUE, or throws an error.
#' @keywords internal
check_inputs <- function(d_base, d_current) {
  if (!is.numeric(d_base) || !is.numeric(d_current)) {
    stop("d_base and d_current must be numeric vectors.")
  }
  if (length(d_base) != length(d_current)) {
    stop("d_base and d_current must have the same length.")
  }
  if (any(d_base < 0) || any(d_current < 0)) {
    stop("Shares cannot be negative.")
  }
  invisible(TRUE)
}

#' @title Internal length check for index functions
#' @param ... Numeric vectors to compare.
#' @return Invisibly TRUE, or throws an error.
#' @keywords internal
check_lengths <- function(...) {
  lens <- vapply(list(...), length, integer(1))
  if (length(unique(lens)) != 1) {
    stop("All input vectors must have the same length.")
  }
  invisible(TRUE)
}
