#' @title Gini coefficient
#' @description Measures the degree of unevenness (concentration) of a
#'   distribution, based on the Lorenz curve.
#' @param shares Numeric vector of shares (parts of a whole).
#' @return A numeric value between 0 (perfectly even) and 1 (fully
#'   concentrated).
#' @references Gatev K., Gateva N. Business Statistics, sec. 9.5.2.
#' @examples
#' shares <- c(0.1, 0.2, 0.3, 0.4)
#' gini_coefficient(shares)
#' @export
gini_coefficient <- function(shares) {
  if (!is.numeric(shares)) stop("shares must be a numeric vector.")
  if (any(shares < 0)) stop("Shares cannot be negative.")

  sorted_shares <- sort(shares)
  n <- length(sorted_shares)

  numerator <- sum(seq_len(n) * sorted_shares)
  denominator <- sum(sorted_shares)

  (2 * numerator) / (n * denominator) - (n + 1) / n
}

#' @title Lorenz curve coordinates
#' @description Returns the (x, y) coordinates needed to plot a Lorenz
#'   curve for a set of shares.
#' @param shares Numeric vector of absolute values or shares.
#' @return A data frame with columns x (cumulative share of units) and
#'   y (cumulative share of the characteristic).
#' @examples
#' lorenz_curve_data(c(0.1, 0.2, 0.3, 0.4))
#' @export
lorenz_curve_data <- function(shares) {
  sorted_shares <- sort(shares)
  cum_units <- cumsum(rep(1 / length(shares), length(shares)))
  cum_shares <- cumsum(sorted_shares) / sum(sorted_shares)
  data.frame(x = c(0, cum_units), y = c(0, cum_shares))
}
