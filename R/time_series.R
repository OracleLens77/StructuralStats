#' @title Simple exponential smoothing
#' @description Recurrence formula for stationary time series, suitable
#'   for short-term forecasts of a cost share or similar structural
#'   indicator.
#'   Formula: \eqn{S_t = \alpha y_t + (1-\alpha) S_{t-1}}
#' @param y Numeric vector of time-series observations.
#' @param alpha Smoothing parameter, 0 < alpha < 1.
#' @return A numeric vector of smoothed values, same length as y.
#' @references Afanasyev V.N. Statistical methods in the study of changes
#'   in the structure and elements of cost, 2019, formula 29.
#' @examples
#' simple_exponential_smoothing(c(100, 105, 110, 108, 112, 115), alpha = 0.3)
#' @export
simple_exponential_smoothing <- function(y, alpha = 0.3) {
  if (alpha <= 0 || alpha >= 1) stop("alpha must be between 0 and 1.")
  n <- length(y)
  s <- numeric(n)
  s[1] <- y[1]
  for (t in 2:n) {
    s[t] <- alpha * y[t] + (1 - alpha) * s[t - 1]
  }
  s
}

#' @title Holt's two-parameter exponential smoothing
#' @description Extends simple smoothing with a trend component, for
#'   non-stationary series.
#' @param y Numeric vector of time-series observations.
#' @param alpha Level-smoothing parameter, 0 < alpha < 1.
#' @param beta Trend-smoothing parameter, 0 < beta < 1.
#' @return A list with numeric vectors `level` and `trend`.
#' @references Afanasyev V.N. Statistical methods..., 2019, formulas 30-31.
#' @examples
#' holt_smoothing(c(100, 105, 110, 108, 112, 115))
#' @export
holt_smoothing <- function(y, alpha = 0.3, beta = 0.1) {
  n <- length(y)
  level <- numeric(n)
  trend <- numeric(n)

  level[1] <- y[1]
  trend[1] <- y[2] - y[1]

  for (t in 2:n) {
    level[t] <- alpha * y[t] + (1 - alpha) * (level[t - 1] + trend[t - 1])
    trend[t] <- beta * (level[t] - level[t - 1]) + (1 - beta) * trend[t - 1]
  }

  list(level = level, trend = trend)
}
