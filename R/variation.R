#' @title Range of variation
#' @description The simplest dispersion measure: the difference between
#'   the maximum and minimum value of a characteristic.
#'   Formula: \eqn{d = x_{max} - x_{min}}
#' @param x Numeric vector of observations.
#' @return A single numeric value.
#' @references Gatev K., Gateva N. Business Statistics, sec. 3.5.2, formula 3.27.
#' @examples
#' variation_range(c(520, 560, 600, 640, 680, 720, 760, 800))
#' @export
variation_range <- function(x) {
  max(x) - min(x)
}

#' @title Coefficient of variation by range
#' @description Relative form of the range, expressed as a percentage of
#'   the mean.
#'   Formula: \eqn{V_d = \frac{x_{max} - x_{min}}{\bar{x}} \cdot 100}
#' @param x Numeric vector of observations.
#' @return A single numeric value (percent).
#' @references Gatev K., Gateva N. Business Statistics, formula 3.28.
#' @export
variation_range_coef <- function(x) {
  variation_range(x) / mean(x) * 100
}

#' @title Mean absolute (linear) deviation
#' @description Weighted mean of the absolute deviations from the
#'   arithmetic mean; a more precise dispersion measure than the range.
#'   Formula: \eqn{\delta = \frac{\sum |x - \bar{x}| f}{\sum f}}
#' @param x Numeric vector of values (e.g. class midpoints).
#' @param f Numeric vector of frequencies/weights for each value. Defaults
#'   to 1 for every observation (unweighted case).
#' @return A single numeric value, in the same units as x.
#' @references Gatev K., Gateva N. Business Statistics, sec. 3.5.3, formula 3.30.
#' @examples
#' # Salaries at firm "N", October 2008 (table 3.4)
#' x <- c(520, 560, 600, 640, 680, 720, 760, 800)
#' f <- c(10, 19, 27, 38, 46, 35, 24, 12)
#' mean_abs_deviation(x, f) # 59.81
#' @export
mean_abs_deviation <- function(x, f = rep(1, length(x))) {
  check_lengths(x, f)
  xbar <- stats::weighted.mean(x, f)
  sum(abs(x - xbar) * f) / sum(f)
}

#' @title Coefficient of variation by mean absolute deviation
#' @description Formula: \eqn{V_\delta = \frac{\delta}{\bar{x}} \cdot 100}
#' @param x Numeric vector of values.
#' @param f Numeric vector of frequencies/weights.
#' @return A single numeric value (percent).
#' @references Gatev K., Gateva N. Business Statistics, formula 3.31.
#' @examples
#' x <- c(520, 560, 600, 640, 680, 720, 760, 800)
#' f <- c(10, 19, 27, 38, 46, 35, 24, 12)
#' coef_variation_delta(x, f) # ~9.0
#' @export
coef_variation_delta <- function(x, f = rep(1, length(x))) {
  xbar <- stats::weighted.mean(x, f)
  mean_abs_deviation(x, f) / xbar * 100
}

#' @title Weighted standard deviation and variance
#' @description The most precise and most widely used dispersion measure.
#'   Formula: \eqn{\sigma = \sqrt{\frac{\sum (x - \bar{x})^2 f}{\sum f}}}
#' @param x Numeric vector of values.
#' @param f Numeric vector of frequencies/weights.
#' @return A list with `sd` (standard deviation) and `variance`.
#' @references Gatev K., Gateva N. Business Statistics, sec. 3.5.4, formulas 3.33.
#' @examples
#' x <- c(520, 560, 600, 640, 680, 720, 760, 800)
#' f <- c(10, 19, 27, 38, 46, 35, 24, 12)
#' weighted_sd(x, f) # sd ~72.48, variance ~5253.36
#' @export
weighted_sd <- function(x, f = rep(1, length(x))) {
  check_lengths(x, f)
  xbar <- stats::weighted.mean(x, f)
  variance <- sum((x - xbar)^2 * f) / sum(f)
  list(sd = sqrt(variance), variance = variance)
}

#' @title Coefficient of variation by standard deviation
#' @description Formula: \eqn{V_\sigma = \frac{\sigma}{\bar{x}} \cdot 100}
#' @param x Numeric vector of values.
#' @param f Numeric vector of frequencies/weights.
#' @return A single numeric value (percent).
#' @references Gatev K., Gateva N. Business Statistics, formula 3.34.
#' @examples
#' x <- c(520, 560, 600, 640, 680, 720, 760, 800)
#' f <- c(10, 19, 27, 38, 46, 35, 24, 12)
#' coef_variation_sigma(x, f) # ~10.9
#' @export
coef_variation_sigma <- function(x, f = rep(1, length(x))) {
  xbar <- stats::weighted.mean(x, f)
  weighted_sd(x, f)$sd / xbar * 100
}

#' @title Standard deviation of a binary (alternative) attribute
#' @description For a dichotomous attribute coded 1/0 with share p having
#'   value 1 and share q = 1 - p having value 0.
#'   Formula: \eqn{\sigma_p = \sqrt{p(1-p)}}, maximum 0.5 at p = 0.5.
#' @param p Numeric, the share of units with value 1 (0 <= p <= 1).
#' @return A single numeric value between 0 and 0.5.
#' @references Gatev K., Gateva N. Business Statistics, sec. 3.5.5, formula 3.40.
#' @examples
#' binary_sd(0.3)
#' binary_sd(0.5) # maximum, 0.5
#' @export
binary_sd <- function(p) {
  if (p < 0 || p > 1) stop("p must be between 0 and 1.")
  sqrt(p * (1 - p))
}

#' @title Quartile deviation (Bowley's semi-interquartile range)
#' @description Half the distance between the first and third quartile;
#'   a robust dispersion measure less sensitive to extreme values than
#'   the range.
#'   Formula: \eqn{Q = \frac{Q_3 - Q_1}{2}}
#' @param x Numeric vector of observations.
#' @param type Quantile algorithm passed to `stats::quantile()` (default 7).
#' @return A single numeric value.
#' @references Gatev K., Gateva N. Business Statistics, sec. 3.5.6, formula 3.41.
#' @examples
#' quartile_deviation(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' @export
quartile_deviation <- function(x, type = 7) {
  qs <- stats::quantile(x, probs = c(0.25, 0.75), type = type, names = FALSE)
  (qs[2] - qs[1]) / 2
}

#' @title Coefficient of variation by quartile deviation
#' @description Formula: \eqn{V_Q = \frac{Q_3 - Q_1}{Q_1 + Q_3} \cdot 100}
#' @param x Numeric vector of observations.
#' @param type Quantile algorithm passed to `stats::quantile()` (default 7).
#' @return A single numeric value (percent).
#' @references Gatev K., Gateva N. Business Statistics, formula 3.42.
#' @examples
#' coef_variation_quartile(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' @export
coef_variation_quartile <- function(x, type = 7) {
  qs <- stats::quantile(x, probs = c(0.25, 0.75), type = type, names = FALSE)
  (qs[2] - qs[1]) / (qs[1] + qs[2]) * 100
}
