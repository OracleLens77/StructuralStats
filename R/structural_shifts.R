#' @title Linear coefficient of absolute structural shifts (Kazinets)
#' @description Measures the average absolute deviation of structure shares
#'   between two periods.
#'   Formula: \eqn{L = \frac{1}{n} \sum |d_{1i} - d_{0i}|}
#' @param d_base Numeric vector of shares in the base period.
#' @param d_current Numeric vector of shares in the current period.
#' @return A single numeric value.
#' @references Kazinets L.S. Growth rates and structural shifts in the
#'   economy. Moscow, 1981.
#' @examples
#' kazinetz_linear_abs(c(0.58, 0.42), c(0.83, 0.17))
#' @export
kazinetz_linear_abs <- function(d_base, d_current) {
  check_inputs(d_base, d_current)
  mean(abs(d_current - d_base))
}

#' @title Quadratic coefficient of absolute structural shifts (Kazinets)
#' @description A more sensitive measure based on squared deviations.
#'   Formula: \eqn{\sigma = \sqrt{\frac{1}{n} \sum (d_{1i} - d_{0i})^2}}
#' @param d_base Numeric vector of shares in the base period.
#' @param d_current Numeric vector of shares in the current period.
#' @return A single numeric value.
#' @references Kazinets L.S. Growth rates and structural shifts in the
#'   economy. Moscow, 1981.
#' @examples
#' kazinetz_quadratic_abs(c(0.58, 0.42), c(0.83, 0.17))
#' @export
kazinetz_quadratic_abs <- function(d_base, d_current) {
  check_inputs(d_base, d_current)
  sqrt(mean((d_current - d_base)^2))
}

#' @title Normalized linear coefficient of structural shifts (index of
#'   difference, Gatev)
#' @description Rescales the sum of absolute differences to the [0, 1]
#'   range, where 0 means identical structures and 1 means complete
#'   difference. Also known as the index of difference I*S.
#'   Formula: \eqn{I^*_S = \frac{1}{2} \sum |d_1 - d_0|}
#' @param d_base Numeric vector of shares in the base period (should sum to 1).
#' @param d_current Numeric vector of shares in the current period (should sum to 1).
#' @return A numeric value between 0 and 1.
#' @references Gatev K., Gateva N. Business Statistics, ch. 9.
#' @examples
#' # Worked example from Gatev & Gateva: export structure by country group
#' v2002 <- c(OECD = 0.30, CEE = 0.40, Arab = 0.20, Other = 0.10)
#' v2007 <- c(OECD = 0.60, CEE = 0.20, Arab = 0.15, Other = 0.05)
#' gatev_linear_norm(v2002, v2007) # 0.30
#' @export
gatev_linear_norm <- function(d_base, d_current) {
  check_inputs(d_base, d_current)
  sum(abs(d_current - d_base)) / 2
}

#' @title Normalized quadratic coefficient of absolute structural shifts
#'   (Kazinets, normalized by Gatev)
#' @description Formula: \eqn{\sigma^*_{\Delta v} = \sqrt{\frac{\sum
#'   (d_1 - d_0)^2}{2}}}
#' @param d_base Numeric vector of shares in the base period.
#' @param d_current Numeric vector of shares in the current period.
#' @return A numeric value between 0 and 1.
#' @references Gatev K., Gateva N. Business Statistics, ch. 9, formula 9.10.
#' @examples
#' v2002 <- c(0.30, 0.40, 0.20, 0.10)
#' v2007 <- c(0.60, 0.20, 0.15, 0.05)
#' gatev_quadratic_norm(v2002, v2007) # 0.26
#' @export
gatev_quadratic_norm <- function(d_base, d_current) {
  check_inputs(d_base, d_current)
  sqrt(sum((d_current - d_base)^2) / 2)
}

#' @title Integral coefficient of structural shifts (Gatev)
#' @description The preferred summary coefficient: it accounts not only
#'   for the differences between shares but also for their magnitude in
#'   both periods.
#'   Formula: \eqn{K_S = \sqrt{\frac{\sum (d_1 - d_0)^2}{\sum d_0^2 + \sum d_1^2}}}
#' @param d_base Numeric vector of shares in the base period.
#' @param d_current Numeric vector of shares in the current period.
#' @return A numeric value between 0 and 1.
#' @references Gatev K., Gateva N. Business Statistics, ch. 9, formulas 9.12-9.13.
#' @examples
#' v2002 <- c(0.30, 0.40, 0.20, 0.10)
#' v2007 <- c(0.60, 0.20, 0.15, 0.05)
#' gatev_integral(v2002, v2007) # 0.43
#' @export
gatev_integral <- function(d_base, d_current) {
  check_inputs(d_base, d_current)
  numerator <- sum((d_current - d_base)^2)
  denominator <- sum(d_current^2 + d_base^2)
  if (denominator == 0) return(NA_real_)
  sqrt(numerator / denominator)
}

#' @title Salai index of structural change
#' @description A relative measure that is highly sensitive to small
#'   shares; undefined when any share is zero.
#'   Formula: \eqn{I_S = \sqrt{\frac{1}{n} \sum \frac{(d_1 - d_0)^2}{d_1 d_0}}}
#' @param d_base Numeric vector of shares in the base period (all > 0).
#' @param d_current Numeric vector of shares in the current period (all > 0).
#' @return A single numeric value.
#' @references Salai A. Methods for measuring structural change.
#' @examples
#' salai_index(c(0.3, 0.4, 0.3), c(0.35, 0.35, 0.30))
#' @export
salai_index <- function(d_base, d_current) {
  check_inputs(d_base, d_current)
  if (any(d_base == 0) || any(d_current == 0)) {
    stop("The Salai index is undefined when any share equals zero.")
  }
  terms <- ((d_current - d_base)^2) / (d_current * d_base)
  sqrt(mean(terms))
}

#' @title Ryabtsev index of structural difference
#' @description A normalized index that avoids the zero-share problem of
#'   the Salai index.
#'   Formula: \eqn{I_R = \sqrt{\frac{\sum (d_1 - d_0)^2}{\sum (d_1 + d_0)^2}}}
#' @param d_base Numeric vector of shares in the base period.
#' @param d_current Numeric vector of shares in the current period.
#' @return A numeric value between 0 and 1.
#' @references Ryabtsev V.M. Statistical methods for studying structure.
#' @examples
#' ryabtsev_index(c(0.58, 0.42), c(0.83, 0.17))
#' @export
ryabtsev_index <- function(d_base, d_current) {
  check_inputs(d_base, d_current)
  numerator <- sum((d_current - d_base)^2)
  denominator <- sum((d_current + d_base)^2)
  if (denominator == 0) return(NA_real_)
  sqrt(numerator / denominator)
}
