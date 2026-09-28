#' @title Oscillation coefficient of absolute structural shifts
#' @description Relates the average absolute shift to the average of the
#'   absolute shifts themselves — a supplementary measure alongside the
#'   Kazinets coefficients.
#'   Formula: \eqn{w = \frac{\sum ||d_1 - d_0| - \overline{|d_1-d_0|}||}
#'   {n \cdot \overline{|d_1-d_0|}}}
#' @param d_base Numeric vector of shares in the base period.
#' @param d_current Numeric vector of shares in the current period.
#' @return A single numeric value.
#' @references Afanasyev V.N. Statistical methods..., 2019, formula 10.
#' @note When the structure has only two elements, |d1 - d0| is identical
#'   for both, so this coefficient is always 0 (Afanasyev, 2019, notes on
#'   formulas 10-11). Use a structure with three or more elements to see
#'   a non-zero result.
#' @examples
#' oscillation_coef(c(0.3, 0.4, 0.3), c(0.5, 0.3, 0.2)) # ~0.33
#' oscillation_coef(c(0.58, 0.42), c(0.83, 0.17)) # 0 (two-element case)
#' @export
oscillation_coef <- function(d_base, d_current) {
  check_inputs(d_base, d_current)
  abs_diff <- abs(d_current - d_base)
  mean_abs_diff <- mean(abs_diff)
  if (mean_abs_diff == 0) return(0)
  sum(abs(abs_diff - mean_abs_diff)) / (length(abs_diff) * mean_abs_diff)
}

#' @title Coefficient of variation of absolute structural shifts
#' @description The standard-deviation analogue of `oscillation_coef()`.
#'   Formula: \eqn{v = \sqrt{\frac{1}{n}\sum(|d_1-d_0| -
#'   \overline{|d_1-d_0|})^2}\big/\overline{|d_1-d_0|}}
#' @param d_base Numeric vector of shares in the base period.
#' @param d_current Numeric vector of shares in the current period.
#' @return A single numeric value.
#' @references Afanasyev V.N. Statistical methods..., 2019, formula 11.
#' @note Also identically 0 for a two-element structure; see
#'   `oscillation_coef()`.
#' @examples
#' variation_coef_shifts(c(0.3, 0.4, 0.3), c(0.5, 0.3, 0.2)) # ~0.35
#' @export
variation_coef_shifts <- function(d_base, d_current) {
  check_inputs(d_base, d_current)
  abs_diff <- abs(d_current - d_base)
  mean_abs_diff <- mean(abs_diff)
  if (mean_abs_diff == 0) return(0)
  sqrt(mean((abs_diff - mean_abs_diff)^2)) / mean_abs_diff
}

#' @title Linear measure of intensity of structural change
#' @description A relative indicator of how far the actual structural
#'   change is from its theoretical maximum.
#'   Formula: \eqn{I_L = \frac{\sum |d_1 - d_0|}{2}}
#'   (numerically identical to the Gatev index of difference I*S).
#' @param d_base Numeric vector of shares in the base period.
#' @param d_current Numeric vector of shares in the current period.
#' @return A numeric value between 0 and 1.
#' @references Afanasyev V.N. Statistical methods..., 2019, formula 15
#'   (after Agapova T.N.).
#' @examples
#' linear_intensity(c(0.58, 0.42), c(0.83, 0.17))
#' @export
linear_intensity <- function(d_base, d_current) {
  gatev_linear_norm(d_base, d_current)
}

#' @title Correlation measure of intensity of structural change
#' @description Derived from the rank correlation between the base- and
#'   current-period shares: the closer the ranks, the smaller the
#'   intensity of change.
#'   Formula: \eqn{I_K = \frac{1 - r}{2}}, where r is the Spearman
#'   correlation between `d_base` and `d_current`.
#' @param d_base Numeric vector of shares in the base period.
#' @param d_current Numeric vector of shares in the current period.
#' @return A numeric value between 0 and 1.
#' @references Afanasyev V.N. Statistical methods..., 2019, formula 18.
#' @examples
#' correlation_intensity(c(0.58, 0.42, 0.10), c(0.83, 0.17, 0.05))
#' @export
correlation_intensity <- function(d_base, d_current) {
  check_inputs(d_base, d_current)
  r <- stats::cor(d_base, d_current, method = "spearman")
  (1 - r) / 2
}

#' @title Seasonality coefficient
#' @description Summarizes the average absolute deviation of seasonal
#'   indices from 100%, across the sub-annual intervals of a time series
#'   (months or quarters).
#'   Formula: \eqn{K_{sez} = \frac{\sum |I_{sez} - 100|}{n_s}}
#' @param seasonal_indices Numeric vector of seasonal indices, expressed
#'   as percentages of the trend (100 = no seasonal effect).
#' @return A single numeric value (percentage points).
#' @references Afanasyev V.N. Statistical methods..., 2019, formula 28.
#' @examples
#' seasonality_coef(c(85, 95, 110, 130, 105, 90, 88, 92, 98, 115, 120, 72))
#' @export
seasonality_coef <- function(seasonal_indices) {
  sum(abs(seasonal_indices - 100)) / length(seasonal_indices)
}
