#' @title Laspeyres price index
#' @description Aggregate price index weighted by base-period quantities.
#'   Formula: \eqn{I_L = \frac{\sum p_1 q_0}{\sum p_0 q_0}}
#' @param p0 Numeric vector of base-period prices.
#' @param p1 Numeric vector of current-period prices.
#' @param q0 Numeric vector of base-period quantities.
#' @return A single numeric value.
#' @examples
#' laspeyres_index(c(10, 20, 30), c(12, 25, 35), c(100, 50, 20))
#' @export
laspeyres_index <- function(p0, p1, q0) {
  check_lengths(p0, p1, q0)
  sum(p1 * q0) / sum(p0 * q0)
}

#' @title Paasche price index
#' @description Aggregate price index weighted by current-period
#'   quantities.
#'   Formula: \eqn{I_P = \frac{\sum p_1 q_1}{\sum p_0 q_1}}
#' @param p0 Numeric vector of base-period prices.
#' @param p1 Numeric vector of current-period prices.
#' @param q1 Numeric vector of current-period quantities.
#' @return A single numeric value.
#' @examples
#' paasche_index(c(10, 20, 30), c(12, 25, 35), c(110, 60, 15))
#' @export
paasche_index <- function(p0, p1, q1) {
  check_lengths(p0, p1, q1)
  sum(p1 * q1) / sum(p0 * q1)
}

#' @title Fisher "ideal" price index
#' @description Geometric mean of the Laspeyres and Paasche indices.
#'   Formula: \eqn{I_F = \sqrt{I_L \cdot I_P}}
#' @param p0 Numeric vector of base-period prices.
#' @param p1 Numeric vector of current-period prices.
#' @param q0 Numeric vector of base-period quantities.
#' @param q1 Numeric vector of current-period quantities.
#' @return A single numeric value.
#' @examples
#' fisher_index(c(10, 20, 30), c(12, 25, 35), c(100, 50, 20), c(110, 60, 15))
#' @export
fisher_index <- function(p0, p1, q0, q1) {
  il <- laspeyres_index(p0, p1, q0)
  ip <- paasche_index(p0, p1, q1)
  sqrt(il * ip)
}
