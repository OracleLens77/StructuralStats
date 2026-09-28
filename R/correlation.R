#' @title Spearman rank correlation
#' @description Measures the strength of association between two ranked
#'   variables; used here to compare the similarity of a structure across
#'   two periods.
#' @param x Numeric vector of ranks or values.
#' @param y Numeric vector of ranks or values.
#' @return A single numeric value between -1 and 1.
#' @references Afanasyev V.N. Statistical methods..., 2019, formula 19.
#' @examples
#' spearman_correlation(c(1, 2, 3, 4), c(1, 3, 2, 4))
#' @export
spearman_correlation <- function(x, y) {
  stats::cor(x, y, method = "spearman")
}

#' @title Kendall's coefficient of concordance
#' @description Measures agreement across multiple rankings (e.g. several
#'   experts or several periods ranking the same set of objects).
#' @param ratings A matrix where rows are objects and columns are
#'   experts/periods.
#' @return A single numeric value between 0 and 1.
#' @examples
#' m <- matrix(c(1, 2, 3, 1, 3, 2, 2, 1, 3), nrow = 3)
#' kendall_concordance(m)
#' @export
kendall_concordance <- function(ratings) {
  if (!is.matrix(ratings)) stop("ratings must be a matrix.")
  k <- ncol(ratings)
  n <- nrow(ratings)

  r_i <- rowSums(ratings)
  r_mean <- mean(r_i)
  s <- sum((r_i - r_mean)^2)

  (12 * s) / (k^2 * (n^3 - n))
}
