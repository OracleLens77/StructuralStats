test_that("Gatev coefficients match the textbook worked example", {
  # Export structure of firm "N" by country group, 2002 vs 2007
  # (Gatev & Gateva, Business Statistics, ch. 9, table 9.1)
  v2002 <- c(0.30, 0.40, 0.20, 0.10)
  v2007 <- c(0.60, 0.20, 0.15, 0.05)

  expect_equal(round(gatev_linear_norm(v2002, v2007), 2), 0.30)
  expect_equal(round(gatev_quadratic_norm(v2002, v2007), 2), 0.26)
  expect_equal(round(gatev_integral(v2002, v2007), 2), 0.43)
})

test_that("identical structures give zero shift", {
  v <- c(0.25, 0.25, 0.25, 0.25)
  expect_equal(gatev_integral(v, v), 0)
  expect_equal(kazinetz_linear_abs(v, v), 0)
})

test_that("salai_index errors on zero shares", {
  expect_error(salai_index(c(0, 0.5, 0.5), c(0.2, 0.4, 0.4)))
})
