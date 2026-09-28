test_that("oscillation and variation coefficients are zero for two-element structures", {
  d0 <- c(0.58, 0.42)
  dt <- c(0.83, 0.17)
  expect_equal(round(oscillation_coef(d0, dt), 8), 0)
  expect_equal(round(variation_coef_shifts(d0, dt), 8), 0)
})

test_that("oscillation coefficient is non-zero for three-element structures", {
  d0 <- c(0.3, 0.4, 0.3)
  dt <- c(0.5, 0.3, 0.2)
  expect_gt(oscillation_coef(d0, dt), 0)
})

test_that("correlation_intensity is between 0 and 1", {
  v <- correlation_intensity(c(0.58, 0.42, 0.10), c(0.83, 0.17, 0.05))
  expect_gte(v, 0)
  expect_lte(v, 1)
})
