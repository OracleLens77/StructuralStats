test_that("variation measures match the textbook salary example", {
  # Salaries at firm "N", October 2008 (Gatev & Gateva, ch. 3, table 3.4)
  x <- c(520, 560, 600, 640, 680, 720, 760, 800)
  f <- c(10, 19, 27, 38, 46, 35, 24, 12)

  expect_equal(round(mean_abs_deviation(x, f), 2), 59.81)
  sd_res <- weighted_sd(x, f)
  expect_equal(round(sd_res$sd, 2), 72.48)
  expect_equal(round(sd_res$variance, 2), 5253.29)
})

test_that("binary_sd peaks at p = 0.5", {
  expect_equal(binary_sd(0.5), 0.5)
  expect_lt(binary_sd(0.3), 0.5)
})

test_that("quartile_deviation is non-negative", {
  expect_gte(quartile_deviation(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), 0)
})
