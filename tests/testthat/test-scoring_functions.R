test_that("uscore calculates properly", {
  x <- c(0.3, 0.56, NA, NaN, 0.1, -12)
  expect_equal(uscore(x, pos = 0, na.rm = TRUE), c(0.6, 0.8, 0.4, 0.2))
  expect_equal(uscore(x, pos = 0), c(0.6, 0.8, NA, NA, 0.4, 0.2))
  x <- 1:5
  expect_equal(uscore(x, pos = 1), c(0.25, 0.375, 0.5, 0.625, 0.75))
})

test_that("nscore calculates properly", {
  x <- 1:4
  expect_equal(nscore(x, pos = 0), qnorm(1:4 / 5))
})

test_that("rpscore calculates properly", {
  x <- 1:4
  expect_equal(rpscore(x, pos = 0), 5 / 4:1)
})
