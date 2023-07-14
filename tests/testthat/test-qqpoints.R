test_that("QQ points works with two numeric.", {
  ## Default `a`
  res <- as.data.frame(qqpoints(1:5, 1:18))
  check <- data.frame(
    quantile_x = c(1, 1,
                   2, 2, 2,
                   3, 3, 3, 3,
                   4, 4, 4, 4,
                   5, 5, 5, 5, 5,
                   1:5),
    quantile_y = c(1:18, 3, 6, 10, 14, 17)
  )
  check <- check[-17, ] # duplicate
  check <- check[order(check[[1]], check[[2]]), ]
  rownames(check) <- NULL
  expect_equal(res, check)
  ## Same, this time with data in a data frame.
  dat <- data.frame(x = 1:5)
  res <- as.data.frame(qqpoints(x, 1:18, data = dat))
  expect_equal(res, check)
  dat <- data.frame(y = 1:18)
  res <- as.data.frame(qqpoints(1:5, y, data = dat))
  expect_equal(res, check)
  ## Try a different `a`
  res <- as.data.frame(qqpoints(1:3, 1:4, a = 1.5))
  check <- data.frame(
    quantile_x = c(1, 1, 2, 2, 3, 3),
    quantile_y = c(1, 2, 2, 3, 3, 4)
  )
  expect_equal(res, check)
  ## Try specifying ngrid
  res <- as.data.frame(qqpoints(1:3, 1:4, ngrid = 5, a = 1.5))
  check <- data.frame(
    quantile_x = c(1, 2, 2, 3),
    quantile_y = c(1, 2, 3, 4)
  )
  expect_equal(res, check)
})

test_that("QQ points errors out appropriately.", {
  expect_error(qqpoints(data.frame(x = 1), 4))
  expect_error(qqpoints(distionary::dst_norm(0, 1), data.frame(x = 1)))
  expect_error(qqpoints(distionary::dst_norm(0, 1),
                        distionary::dst_exp(1)))
})

test_that("QQ points returns a zero-row data frame when appropriate.", {
  expect_equal(nrow(qqpoints(numeric(0), 1:10)), 0)
  expect_equal(nrow(qqpoints(numeric(0), 1:10, ngrid = 10)), 0)
  expect_equal(nrow(qqpoints(1:10, numeric(0))), 0)
  expect_equal(nrow(qqpoints(1:10, numeric(0), ngrid = 10)), 0)
})

test_that("QQ points works with two distributions.", {
  res <- as.data.frame(qqpoints(distionary::dst_norm(0, 1),
                                distionary::dst_exp(1), ngrid = 5))
  u <- uscore(1:5)
  check <- data.frame(quantile_x = stats::qnorm(u), quantile_y = qexp(u))
  expect_equal(res, check)
})

test_that("QQ points works with one numeric, one distribution.", {
  res <- as.data.frame(qqpoints(1:5, distionary::dst_exp(1)))
  check <- data.frame(quantile_x = 1:5,
                      quantile_y = stats::qexp(uscore(1:5)))
  expect_equal(res, check)
  res <- as.data.frame(qqpoints(distionary::dst_exp(1), 1:5))
  check <- data.frame(quantile_x = stats::qexp(uscore(1:5)),
                      quantile_y = 1:5)
  expect_equal(res, check)
})

test_that("Infinite quantiles are removed.", {
  res <- as.data.frame(qqpoints(distionary::dst_norm(0, 1),
                                distionary::dst_unif(0, 1),
                                ngrid = 5, a = -1))
  check <- data.frame(quantile_x = stats::qnorm(1:3 / 4),
                      quantile_y = 1:3 / 4)
  expect_equal(res, check)
})
