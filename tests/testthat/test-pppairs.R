test_that("PP points works with two numeric.", {
  ## Default `a`
  res <- as.data.frame(pppairs(1:5, 1:18))
  check <- data.frame(
    prob_x = c(uscore(1:5)[c(1:5, rep(5, 18 - 5))]),
    prob_y = uscore(1:18)
  )
  check <- check[order(check[[1]], check[[2]]), ]
  rownames(check) <- NULL
  expect_equal(res, check)
  ## Same, this time with data in a data frame.
  dat <- data.frame(x = 1:5)
  res <- as.data.frame(pppairs(x, 1:18, data = dat))
  expect_equal(res, check)
  dat <- data.frame(y = 1:18)
  res <- as.data.frame(pppairs(1:5, y, data = dat))
  expect_equal(res, check)
  ## Try a different `a`
  res <- as.data.frame(pppairs(1:3, 1:4, a = 1.5))
  check <- data.frame(
    prob_x = c(uscore(1:3, a = 1.5), uscore(1:3, a = 1.5)[3]),
    prob_y = c(uscore(1:4, a = 1.5)[1:3], uscore(1:4, a = 1.5)[4])
  )
  expect_equal(res, check)
  ## Try specifying ngrid
  res <- as.data.frame(pppairs(1:3, 1:4, ngrid = 5, a = 1.5))
  check <- data.frame(
    prob_x = uscore(1:5, a = 1.5),
    prob_y = uscore(1:4, a = 1.5)[c(1, 2, 2, 3, 3)]
  )
  expect_equal(res, check)
})

test_that("Duplicate data still works.", {
  res <- as.data.frame(pppairs(c(2, 2, 3.5), 1:4, a = 1.5))
  check <- data.frame(
    prob_x = c(0, 7, 7, 9, 9) / 14,
    prob_y = c(5, 7, 9, 9, 11) / 16
  )
  expect_equal(res, check)
})

test_that("PP points errors out appropriately.", {
  expect_error(pppairs(data.frame(x = 1), 4))
  expect_error(pppairs(distionary::dst_norm(0, 1), data.frame(x = 1)))
  expect_error(pppairs(distionary::dst_norm(0, 1),
                        distionary::dst_exp(1)))
})

test_that("PP points returns a zero-row data frame when appropriate.", {
  expect_equal(nrow(pppairs(numeric(0), 1:10)), 0)
  expect_equal(nrow(pppairs(numeric(0), 1:10, ngrid = 10)), 0)
  expect_equal(nrow(pppairs(1:10, numeric(0))), 0)
  expect_equal(nrow(pppairs(1:10, numeric(0), ngrid = 10)), 0)
})

test_that("PP points works with two distributions.", {
  res <- as.data.frame(pppairs(distionary::dst_norm(0, 1),
                                distionary::dst_exp(1), ngrid = 5))
  u <- uscore(1:5)
  check <- data.frame(prob_x = u,
                      prob_y = stats::pexp(stats::qnorm(u)))
  expect_equal(res, check)
})

test_that("PP points works with one numeric, one distribution.", {
  res <- as.data.frame(pppairs(1:5, distionary::dst_exp(1)))
  check <- data.frame(prob_x = uscore(1:5),
                      prob_y = stats::pexp(1:5))
  expect_equal(res, check)
  res <- as.data.frame(pppairs(distionary::dst_exp(1), 1:5))
  check <- data.frame(prob_x = stats::pexp(1:5),
                      prob_y = uscore(1:5))
  expect_equal(res, check)
})

test_that("PP knows what to do when finite distributions are supplied.", {
  x <- distionary::dst_empirical(1:3, weights = c(0.1, 0.41, 0.49))
  y <- c(1.5, 2, 3)
  res <- as.data.frame(pppairs(x, y))
  check <- data.frame(
    prob_x = c(0.1, 0.1, 0.51, 1),
    prob_y = c(0,   uscore(1:3))
  )
  expect_equal(res, check)
  y <- distionary::dst_norm(0, 1)
  res <- as.data.frame(pppairs(x, y))
  check <- data.frame(
    prob_x = c(0.1, 0.51, 1),
    prob_y = stats::pnorm(1:3)
  )
  expect_equal(res, check)
})
