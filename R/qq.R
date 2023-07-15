#' QQ and PP pairs
#'
#' Pairs quantiles (`qqpoints()`) and non-exceedance probabilities
#' (`pppairs()`)
#' on a grid of uniform scores, for the production of QQ and PP plots.
#'
#' @param x,y Numeric vectors or (univariate) probability distributions (of
#' class `"dst"`); when `data` is specified, these objects will be searched
#' for in the data first.
#' @param data Optional data frame specifying where to look for the `x` and `y`
#' objects.
#' @param ngrid Grid size to evaluate the pairs. Optional if `x` or `y` is
#' numeric or a `"finite"` distribution, in which case the grid is
#' made up of the discrete points. In the case of `pppairs()`, the grid
#' of uniform scores is associated with `x`.
#' @param a Uniform score adjustment value when determining non-exceedance
#' probabilities associated with `x` or `y`, if numeric; or, the grid
#' of uniform scores if `ngrid` is supplied. Passed to `uscore()`.
#' @param col_prefix Prefix for output column names.
#' @return A tibble of paired quantiles (`qqpairs()`) or
#' non-exceedance probabilities (`pppairs()`), the first column associated with
#' `x`, and the second with `y`.
#' @examples
#' set.seed(1)
#' qqpairs(rnorm(10), rnorm(10))
#' qqpairs(rnorm(10), distionary::dst_norm(0, 1))
#' qqpairs(distionary::dst_t(5), distionary::dst_norm(0, 1), ngrid = 20)
#' pppairs(rnorm(10), rnorm(10))
#' pppairs(rnorm(10), distionary::dst_norm(0, 1))
#' pppairs(distionary::dst_t(5), distionary::dst_norm(0, 1), ngrid = 20)
#' @rdname PPQQ
#' @export
qqpairs <- function(x, y, data, ngrid, a = -0.5, col_prefix = "quantile") {
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  if (missing(data)) data <- NULL
  x <- rlang::eval_tidy(x, data = data)
  y <- rlang::eval_tidy(y, data = data)
  x_is_dst <- distionary::is_distribution(x)
  y_is_dst <- distionary::is_distribution(y)
  x_is_num <- is.numeric(x)
  y_is_num <- is.numeric(y)
  uxs <- numeric(0L)
  uys <- numeric(0L)
  if (!x_is_dst && !x_is_num) {
    stop("`x` needs to be either numeric or a distribution; provided class ",
         class(x), ".")
  }
  if (!y_is_dst && !y_is_num) {
    stop("`y` needs to be either numeric or a distribution; provided class ",
         class(y), ".")
  }
  if (x_is_num) {
    ux <- uscore(x, a = a, na.rm = TRUE, ties.method = "max")
    uxs <- sort(ux)
    xs <- sort(x, na.last = NA)
    xdup <- duplicated(uxs)
    uxs <- uxs[!xdup]
    xs <- xs[!xdup]
    nx <- length(xs)
    if (nx == 0) {
      res <- as.data.frame(matrix(nrow = 0, ncol = 2))
      res <- stats::setNames(res, paste(col_prefix, c("x", "y"), sep = "_"))
      if (requireNamespace("tibble", quietly = TRUE)) {
        res <- tibble::as_tibble(res)
      }
      return(res)
    }
    qx <- stats::approxfun(c(-Inf, uxs), c(xs[1], xs), method = "constant",
                           f = 1, yright = xs[nx])
  } else {
    qx <- function(p) distionary::eval_quantile(x, at = p)
    if (distionary::is_finite_dst(x)) {
      px <- x$probabilities$size
      uxs <- cumsum(px)
      xs <- qx(uxs)
    }
  }
  if (y_is_num) {
    uy <- uscore(y, a = a, na.rm = TRUE, ties.method = "max")
    uys <- sort(uy)
    ys <- sort(y, na.last = NA)
    ydup <- duplicated(uys)
    uys <- uys[!ydup]
    ys <- ys[!ydup]
    ny <- length(ys)
    if (ny == 0) {
      res <- as.data.frame(matrix(nrow = 0, ncol = 2))
      res <- stats::setNames(res, paste(col_prefix, c("x", "y"), sep = "_"))
      if (requireNamespace("tibble", quietly = TRUE)) {
        res <- tibble::as_tibble(res)
      }
      return(res)
    }
    qy <- stats::approxfun(c(-Inf, uys), c(ys[1], ys), method = "constant",
                           f = 1, yright = ys[ny])
  } else {
    qy <- function(p) distionary::eval_quantile(y, at = p)
    if (distionary::is_finite_dst(y)) {
      py <- y$probabilities$size
      uys <- cumsum(py)
      ys <- qy(uys)
    }
  }
  if (!missing(ngrid)) {
    if (ngrid <= 0 || ngrid %% 1 != 0) {
      stop("`ngrid` must be a positive integer.")
    }
    tau <- uscore(1:ngrid, a = a)
    res <- data.frame(x = qx(tau), y = qy(tau))
    is_inf <- is.infinite(res[[1]]) | is.infinite(res[[2]])
    res <- res[!is_inf, , drop = FALSE]
    res <- stats::setNames(res, paste(col_prefix, c("x", "y"), sep = "_"))
    res <- res[!duplicated(res), , drop = FALSE]
    rownames(res) <- NULL
    if (requireNamespace("tibble", quietly = TRUE)) {
      res <- tibble::as_tibble(res)
    }
    return(res)
  }
  if (length(uxs) + length(uys) == 0) {
    stop("Must specify `ngrid` when inputting two distributions.")
  }
  if (length(uxs)) {
    qy_from_x <- qy(uxs)
    qx_from_x <- xs
  } else {
    qy_from_x <- numeric(0L)
    qx_from_x <- numeric(0L)
  }
  if (length(uys)) {
    qx_from_y <- qx(uys)
    qy_from_y <- ys
  } else {
    qx_from_y <- numeric(0L)
    qy_from_y <- numeric(0L)
  }
  res <- data.frame(x = c(qx_from_x, qx_from_y),
                    y = c(qy_from_x, qy_from_y))
  res <- stats::setNames(res, paste(col_prefix, c("x", "y"), sep = "_"))
  is_inf <- is.infinite(res[[1]]) | is.infinite(res[[2]])
  res <- res[!is_inf, , drop = FALSE]
  res <- res[!duplicated(res), , drop = FALSE]
  res <- res[order(res[[1]], res[[2]]), , drop = FALSE]
  rownames(res) <- NULL
  if (requireNamespace("tibble", quietly = TRUE)) {
    res <- tibble::as_tibble(res)
  }
  return(res)
}

