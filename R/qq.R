#' Quantile-Quantile Points
#'
#' Evaluates quantiles on a grid of uniform scores.
#'
#' @param x,y Numeric vectors or (univariate) probability distributions (of
#' class `"dst"`); when `data` is specified, these objects will be searched
#' for in the data first.
#' @param data Optional data frame specifying where to look for the `x` and `y`
#' objects.
#' @param ngrid Number of uniform scores to evaluate the quantiles at.
#' Optional if `x` or `y` is numeric.
#' @param a Uniform score adjustment value when determining non-exceedance
#' probabilities associated with `x` or `y`, if numeric. Passed to `uscore()`.
#' @param col_prefix Prefix for output column names.
#' @return A tibble of paired quantiles, each row corresponding to a
#' uniform score (non-exceedance probability).
#' @note Code is a little hacky right now.
#' @examples
#' set.seed(1)
#' qqpoints(rnorm(10), rnorm(10))
#' qqpoints(rnorm(10), dst_norm(0, 1))
#' qqpoints(distionary::dst_t(5), distionary::dst_norm(0, 1), ngrid = 20)
#' @export
qqpoints <- function(x, y, data, ngrid, a = -0.5, col_prefix = "quantile") {
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
    ux <- uscore(x, a = a, na.rm = TRUE)
    uxs <- sort(ux, na.last = NA)
    xs <- sort(x, na.last = NA)
    nx <- length(xs)
    if (nx == 0) {
      res <- as.data.frame(matrix(nrow = 0, ncol = 2))
      res <- setNames(res, paste(col_prefix, c("x", "y"), sep = "_"))
      if (requireNamespace("tibble", quietly = TRUE)) {
        res <- tibble::as_tibble(res)
      }
      return(res)
    }
    qx <- approxfun(c(-Inf, uxs), c(xs[1], xs), method = "constant",
                    f = 1, yright = xs[nx])
  } else {
    qx <- function(p) eval_quantile(x, at = p)
    if (distionary::is_finite_dst(x)) {
      px <- x$probabilities$size
      uxs <- cumsum(px)
      xs <- qx(uxs)
    }
  }
  if (y_is_num) {
    uy <- uscore(y, a = a, na.rm = TRUE)
    uys <- sort(uy, na.last = NA)
    ys <- sort(y, na.last = NA)
    ny <- length(ys)
    if (ny == 0) {
      res <- as.data.frame(matrix(nrow = 0, ncol = 2))
      res <- setNames(res, paste(col_prefix, c("x", "y"), sep = "_"))
      if (requireNamespace("tibble", quietly = TRUE)) {
        res <- tibble::as_tibble(res)
      }
      return(res)
    }
    qy <- approxfun(c(-Inf, uys), c(ys[1], ys), method = "constant",
                    f = 1, yright = ys[ny])
  } else {
    qy <- function(p) eval_quantile(y, at = p)
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
    res <- setNames(res, paste(col_prefix, c("x", "y"), sep = "_"))
    res <- res[!duplicated(res), , drop = FALSE]
    rownames(res) <- NULL
    if (requireNamespace("tibble", quietly = TRUE)) {
      res <- tibble::as_tibble(res)
    }
    return(res)
  }
  if (x_is_dst && y_is_dst) {
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
  res <- setNames(res, paste(col_prefix, c("x", "y"), sep = "_"))
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

