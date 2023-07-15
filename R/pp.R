#' @rdname PPQQ
#' @export
pppairs <- function(x, y, data, ngrid, a = -0.5, col_prefix = "prob") {
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
    cdfx <- stats::approxfun(xs, uxs, method = "constant", f = 0,
                             yright = uxs[nx], yleft = 0)
    qx <- stats::approxfun(c(-Inf, uxs), c(xs[1], xs), method = "constant",
                           f = 1, yright = xs[nx])
  } else {
    cdfx <- function(t) distionary::eval_cdf(x, at = t)
    qx <- function(p) distionary::eval_quantile(x, at = p)
    if (distionary::is_finite_dst(x)) {
      px <- x$probabilities$size
      uxs <- cumsum(px)
      xs <- x$probabilities$location
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
    cdfy <- stats::approxfun(ys, uys, method = "constant", f = 0,
                             yright = uys[ny], yleft = 0)
    qy <- stats::approxfun(c(-Inf, uys), c(ys[1], ys), method = "constant",
                           f = 1, yright = ys[ny])
  } else {
    cdfy <- function(t) distionary::eval_cdf(y, at = t)
    qy <- function(p) distionary::eval_quantile(y, at = p)
    if (distionary::is_finite_dst(y)) {
      py <- y$probabilities$size
      uys <- cumsum(py)
      ys <- y$probabilities$location
    }
  }
  if (!missing(ngrid)) {
    if (ngrid <= 0 || ngrid %% 1 != 0) {
      stop("`ngrid` must be a positive integer.")
    }
    tau <- uscore(1:ngrid, a = a)
    res <- data.frame(x = tau, y = cdfy(qx(tau)))
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
    uy_from_x <- cdfy(xs)
    ux_from_x <- uxs
  } else {
    uy_from_x <- numeric(0L)
    ux_from_x <- numeric(0L)
  }
  if (length(uys)) {
    ux_from_y <- cdfx(ys)
    uy_from_y <- uys
  } else {
    ux_from_y <- numeric(0L)
    uy_from_y <- numeric(0L)
  }
  res <- data.frame(x = c(ux_from_x, ux_from_y),
                    y = c(uy_from_x, uy_from_y))
  res <- stats::setNames(res, paste(col_prefix, c("x", "y"), sep = "_"))
  res <- res[!duplicated(res), , drop = FALSE]
  res <- res[order(res[[1]], res[[2]]), , drop = FALSE]
  rownames(res) <- NULL
  if (requireNamespace("tibble", quietly = TRUE)) {
    res <- tibble::as_tibble(res)
  }
  return(res)
}

