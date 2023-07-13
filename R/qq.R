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
  dots <- lapply(dots, rlang::eval_tidy, data = data)
  dots <- rlang::squash_if(dots, rlang::is_bare_list)
  zero_entries <- vapply(dots, length, FUN.VALUE = integer(1L))
  dots[zero_entries] <- NULL
  if (length(dots) == 0) return(as.data.frame(matrix(nrow = 0, ncol = 0)))
  finites <- vapply(dots, distionary::is_finite_dst, FUN.VALUE = logical(1L))
  uvals_finites <- lapply(dots[finites], function(x) {
    u <- cumsum(x$probabilities)
    u[u != 1]
  })
  numeric <- vapply(dots, is.vector, FUN.VALUE = logical(1L))
  uvals_numerics <- lapply(dots[numeric], uscore, a = a, na.rm = TRUE)
  dots[numeric] <- lapply(dots[numeric], function(x) {
    xs <- sort(x, na.last = NA)
    u <- uscore(xs)
    u[length(u)] <- 1
    w <- c(u[1L], diff(u))
    distionary::dst_finite(xs, probs = w)
  })
  if (missing(ngrid)) {
    lengths <- vapply(dots[finites], function(x) nrow(x$probabilities), # Need to fix num_discretes
                      FUN.VALUE = integer(1L))
    if (length(lengths) == 0) {
      stop("Cannot determine grid from input distributions. ",
           "Must specify `ngrid` argument.")
    }
    ngrid <- Reduce(pracma::Lcm, lengths)
  }
  if (ngrid == 0) {
    return(as.data.frame(matrix(nrow = 0, ncol = length(dots))))
  }
  tau <- uscore(1:ngrid, a = a)
  res <- distionary::enframe_quantile(!!!dots, at = tau, fn_prefix = col_prefix)
  res$.arg <- NULL
  res
}
