#' Quantile-Quantile Points
#'
#' Evaluates quantiles on a grid of uniform scores.
#'
#' @param ... Numeric vectors (column names when `data` is specified), or
#' distplyr distributions.
#' @param data Optional data frame specifying where to take the numeric vectors
#' from.
#' @param ngrid Number of uniform scores to evaluate the quantiles at.
#' @param a Uniform score adjustment value, passed to `uscore()` when
#' determining the uniform grid to evaluate the quantiles on.
#' @param col_prefix Prefix for output column names.
#' @return A tibble of paired quantiles, each row corresponding to a
#' uniform score (non-exceedance probability).
#' @examples
#' set.seed(1)
#' qqpoints(rnorm(10), rnorm(10))
#' qqpoints(rnorm(10), dst_norm(0, 1))
#' qqpoints(distionary::dst_t(5), distionary::dst_norm(0, 1), ngrid = 20)
#' @export
qqpoints <- function(..., data, ngrid, a = -0.5, col_prefix = "quantile") {
  dots <- rlang::enquos(...)
  if (missing(data)) data <- NULL
  dots <- lapply(dots, rlang::eval_tidy, data = data)
  numeric <- vapply(dots, is.vector, FUN.VALUE = logical(1L))
  dots[numeric] <- lapply(dots[numeric], distionary::dst_empirical) # Needs a-adjustment?
  distributions <- vapply(dots, distionary::is_distribution,
                          FUN.VALUE = logical(1L))
  finites <- vapply(dots, distionary::is_finite_dst, FUN.VALUE = logical(1L)) # What if they have a different a-adjustment?
  if (missing(ngrid)) {
    lengths <- vapply(dots[finites], distionary::num_discretes, # Need to fix num_discretes
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
