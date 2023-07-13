#' Probability-Probability Points
#'
#' Evaluates non-exceedance probabilities (cumulative distribution function)
#' on a grid of uniform scores.
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
#' @note Code is a little hacky right now.
#' @examples
#' set.seed(1)
#' qqpoints(rnorm(10), rnorm(10))
#' qqpoints(rnorm(10), dst_norm(0, 1))
#' qqpoints(distionary::dst_t(5), distionary::dst_norm(0, 1), ngrid = 20)
#' @export
pppoints <- function(..., data, ngrid, a = -0.5, col_prefix = "prob") {
	dots <- rlang::enquos(...)
	if (missing(data)) data <- NULL
	dots <- lapply(dots, rlang::eval_tidy, data = data)
	dots <- rlang::squash_if(dots, rlang::is_bare_list)
	numeric <- vapply(dots, is.vector, FUN.VALUE = logical(1L))
	dots[numeric] <- lapply(dots[numeric], distionary::dst_empirical)
	distributions <- vapply(dots, distionary::is_distribution,
							FUN.VALUE = logical(1L))
	finites <- vapply(dots, distionary::is_finite_dst, FUN.VALUE = logical(1L)) # What if the provided finites have a different a-adjustment?
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
	res <- distionary::enframe_cdf(!!!dots, at = tau, fn_prefix = col_prefix)
	res$.arg <- NULL
	res <- as.data.frame(lapply(res, uscore))
	if (rlang::is_installed("tibble")) {
	  tibble::as_tibble(res)
	} else {
	  res
	}
}
