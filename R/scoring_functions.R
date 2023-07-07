#' Rank-based Scores
#'
#' Converts a numeric vector to its rank-based scores. For `uscore()`
#' (uniform scores), values become roughly equally spaced between 0 and 1,
#' keeping their order. `nscore()` calculates normal scores by
#' spacing the uniform scores along a standard
#' normal distribution; `rpscore()` calculates empirical return periods
#' by spacing the uniform scores `u` by `1 / (1 - u)`.
#'
#' @param x Numeric vector.
#' @param a Single numeric; adjustment value for uniform scores. See Details.
#' @param na.rm Logical indicating whether `NA` and `NaN` values should be
#' removed from the output.
#' @details Uniform scores are calculated by `(rank+a)/(n+1+2*a)`,
#' where `rank` is the ranked `x` values.
#' @return Vector of uniform scores.
#' @author Thanks to Dr. Harry Joe for providing a starting framework for
#' the `uscore()` function.
#' @examples
#' x <- c(0.3, 0.56, NA, 0.1, -12)
#' uscore(x)
#' nscore(x)
#' rpscore(x)
#'
#' @export
#' @rdname scores
uscore <- function(x, a = -0.5, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  nr <- sum(!is.na(x))
  us <- (seq_len(nr) + a) / (nr + 1 + 2 * a)
  jj <- rank(x, na.last = "keep")
  us[jj]
}

#' @export
#' @rdname scores
nscore <- function(x, a = -0.5, na.rm = FALSE) {
  stats::qnorm(uscore(x, a = a, na.rm = na.rm))
}

#' @export
#' @rdname scores
rpscore <- function(x, a = -0.5, na.rm = FALSE) {
  1 / (1 - uscore(x, a = a, na.rm = na.rm))
}
