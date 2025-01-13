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
#' @param pos Positional adjustment for uniform scores. See Details.
#' Can be a single numeric, or could be named after one of the
#' proponents behind a choice of the numeric: "Weibull", "Beard",
#' "Gringorten", or "Hazen".
#' @param na.rm Logical indicating whether `NA` and `NaN` values should be
#' removed from the output.
#' @details Uniform scores are calculated by `(rank+a)/(n+1+2*a)`,
#' where `rank` is the ranked `x` values, and `a` is the positional
#' adjustment `pos`. Alternatively, could be named after an individual
#' associated with a choice of `a`:
#'
#' - Weibull (1939) proposed `a = 0`.
#' - Beard (1943) proposed `a = -0.31`.
#' - Gringorten (1963) proposed `a = -0.44`.
#' - Hazen (1914) proposed `a = -0.5`.
#'
#' @return Vector of uniform scores.
#' @author Thanks to Dr. Harry Joe for providing a starting framework for
#' the `uscore()` function.
#' @examples
#' x <- c(0.3, 0.56, NA, 0.1, -12)
#' uscore(x)
#' uscore(x, pos = "Gringorten")
#' nscore(x, pos = -0.4)
#' rpscore(x)
#'
#' @export
#' @rdname scores
uscore <- function(x, pos = "Hazen", na.rm = FALSE) {
  if (is.character(pos)) {
    poslow <- tolower(pos)
    a <- switch(
      poslow,
      weibull = 0,
      beard = -0.31,
      gringorten = 0.44,
      hazen = -0.5
    )
    if (is.null(a)) {
      stop("Did not recognize positional adjustment '", pos, "'.")
    }
  } else {
    a <- pos
  }
  if (na.rm) x <- x[!is.na(x)]
  nr <- sum(!is.na(x))
  us <- (seq_len(nr) + a) / (nr + 1 + 2 * a)
  jj <- rank(x, na.last = "keep")
  us[jj]
}

#' @export
#' @rdname scores
nscore <- function(x, pos = "Hazen", na.rm = FALSE) {
  stats::qnorm(uscore(x, pos = pos, na.rm = na.rm))
}

#' @export
#' @rdname scores
rpscore <- function(x, pos = "Hazen", na.rm = FALSE) {
  1 / (1 - uscore(x, pos = pos, na.rm = na.rm))
}
