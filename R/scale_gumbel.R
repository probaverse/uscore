#' Gumbel transformations used for ggplot2 scales
gumbelRP_trans <- scales::trans_new(
  "gumbelRP",
  transform = function(x) -log(-log(1 - 1 / x)),
  inverse = function(x) 1 / (1 - exp(-exp(-x))),
  breaks = scales::breaks_log(),
  domain = c(1, Inf)
)

gumbelAEP_trans <- scales::trans_new(
  "gumbelAEP",
  transform = function(x) -log(-log(1 - x)),
  inverse = function(x) 1 - exp(-exp(-x)),
  breaks = scales::breaks_log(),
  domain = c(1e-100, 1)
)

#' Gumbel-transformed axes
#'
#' Transforms return period or AEP on either the x or y axis to be spaced
#' according to a reduced Gumbel variate, meant to be added as a layer to
#' a ggplot2 graphic.
#'
#' @param ... Arguments to pass to `scale_x_continuous` or `scale_y_continuous`
#' from ggplot2.
#' @param minor_breaks Minor grid lines; passed to `scale_x_continuous` or
#' `scale_y_continuous`.
#' @return The same output as `scale_x_continuous` or `scale_y_continuous`,
#' but with the appropriate gumbel spacing.
#' @rdname gumbel_spacing
#' @export
scale_x_gumbelRP <- function(..., minor_breaks = unlist(lapply(10^(-10:10), function(x) x * 1:10))) {
  ggplot2::scale_x_continuous(
    ...,
    minor_breaks = minor_breaks,
    trans = gumbelRP_trans)
}

#' @rdname gumbel_spacing
#' @export
scale_y_gumbelRP <- function(..., minor_breaks = unlist(lapply(10^(-10:10), function(x) x * 1:10))) {
  ggplot2::scale_y_continuous(
    ...,
    minor_breaks = minor_breaks,
    trans = gumbelRP_trans)
}

#' @rdname gumbel_spacing
#' @export
scale_x_gumbelAEP <- function(..., minor_breaks = unlist(lapply(10^(-10:10), function(x) x * 1:10))) {
  ggplot2::scale_x_continuous(
    ...,
    minor_breaks = minor_breaks,
    trans = gumbelAEP_trans)
}

#' @rdname gumbel_spacing
#' @export
scale_y_gumbelAEP <- function(..., minor_breaks = unlist(lapply(10^(-10:10), function(x) x * 1:10))) {
  ggplot2::scale_y_continuous(
    ...,
    minor_breaks = minor_breaks,
    trans = gumbelAEP_trans)
}
