#' Stamen Layer for Rose Plots
#'
#' Adds stamen-like elements (segments with points) to a rose plot, typically used to show
#' dimension values. The stamen is the pollen-producing part of a flower, which in this
#' visualization extends from the center to the petal values.
#'
#' @inheritParams ggplot2::geom_segment
#' @param linewidth Line width for the stamen bars.
#' @param point.size Size of the anther (end point of the stamen).
#' @param inner.radius Inner radius for the stamen plot.
#' @param na.rm If FALSE (the default), missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#' @param ... Other arguments passed to `layer()`.
#'
#' @importFrom ggplot2 layer
#' @export
