# Define .pt as in ggplot2 (points per mm)
.pt <- 72.27 / 25.4

#' Cascade Spiral and Bar Geom
#'
#' A custom ggplot2 geom for drawing a combination of a spiral (polyline) and a bar (segment), useful for cascade or spiral visualizations.
#'
#' @param mapping Set of aesthetic mappings created by `aes()` or `aes_()`. If specified and `inherit.aes = TRUE` (the default), it is combined with the default mapping at the top level of the plot. You must supply `mapping` if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options: If `NULL`, the default, the data is inherited from the plot data as specified in the call to `ggplot`.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param ... Other arguments passed on to `layer()`. These are often aesthetics, used to set an aesthetic to a fixed value, like `color = 'red'` or `size = 3`. They may also be parameters to the paired stat.
#' @param na.rm If `FALSE` (the default), removes missing values with a warning. If `TRUE` silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends? `NA`, the default, includes if any aesthetics are mapped. `FALSE` never includes, and `TRUE` always includes.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification.
#'
#' @return A ggplot2 layer that can be added to a plot.
#' @export
#'
#' @examples
#' # Example usage (requires appropriate data with 'part' column)
#' # ggplot(data, aes(x, y, color = color, size = size, part = part)) +
#' #   geom_cascade_spiral()
#'
#' @seealso [ggplot2::layer], [ggplot2::Geom]
geom_cascade_spiral <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    geom = GeomCascadeSpiral, mapping = mapping, data = data,
    stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' @rdname geom_cascade_spiral
#' @format NULL
#' @usage NULL
GeomCascadeSpiral <- ggplot2::ggproto("GeomCascadeSpiral", ggplot2::Geom,
                                      required_aes = c("x", "y"),
                                      default_aes = ggplot2::aes(color = "black", size = 0.5, linetype = 1, alpha = 1),

                                      draw_panel = function(data, panel_params, coord) {
                                        # Split into spiral and bar data
                                        spiral_data <- data[data$part == "spiral", ]
                                        bar_data <- data[data$part == "bar", ]

                                        # Transform coordinates
                                        coords_spiral <- coord$transform(spiral_data, panel_params)
                                        coords_bar <- coord$transform(bar_data, panel_params)

                                        # Create grobs only if data is present
                                        spiral_grob <- if (nrow(coords_spiral) > 0) {
                                          grid::polylineGrob(
                                            x = coords_spiral$x,
                                            y = coords_spiral$y,
                                            gp = grid::gpar(col = coords_spiral$color[1], lwd = coords_spiral$size[1] * .pt)
                                          )
                                        } else {
                                          NULL
                                        }

                                        bar_grob <- if (nrow(coords_bar) > 1) {
                                          grid::segmentsGrob(
                                            x0 = coords_bar$x[1], x1 = coords_bar$x[2],
                                            y0 = coords_bar$y[1], y1 = coords_bar$y[2],
                                            gp = grid::gpar(col = coords_bar$color[1], lwd = coords_bar$size[1] * .pt)
                                          )
                                        } else {
                                          NULL
                                        }

                                        # Remove NULLs and return gList
                                        grobs <- list(spiral_grob, bar_grob)
                                        grobs <- grobs[!sapply(grobs, is.null)]
                                        do.call(grid::gList, grobs)
                                      }
)
