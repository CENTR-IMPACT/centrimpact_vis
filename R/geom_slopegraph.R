#' @importFrom ggplot2 ggproto GeomLine GeomPoint aes
#' @importFrom rlang %||%
#' @importFrom grid gList
#' @importFrom scales alpha
#' @rdname geom_slopegraph
#' @export
GeomSlopegraph <- ggproto("GeomSlopegraph", ggplot2::Geom,
  required_aes = c("x", "y", "group"),
  default_aes = aes(
    colour = "grey30",
    size = 2,
    alpha = 1,
    linewidth = 0.5,
    fill = NA
  ),
  draw_panel = function(self, data, panel_params, coord, na.rm = FALSE) {
    # Remove rows with missing values
    data <- data[stats::complete.cases(data[c("x", "y", "group")]), ]
    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    # Split data by group for line drawing
    groups <- split(data, data$group)

    # Create line grobs for each group
    line_grobs <- lapply(groups, function(group_data) {
      coords <- coord$transform(group_data, panel_params)
      if (nrow(coords) < 2) {
        return(NULL)
      }

      grid::polylineGrob(
        x = coords$x,
        y = coords$y,
        default.units = "npc",
        gp = grid::gpar(
          col = scales::alpha(coords$colour[1], coords$alpha[1]),
          lwd = (coords$linewidth[1] %||% 0.5) * .pt,
          lty = coords$linetype[1],
          lineend = "butt",
          linejoin = "round",
          linemitre = 10
        )
      )
    })

    # Create point grobs
    point_grobs <- lapply(groups, function(group_data) {
      coords <- coord$transform(group_data, panel_params)
      
      # If fill is not specified (is NA), use the line color instead
      point_fill <- ifelse(is.na(coords$fill), coords$colour, coords$fill)
      
      grid::pointsGrob(
        x = coords$x,
        y = coords$y,
        pch = 21,
        gp = grid::gpar(
          col = scales::alpha(coords$colour, coords$alpha),
          fill = scales::alpha(point_fill, coords$alpha), # Use the corrected fill
          fontsize = coords$size * .pt,
          lwd = (coords$linewidth %||% 0.5) * .pt / 2
        )
      )
    })

    # Combine all grobs
    grid::gTree(children = do.call(grid::gList, c(Filter(Negate(is.null), line_grobs), point_grobs)))
  },
  draw_key = ggplot2::draw_key_point,

  # Required for ggplot2 to recognize linewidth as a valid aesthetic
  rename_size = TRUE
)

#' Slope Graph (Bump Chart)
#'
#' Creates a slope graph (also known as a bump chart) that shows changes across
#' categories or time points. Slope graphs are particularly effective for
#' visualizing rankings or values across a small number of time points or
#' categories.
#'
#' @inheritParams ggplot2::geom_point
#' @param na.rm If `FALSE`, the default, missing values are removed with a warning.
#'   If `TRUE`, missing values are silently removed.
#' @param ... Additional parameters passed to [ggplot2::layer()].
#'
#' @section Aesthetics:
#' `geom_slopegraph()` understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#'   \item **`x`**: The x-axis position (typically a categorical variable)
#'   \item **`y`**: The y-axis position (typically a numeric value)
#'   \item **`group`**: The grouping variable that connects points
#'   \item `colour`: Line and point color
#'   \item `fill`: Point fill color (for points with shapes 21-25)
#'   \item `size`: Point size
#'   \item `alpha`: Alpha level for transparency
#'   \item `linewidth`: Width of the connecting lines
#'   \item `linetype`: Type of the connecting lines
#' }
#'
#' @seealso [ggplot2::geom_line()], [ggplot2::geom_point()]
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # Basic example with two time points
#' df <- data.frame(
#'   category = rep(letters[1:5], 2),
#'   time = rep(c("Before", "After"), each = 5),
#'   value = c(1:5, 5:1),
#'   color = rep(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2"), 2)
#' )
#'
#' ggplot(df, aes(x = time, y = value, group = category, color = color)) +
#'   geom_slopegraph(size = 3, linewidth = 1) +
#'   scale_color_identity() +
#'   theme_minimal()
#'
#' # Example with multiple time points
#' df_multi <- data.frame(
#'   category = rep(letters[1:3], each = 3),
#'   time = rep(1:3, 3),
#'   value = c(1, 2, 3, 3, 2, 1, 2, 1, 2)
#' )
#'
#' ggplot(df_multi, aes(x = time, y = value, group = category, color = category)) +
#'   geom_slopegraph(size = 3, linewidth = 1) +
#'   scale_x_continuous(breaks = 1:3, labels = c("Start", "Middle", "End")) +
#'   theme_minimal()
geom_slopegraph <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSlopegraph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
