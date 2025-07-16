#' Rose plot
#'
#' A polar bar chart that displays data in a circular layout.
#'
#' @inheritParams ggplot2::geom_bar
#' @param inner.radius Numeric value between 0 and 1 specifying the inner radius of the rose plot
#' @param expand If `FALSE`, the default, removes padding at the plot boundaries
#' @param color Color of the bar borders
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param linewidth Line width for the rose bars.
#' @param ... Other arguments passed to `layer()`
#'
#' @importFrom ggplot2 layer GeomBar aes Geom
#' @importFrom ggplot2 ggproto draw_key_rect draw_key_point
#' @importFrom grid gpar segmentsGrob pointsGrob gList unit gTree gList
#' @importFrom scales alpha
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom magrittr %>%
#' @export
geom_rose <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "stack",
                      ...,
                      width = 1,
                      linewidth = 0.25,
                      color = NA,
                      expand = FALSE,
                      inner.radius = 0,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  if (inner.radius < 0 || inner.radius >= 1) {
    stop("inner.radius must be between 0 and 1")
  }

  params <- list(
    width = width,
    linewidth = linewidth,
    color = color,
    expand = expand,
    inner.radius = inner.radius,
    na.rm = na.rm,
    ...
  )

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRose,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
# @noRd
GeomRose <- ggproto("GeomRose", GeomBar,
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA,
    fill = NA
  ),
  draw_panel = function(self, data, panel_params, coord, width = 1,
                        linewidth = 0.25, color = NA, expand = FALSE,
                        inner.radius = 0) {
    # Ensure data is in the same order as the factor levels
    if (is.factor(data$x)) {
      data <- data[order(data$x), ]
    }

    # Transform the data
    data <- coord$transform(data, panel_params)

    # Group data by fill (domain) to ensure consistent heights within domains
    data <- data %>%
      group_by(fill) %>%
      mutate(y = max(y)) %>%
      ungroup()

    # Calculate bar positions and widths
    n <- nrow(data)
    angle_width <- 2 * pi / n # Full circle divided by number of bars

    # Scale factor to fit within the plot area (0-1 scale)
    max_radius <- 0.4 # Max radius as fraction of plot area

    # Create rectangles for each bar
    grobs <- lapply(seq_len(n), function(i) {
      # Calculate angles for this bar (no gap between bars)
      start_angle <- (i - 1) * angle_width - (pi / 2) # Start from top (12 o'clock)
      end_angle <- i * angle_width - (pi / 2)

      # Scale y to fit within the plot area (0-1 scale)
      y_scaled <- inner.radius + (data$y[i] * (max_radius - inner.radius))

      # Create points for the bar polygon
      t <- seq(start_angle, end_angle, length.out = 20)
      x <- c(inner.radius * cos(t), rev(y_scaled * cos(t)))
      y <- c(inner.radius * sin(t), rev(y_scaled * sin(t)))

      # Use fill color for the border to remove white edges
      border_color <- if (!is.na(color) && color != "white") color else data$fill[i]

      # Create polygon grob for the bar
      grid::polygonGrob(
        x = 0.5 + x, # Center at 0.5, 0.5
        y = 0.5 + y, # Center at 0.5, 0.5
        default.units = "npc",
        gp = grid::gpar(
          col = border_color, # Use fill color for border
          fill = scales::alpha(data$fill[i], data$alpha[i] %||% NA),
          lwd = (data$linewidth[i] %||% 0.5) * .pt * linewidth,
          lty = data$linetype[i],
          linejoin = "mitre"
        )
      )
    })

    # Combine all grobs
    grid::grobTree(children = do.call(grid::gList, grobs))
  },
  draw_key = draw_key_rect
)

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
geom_stamen <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        linewidth = 1,
                        point.size = 3,
                        inner.radius = 0,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStamen,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linewidth = linewidth,
      point.size = point.size,
      inner.radius = inner.radius,
      na.rm = na.rm,
      ...
    )
  )
}

# @rdname ggplot2-ggproto
# @format NULL
# @usage NULL
# @noRd
GeomStamen <- ggproto("GeomStamen", Geom,
  required_aes = c("x", "y", "xend", "yend", "colour"),
  default_aes = aes(
    colour = "black",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA,
    fill = NA
  ),
  draw_panel = function(self, data, panel_params, coord, linewidth = 1, point.size = 3, inner.radius = 0) {
    # Ensure data is in the same order as the factor levels
    if (is.factor(data$x)) {
      data <- data[order(data$x), ]
    }

    # Transform coordinates
    coords <- coord$transform(data, panel_params)

    # Constants that must match GeomRose
    max_radius <- 0.4 # Max radius as fraction of plot area

    # Calculate angles for each point (matching the rose plot)
    n <- nrow(coords)
    angle_width <- 2 * pi / n
    angles <- (seq_len(n) - 1) * angle_width - (pi / 2) # Start from top (12 o'clock)

    # Scale yend to fit within the plot area (0-1 scale)
    # Ensure we're using the same scaling as in GeomRose
    coords$yend_scaled <- inner.radius + (coords$yend * (max_radius - inner.radius))

    # Calculate start and end points for segments
    x0 <- 0.5 + inner.radius * cos(angles)
    y0 <- 0.5 + inner.radius * sin(angles)
    x1 <- 0.5 + coords$yend_scaled * cos(angles)
    y1 <- 0.5 + coords$yend_scaled * sin(angles)

    # Create segments grob (from inner radius to point)
    segments <- segmentsGrob(
      x0 = x0,
      y0 = y0,
      x1 = x1,
      y1 = y1,
      default.units = "npc",
      gp = gpar(
        col = alpha(coords$colour, coords$alpha %||% 1),
        lwd = (coords$linewidth %||% 0.5) * .pt * linewidth,
        lty = coords$linetype,
        lineend = "round"
      )
    )

    # Create points grob at the end positions
    points <- pointsGrob(
      x = x1,
      y = y1,
      pch = 16,
      size = unit(point.size, "mm"),
      default.units = "npc",
      gp = gpar(
        col = alpha(coords$colour, coords$alpha %||% 1),
        fill = alpha(coords$colour, coords$alpha %||% 1)
      )
    )

    # Return combined grobs
    gList(segments, points)
  },
  draw_key = draw_key_point
)
