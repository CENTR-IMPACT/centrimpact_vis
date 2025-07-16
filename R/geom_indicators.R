# Add this at the top of the file
utils::globalVariables(c(
  "group_by", "summarize", "na.omit"
))

# Helper function to calculate default size
calculate_default_size <- function(data) {
  if (is.null(data)) {
    return(18)
  }

  # If there's only one point, return default size
  if (nrow(data) <= 1) {
    return(18)
  }

  # Calculate minimum distance between points
  x_pos <- sort(unique(data$x))
  min_dist <- min(diff(x_pos), na.rm = TRUE)

  # Calculate max radius based on minimum distance
  # Using 1/4 of minimum distance as max radius (with some padding)
  max_radius <- min_dist * 0.2

  # Scale to a reasonable size for points
  max_radius * 100 # Scaling factor to get to reasonable point size
}

#' @importFrom ggplot2 layer GeomPoint geom_hline scale_size_area scale_y_continuous
#' @importFrom rlang .data
#' @importFrom dplyr group_by summarize
#' @importFrom stats na.omit
#' @importFrom scales rescale
#' @export
#' @rdname geom_indicators
#' @usage NULL
#' @format NULL
#' @export
GeomIndicators <- ggplot2::ggproto("GeomIndicators", ggplot2::Geom,
  required_aes = c("x", "r"),
  default_aes = ggplot2::aes(
    colour = "grey30",
    fill = "grey80",
    alpha = NA,
    shape = 21,
    stroke = 0.5,
    y = 0.5,
    size = 3
  ),
  draw_panel = function(self, data, panel_params, coord, scale_method = "sqrt", scale_factor = NULL, 
                        show_reference_lines = TRUE, show_reference_circles = TRUE, na.rm = FALSE) {
    # Remove missing values
    data <- data[stats::complete.cases(data[, c("x", "r", "y")]), ]
    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }
    
    # Normalize r values to a reasonable range (1-10)
    if (length(unique(data$r)) > 1) {  # Only normalize if there's variation
      data$r_norm <- scales::rescale(data$r, to = c(1, 10))
    } else {
      data$r_norm <- data$r
    }

    # Calculate scale_factor if not provided
    if (is.null(scale_factor)) {
      scale_factor <- calculate_default_size(data)
    }

    # Transform data
    coords <- coord$transform(data, panel_params)
    
    # Create a copy of coords with normalized r values
    coords_norm <- coords
    coords_norm$r <- coords_norm$r_norm

    grobs <- list()

    # Reference line
    if (show_reference_lines) {
      y_line <- unique(coords$y)
      if (length(y_line) == 1) {
        grobs[[length(grobs) + 1]] <- grid::linesGrob(
          x = c(0, 1),
          y = c(y_line, y_line),
          gp = grid::gpar(col = "#E0E0E0", lwd = 0.5)
        )
      }
    }

    # Reference circles - using normalized values
    if (show_reference_circles) {
      ref_data <- stats::aggregate(r_norm ~ x, data = coords, max)
      ref_data$y <- mean(coords$y, na.rm = TRUE)  # Use mean y position
      
      if (nrow(ref_data) > 0) {
        # Scale the radius for better visibility
        max_r <- max(ref_data$r_norm, na.rm = TRUE)
        ref_data$size <- ref_data$r_norm / max_r * scale_factor * 2
        
        grobs[[length(grobs) + 1]] <- grid::circleGrob(
          x = ref_data$x,
          y = ref_data$y,
          r = unit(ref_data$size, "mm"),
          gp = grid::gpar(
            col = "#E0E0E0",
            fill = NA,
            lwd = 1,
            alpha = 0.7
          )
        )
      }
    }

    # Main points - using normalized values
    if (nrow(coords) > 0) {
      # Use normalized values for scaling
      max_r_norm <- max(coords$r_norm, na.rm = TRUE)
      r_scaled <- coords$r_norm / max_r_norm * scale_factor
      # Ensure alpha is always numeric and not NA
      alpha_val <- coords$alpha
      if (all(is.na(alpha_val))) alpha_val <- 1
      grobs[[length(grobs) + 1]] <- grid::pointsGrob(
        x = coords$x,
        y = coords$y,
        pch = coords$shape,
        size = unit(r_scaled, "mm"),
        gp = grid::gpar(
          col = coords$colour,
          fill = coords$fill,
          alpha = alpha_val,
          lwd = coords$stroke
        )
      )
    }

    grid::gTree(children = do.call(grid::gList, grobs))
  }
)

#' Indicator Visualization
#'
#' This geom creates a specialized visualization for indicators using
#' proportionally-sized circles with labels, all on the same horizontal line.
#'
#' @inheritParams ggplot2::layer
#' @param scale_method Method for scaling circle sizes ("sqrt", "linear", or "log")
#' @param scale_factor Maximum size of the largest point. If NULL, will be calculated automatically.
#' @param show_reference_lines Whether to show reference lines
#' @param show_reference_circles Whether to show reference circles
#' @param ... Additional arguments passed to [ggplot2::layer()]
#' @param na.rm If FALSE (the default), missing values are removed with a warning. If TRUE, missing values are silently removed.
#'
#' @export
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   indicator = c("A", "B", "C"),
#'   value = c(10, 20, 30)
#' )
#'
#' # Basic usage
#' ggplot(df, aes(x = indicator, r = value)) +
#'   geom_indicators()
#'
#' # Custom appearance
#' ggplot(df, aes(x = indicator, r = value, fill = indicator)) +
#'   geom_indicators(show_reference_circles = FALSE) +
#'   scale_fill_brewer(palette = "Set1") +
#'   theme_minimal()
geom_indicators <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE,
                            scale_method = "sqrt",
                            scale_factor = NULL,
                            show_reference_lines = TRUE,
                            show_reference_circles = TRUE,
                            ...) {
  if (is.null(mapping)) mapping <- ggplot2::aes()
  if (is.null(mapping$y)) mapping$y <- 0.5

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIndicators,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      scale_method = scale_method,
      scale_factor = scale_factor,
      show_reference_lines = show_reference_lines,
      show_reference_circles = show_reference_circles,
      na.rm = na.rm,
      ...
    )
  )
}
