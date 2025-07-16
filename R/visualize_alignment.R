utils::globalVariables(c(
  "alignment_medians", "alignment", "researcher", "partner", "overall", "group", "value", "solid_palette", "alignment_score"
))

#' Create Alignment Visualization
#'
#' @title Create Alignment Visualization
#' @name visualize_alignment
#' @description This function visualizes alignment data between researchers and partners, calculating agreement scores and presenting them in a plot.
#' @importFrom ggplot2 ggplot geom_bar scale_x_continuous scale_y_discrete scale_fill_identity scale_color_identity geom_text coord_flip theme_bw theme element_blank element_text element_line
#' @importFrom dplyr select mutate case_when
#' @importFrom tidyr pivot_longer
#' @importFrom stats na.omit
#' @param alignment_df Data frame of alignment data.
#' @param project_title Character string specifying the project title.
#' @param report_date Date object for the report date. Defaults to current system date.
#' @param color_df Optional color scheme data frame.
#' @param font_df Optional font scheme data frame.
#' @return A ggplot2 object containing the alignment visualization.
#' @export
visualize_alignment <- function(
    alignment_df,
    project_title,
    report_date = Sys.Date(),
    color_df = NULL,
    font_df = NULL) {

  # ============================================================================
  # PARAMETER VALIDATION AND SETUP
  # ============================================================================
  # Ensure we have a color scheme - use default if none provided
  # This prevents errors downstream when referencing color elements
  if(is.null(color_df)) {
    color_df <- define_colors()
  }

  # ===========================================================================
  # [4] Prepare Data for Visualization
  # ===========================================================================
  # Check if data is in long format (has alignment_medians column)
  if ("alignment_medians" %in% names(alignment_df)) {
    # Data is in long format, create a copy to modify
    plot_data <- alignment_df
    
    # Rename columns if they exist and are different
    if (!"value" %in% names(plot_data) && "alignment_medians" %in% names(plot_data)) {
      plot_data$value <- plot_data$alignment_medians
    }
    
    if (!"group" %in% names(plot_data) && "role" %in% names(plot_data)) {
      plot_data$group <- plot_data$role
    }
    
    # Ensure we have the required columns
    required_cols <- c("alignment", "value", "group")
    if (!all(required_cols %in% names(plot_data))) {
      stop("Input data must contain either 'alignment_medians' and 'role' or 'researcher', 'partner', and 'overall' columns")
    }
    
    # Process group names and convert to factor
    plot_data <- plot_data |>
      dplyr::mutate(
        group = dplyr::case_when(
          tolower(group) == "researcher" ~ "Researchers",
          tolower(group) == "partner" ~ "Partners",
          tolower(group) == "overall" ~ "Overall",
          tolower(group) == "researchers" ~ "Researchers",
          tolower(group) == "partners" ~ "Partners",
          TRUE ~ as.character(group)
        ),
        group = factor(
          group,
          levels = c("Researchers", "Partners", "Overall"),
          ordered = TRUE
        )
      )
  } else {
    # Data is in wide format, convert to long format
    plot_data <- alignment_df |>
      # Select and reshape columns for plotting
      dplyr::select(alignment, dplyr::any_of(c("researcher", "partner", "overall"))) |>
      tidyr::pivot_longer(
        cols = -alignment,
        names_to = "group",
        values_to = "value"
      ) |>
      # Convert group names to more readable labels
      dplyr::mutate(
        group = dplyr::case_when(
          tolower(group) == "researcher" ~ "Researchers",
          tolower(group) == "partner" ~ "Partners",
          tolower(group) == "overall" ~ "Overall",
          TRUE ~ as.character(group)
        ),
        # Convert to ordered factor to control plot order
        group = factor(
          group,
          levels = c("Researchers", "Partners", "Overall"),
          ordered = TRUE
        )
      )
  }
  
  # Remove any rows with missing values
  plot_data <- stats::na.omit(plot_data)

  plot_data <- assign_colors(plot_data, "alignment", color_df)

  # Extract group boundaries for label positioning
  # We need the first and last groups to know where to place
  # the alignment category labels on left and right sides
  first_group <- levels(plot_data$group)[1]
  last_group <- levels(plot_data$group)[length(levels(plot_data$group))]

  # ============================================================================
  # FONT CONFIGURATION AND LOADING
  # ============================================================================

  # Ensure we have a font scheme - use default if none provided
  configure_typography(font_df)

  # Enable showtext rendering system for custom font display
  # This ensures fonts render correctly in both screen and file output
  showtext::showtext_auto()

  # ============================================================================
  # MAIN PLOT CONSTRUCTION
  # ============================================================================

  # Create the foundational ggplot object with aesthetic mappings
  # This establishes the core structure: groups on x-axis, values on y-axis
  # Lines connect points of the same alignment category across groups
  alignment_plot <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(
      x = group,           # Categorical groupings (e.g., time periods, conditions)
      y = value,           # Numeric alignment scores
      group = alignment,   # Line grouping by alignment category
      color = solid_palette        # Color mapping from prepared data
    )
  ) +

    # ========================================================================
    # GEOMETRIC LAYERS: LINES AND POINTS
    # ========================================================================

    # Add connecting lines between points to show trends
    # Subtle line weight to avoid overwhelming the data points
  ggplot2::geom_line(linewidth = 0.5) +

    # Add data points at each intersection
    # Points are automatically sized and use the color aesthetic
    ggplot2::geom_point() +

    # ========================================================================
    # SCALE CONFIGURATIONS
    # ========================================================================

    # Position x-axis labels at top for better readability with slope plots
    # This follows best practices for before/after comparisons
  ggplot2::scale_x_discrete(position = "top") +

    # Format y-axis values with appropriate precision
    # Two decimal places provide sufficient detail without clutter
    ggplot2::scale_y_continuous(
      limits = c(0, 10.2),
      breaks = seq(0, 10, by = 2)
    ) +

    # Use identity color scale since colors are pre-assigned in data
    # This gives us full control over the color mapping
    ggplot2::scale_color_identity() +

    # ========================================================================
    # TEXT ANNOTATIONS: LEFT SIDE LABELS
    # ========================================================================

    # Add alignment category labels on the left side
    # ggrepel automatically handles overlapping text positioning
    ggrepel::geom_text_repel(
      data = dplyr::filter(plot_data, group == first_group),  # Only leftmost points
      ggplot2::aes(label = alignment),
      family = "sansfont",      # Clean sans-serif for readability
      fontface = "italic",       # Italic to distinguish from data values
      size = 4,                 # Appropriate size for chart labels
      nudge_x = -0.3,           # Push labels left of the plot area
      direction = "y",          # Allow vertical adjustment only
      hjust = 1,                # Right-align text (closer to plot)
      segment.size = 0.25,      # Thin connecting lines
      segment.color = color_df$dark,  # Subtle line color
      box.padding = 0.5         # Spacing around text boxes
    ) +

    # ========================================================================
    # TEXT ANNOTATIONS: RIGHT SIDE LABELS
    # ========================================================================

    # Mirror the left side labels on the right for balance
    # This creates a clean, symmetric appearance
    ggrepel::geom_text_repel(
      data = dplyr::filter(plot_data, group == last_group),   # Only rightmost points
      ggplot2::aes(label = alignment),
      family = "sansfont",
      fontface = "italic",
      size = 4,
      nudge_x = 0.3,            # Push labels right of the plot area
      direction = "y",
      hjust = 0,                # Left-align text (closer to plot)
      segment.size = 0.25,
      segment.color = color_df$dark,
      box.padding = 0.5
    ) +

    # ========================================================================
    # VALUE LABELS ON DATA POINTS
    # ========================================================================

    # Add numeric values directly on or near each data point
    # This provides precise readings while maintaining visual appeal
    ggplot2::geom_text(
      data = plot_data,
      ggplot2::aes(label = value),
      size = 4,                # Readable but not overwhelming
      nudge_y = 0.2,         # Slight vertical offset to avoid overlap
      family = "sansfont"
    ) +

    # ========================================================================
    # PLOT TITLES AND SUBTITLES
    # ========================================================================
    # Add descriptive title and dynamic subtitle
    # Subtitle includes project context and report date for documentation
  ggplot2::labs(
      title = "Project Alignment",
      subtitle = glue::glue("{project_title} / {report_date}"),
      caption = bquote(S[a] == .(alignment_score))
    ) +
    # ========================================================================
    # THEME CUSTOMIZATION FOR PROFESSIONAL APPEARANCE
    # ========================================================================

  ggplot2::theme(
      # Remove legend since colors are explained by direct labels
      legend.position = "none",

      # Clean background without distracting elements
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),

      # Generous margins for label space and visual breathing room
      plot.margin = ggplot2::margin(30, 30, 30, 30),

      # Remove axis titles since they're clear from context
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),

      # Style axis text for hierarchy and readability
      axis.text.y = ggplot2::element_text(
        size = 10,
        family = "sansfont",
        face = "italic",
        color = color_df$dark
      ),
      axis.text.x = ggplot2::element_text(
        size = 14,              #  size for primary group labels
        family = "seriffont",  # Serif for visual distinction
        face = "italic",
        color = color_df$dark
      ),
      plot.caption = ggplot2::element_text(
        size = 20,
        hjust = 1,
        family = "monofont",
        face = "italic"),
      plot.title = ggplot2::element_text(
        size = 20,
        family = "seriffont",
        face = "bold",
      ),
      plot.subtitle = ggplot2::element_text(
        size = 16,
        family = "sansfont",
        face = "italic",
      ),
      # Subtle horizontal grid lines for value reference
      # Light color to provide guidance without distraction
      panel.grid.major.y = ggplot2::element_line(
        linewidth = 0.25,
        color = "#E0E0E0"
      ),
      panel.grid.major.x = ggplot2::element_blank(),  # No vertical grid lines

      # Set default text family for consistency
      text = ggplot2::element_text(family = "seriffont")
    )

   # Return the completed visualization object
  # The plot can now be displayed, saved, or further customized
  return(alignment_plot)
}
