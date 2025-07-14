################################################################################
# dynamics_visualization.R - Visualizing Project Dynamics for CEnTR-IMPACT
################################################################################
#
# This script generates visualizations for the Dynamics component of
# CEnTR-IMPACT reports. Project Dynamics represent how the various domains
# of a community-engaged research project interact and develop over time.
#
# The main domains measured in Project Dynamics are:
# - Contexts: Environmental and situational factors
# - Partnerships: Collaborative relationships between stakeholders
# - Research: Investigation and knowledge generation
# - Learning: Knowledge exchange and capacity building
# - Outcomes: Results and impacts
#
# The visualizations use a radar/spider chart approach to show the
# relative strength of each domain, as well as the dimensions within them.
# A diversity score (Gini-Simpson index) is calculated to measure how
# balanced the development is across domains.
################################################################################

# Add this at the top of the file
utils::globalVariables(c(
  "assign_colors", "color_df", "domain", "font_df", "y", "label", "domain_score", "fill_palette", "dimension", "dimension_value", "solid_palette", "dynamics_score", "position_dodge"
))

#' Create Dynamics Visualizations
#'
#' @title Create Dynamics Visualizations
#' @name visualize_dynamics
#' @description This function generates comprehensive visualizations of Project Dynamics based on the CEnTR-IMPACT framework.
#' @importFrom ggplot2 ggplot geom_bar geom_col geom_segment geom_point geom_hline geom_text scale_y_continuous scale_x_discrete scale_fill_identity scale_color_identity coord_polar theme element_blank element_line element_text position_dodge
#' @importFrom dplyr mutate
#' @importFrom scales rescale
#' @importFrom showtext showtext_auto
#' @param dynamics_df Data frame of dynamics data.
#' @param domain_df Data frame of domain data.
#' @param colors Optional list of color definitions.
#' @param project_title Character string specifying the project name for titles and contextual labeling.
#' @param report_date Date object specifying when the report was generated. Defaults to current system date.
#' @param font Optional font specification for text rendering.
#' @return A ggplot2 object containing the dynamics visualization.
#' @export
visualize_dynamics <- function(
    dynamics_df,
    domain_df,
    project_title,
    report_date = Sys.Date(),
    colors = NULL,
    font = NULL) {
  # ============================================================================
  # STEP 1: COLOR SCHEME INITIALIZATION
  # ============================================================================
  # Initialize the color palette system that will provide consistent visual
  # styling across all elements of the radar chart visualization

  # Load default color definitions if none provided
  # This ensures consistent branding and visual coherence
  if (is.null(colors)) {
    colors <- define_colors()
  }

  # ============================================================================
  # STEP 2: DATA PREPARATION AND COLOR ASSIGNMENT
  # ============================================================================
  # Enhance the input data frame by adding color specifications for both
  # fill (interior) and border (outline) elements of the visualization

  dynamics_df <- assign_colors(dynamics_df, "domain", color_df)
  domain_df <- assign_colors(domain_df, "domain", color_df)

  # ==========================================================================
  # STEP 2: CREATE DOMAIN-LEVEL VISUALIZATION DATA
  # ==========================================================================
  # Create a separate data frame containing one row per domain with all
  # necessary information for creating domain-level labels and positioning
  # in radar chart visualizations.

  domain_df <- domain_df |>
    # Create formatted labels that combine domain names with their scores
    # Format: "Domain Name (X.XX)" for clear identification
    dplyr::mutate(domain_label = glue::glue("{domain} ({domain_score})")) |>
    # Define x-coordinates for positioning domain labels in radar chart
    # These coordinates are evenly spaced to create a balanced circular layout
    # Assumes 5 domains - adjust coordinates based on actual number of domains
    dplyr::mutate(x = c(2.5, 6.5, 10.5, 14.5, 18.5)) |>
    # Convert domain to ordered factor to ensure consistent plotting order
    # This prevents ggplot from alphabetically reordering domains
    dplyr::mutate(domain = factor(domain, levels = unique(domain), ordered = TRUE))


  # ==========================================================================
  # STEP 4: PREPARE DIMENSION ORDERING FOR CONSISTENT VISUALIZATION
  # ==========================================================================
  # Ensure that dimensions maintain their original order in the input data
  # rather than being alphabetically sorted by ggplot. This is crucial for
  # maintaining the intended sequence in radar charts and other visualizations.

  dynamics_df$dimension <- factor(dynamics_df$dimension,
    levels = unique(dynamics_df$dimension),
    ordered = TRUE
  )


  # ============================================================================
  # STEP 3: FONT RENDERING SETUP
  # ============================================================================
  # Ensure we have a font scheme - use default if none provided
  configure_typography(font_df)

  # Enable showtext rendering system for custom font display
  # This ensures fonts render correctly in both screen and file output
  showtext::showtext_auto()

  # In visualize_dynamics.R, replace the geom_bar section:
  dynamics_plot <- ggplot2::ggplot() +
    # Draw the inner reference circles at 0.25, 0.5, 0.75, and 1.0
    ggplot2::geom_hline(
      yintercept = c(0.25, 0.5, 0.75, 1),
      color = "#E0E0E0", linewidth = 0.25
    ) +
    # Add labels for the reference circles
    ggplot2::geom_text(
      data = data.frame(
        y = c(0.25, 0.5, 0.75, 1),
        label = c("0.25", "0.50", "0.75", "1.00")
      ),
      aes(x = 0.5, y = y, label = label), # Position x at first category
      family = "monofont",
      size = 3,
      color = color_df$dark,
      hjust = 0.5,
      vjust = 0.5
    ) +
    geom_bar(
      data = domain_df,
      aes(
        x = domain, # Use formatted domain labels for x-axis,
        y = domain_score,
        fill = fill_palette
      ),
      stat = "identity",
      position = "identity",
      width = 1
    ) +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1)
    ) +
    # Use discrete x-axis for dimensions
    scale_x_discrete() +
    # Use the fill colors defined in the data
    scale_fill_identity() +
    # geom_text(
    #   data = domain_df, aes(
    #     y = domain_score # + 0.2,  # Position labels above the domain's score
    #     x = x,                   # Use pre-calculated x positions
    #     color = solid_palette,      # Use domain-specific colors
    #     label = domain_label     # Show domain name and score
    #   ),
    #   family = "seriffont", fontface = "italic", size = 6
    # ) +
    # # Use the text colors defined in the data
    # scale_color_identity() +
    # Style the plot with a clean, minimal theme
    theme(
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = ggplot2::element_text(
        size = 6,
        family = "seriffont",
        face = "italic"
      ),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.5, color = "#E0E0E0", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      text = element_text(family = "seriffont")
    ) +
    geom_segment(
      data = dynamics_df, aes(
        x = dimension, # Start at the domain's x position
        y = 0, # Start at the center (0)
        xend = dimension, # End at the domain's position
        yend = dimension_value, # End at the domain's score
        color = solid_palette # Use domain-specific colors
      ),
      linewidth = 1,
      position = "identity"
    ) +
    # Convert to polar coordinates for radar chart appearance
    # Start at -pi/2 to put the first category at the top
    coord_polar(start = -pi / 2)



  dynamics_plot <- ggplot2::ggplot() +
    # Add segments FIRST (position = "identity" + dodge)
    ggplot2::geom_segment(
      data = dynamics_df,
      ggplot2::aes(
        x = dimension, # Use dimension for positioning
        y = 0,
        xend = dimension,
        yend = dimension_value,
        color = solid_palette
      ),
      linewidth = 1,
      position = position_dodge(width = 0.8) # Dodge segments within domains
    ) +

    # Add bars SECOND (on top of segments)
    ggplot2::geom_bar(
      data = domain_df, # Use domain-level data
      ggplot2::aes(
        x = domain, # Group by domain
        y = domain_score,
        fill = fill_palette
      ),
      stat = "identity",
      width = 0.7, # Narrower bars to reveal segments
      alpha = 0.8, # Partial transparency
      position = "identity" # Prevent automatic dodging
    ) +

    # Remaining code (scales, theme, coord_polar)
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    ggplot2::coord_polar(start = -pi / 2)


  dynamics_plot_dashboard <- ggplot2::ggplot() +
    # Draw the inner reference circles at 0.25, 0.5, 0.75, and 1.0
    ggplot2::geom_hline(
      yintercept = c(0.25, 0.5, 0.75, 1),
      color = "#E0E0E0", linewidth = 0.25
    ) +
    # Add labels for the reference circles
    ggplot2::geom_text(
      data = data.frame(
        y = c(0.25, 0.5, 0.75, 1),
        label = c("0.25", "0.50", "0.75", "1.00")
      ),
      ggplot2::aes(x = 0.5, y = y, label = label), # Position x at first category
      family = "monofont",
      size = 8,
      color = "#4A4A4A",
      hjust = 0.5,
      vjust = 0.5
    ) +
    # Add bars for each domain's score
    # Use geom_col instead of geom_bar
    ggplot2::geom_col(
      data = domain_df,
      ggplot2::aes(
        x = domain,
        y = domain_score,
        fill = fill_palette
      ),
      width = 1,
      alpha = 0.7 # Make bars semi-transparent
    ) +

    # Add segments on top
    ggplot2::geom_segment(
      data = dynamics_df,
      ggplot2::aes(
        x = dimension, # This should match domain positioning
        y = 0,
        xend = dimension,
        yend = dimension_value,
        color = solid_palette
      ),
      linewidth = 1
    ) +

    # Add points at segment ends (like in your target image)
    ggplot2::geom_point(
      data = dynamics_df,
      ggplot2::aes(
        x = dimension,
        y = dimension_value,
        color = solid_palette
      ),
      size = 2
    ) +

    # Scales and styling
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_polar(start = -pi / 2)

  ggplot2::geom_bar(
    data = dynamics_df, ggplot2::aes(
      x = dimension,
      y = domain_score,
      fill = fill_palette
    ),
    stat = "identity", width = 1
  ) +
    # Set the y-axis to range from 0 to 1
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1)
    ) +
    # Use discrete x-axis for dimensions
    ggplot2::scale_x_discrete() +
    # Use the fill colors defined in the data
    ggplot2::scale_fill_identity() +
    # Convert to polar coordinates for radar chart appearance
    # Start at -pi/2 to put the first category at the top
    ggplot2::coord_polar(start = -pi / 2)

  # ============================================================================
  # STEP 9: BALANCE SCORING AND CONTEXTUAL ENHANCEMENTS
  # ============================================================================
  # Add summary metrics and contextual information that provide additional
  # analytical value and proper identification for the visualization


  # Ensure dimension and domain use the same factor levels
  dynamics_df$dimension <- factor(dynamics_df$dimension, levels = levels(domain_df$domain))
  domain_df$domain <- factor(domain_df$domain)

  # Then use as.numeric() for exact positioning
  ggplot2::geom_segment(
    data = dynamics_df,
    ggplot2::aes(
      x = as.numeric(factor(dimension)), # Convert to numeric
      y = 0,
      xend = as.numeric(factor(dimension)),
      yend = dimension_value,
      color = solid_palette
    ),
    linewidth = 1
  )

  dynamics_plot <- ggplot2::ggplot() +
    # Reference circles and labels
    ggplot2::geom_hline(
      yintercept = c(0.25, 0.5, 0.75, 1),
      color = "#E0E0E0", linewidth = 0.25
    ) +

    # Use geom_col instead of geom_bar
    ggplot2::geom_bar(
      data = dynamics_df,
      ggplot2::aes(
        x = domain,
        y = scales::rescale(domain_score, to = c(0, 1)),
        fill = fill_palette
      ),
      width = 1,
      alpha = 0.7, # Make bars semi-transparent
      stat = "identity"
    ) +

    # Then use as.numeric() for exact positioning
    ggplot2::geom_segment(
      data = dynamics_df,
      ggplot2::aes(
        x = as.numeric(factor(dimension)), # Convert to numeric
        y = 0,
        xend = as.numeric(factor(dimension)),
        yend = dimension_value,
        color = solid_palette
      ),
      linewidth = 1
    )

  # Add points at segment ends (like in your target image)
  ggplot2::geom_point(
    data = dynamics_df,
    ggplot2::aes(
      x = dimension,
      y = dimension_value,
      color = solid_palette
    ),
    size = 2
  ) +

    # Scales and styling
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_polar(start = -pi / 2)



  # Add the balance score metric to provide diversity assessment
  # This quantifies how evenly developed the project is across domains
  dynamics_plot <- dynamics_plot +
    # ========================================================================
    # PLOT TITLES AND SUBTITLES
    # ========================================================================
    # Add descriptive title and dynamic subtitle
    # Subtitle includes project context and report date for documentation
    ggplot2::labs(
      title = "Project Alignment",
      subtitle = glue::glue("{project_title} / {report_date}"),
      caption = bquote(S[d] == .(dynamics_score))
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        size = 20,
        hjust = 1,
        family = "monofont",
        face = "italic"
      ),
      plot.title = ggplot2::element_text(
        size = 20,
        family = "seriffont",
        face = "bold",
      ),
      plot.subtitle = ggplot2::element_text(
        size = 16,
        family = "sansfont",
        face = "italic",
      )
    )


  # ============================================================================
  # STEP 10: RETURN COMPLETED VISUALIZATION
  # ============================================================================
  # Return the fully constructed and styled radar chart for display,
  # further manipulation, or saving to file

  # Return the completed plot object for display or additional processing
  return(dynamics_plot)
}
