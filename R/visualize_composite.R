################################################################################
# composite_visualization.R - Creating Composite Dashboards for CEnTR-IMPACT
################################################################################
#
# This module creates comprehensive composite dashboards that combine multiple
# individual visualizations into a unified, publication-ready report layout.
# The composite approach provides:
# 1. Integrated view of all project components in a single visualization
# 2. Consistent layout and styling across different measurement domains
# 3. Professional presentation suitable for stakeholder reporting
# 4. Flexible arrangement that emphasizes different aspects appropriately
#
# The composite layout uses a two-row structure:
# - Top row: Project indicators (horizontal emphasis for comparison)
# - Bottom row: Three analytical plots (dynamics, alignment, cascade)
#
# This arrangement balances the quantitative indicators with the more complex
# analytical visualizations, creating a comprehensive project overview.
################################################################################

# Add this at the top of the file
utils::globalVariables(c(
  "wrap_elements", "plot_layout"
))

#' Create Composite Dashboard
#'
#' @title Create Composite Dashboard
#' @name visualize_composite
#' @description Create a composite dashboard combining all CEnTR-IMPACT visualizations.
#' @importFrom ggplot2 ggplot annotate theme_void theme element_rect
#' @importFrom patchwork wrap_elements plot_layout
#' @param project_title Character string specifying the project name.
#' @param report_date Date object for the report date.
#' @param dynamics_plot ggplot2 object for the dynamics radar chart.
#' @param alignment_plot ggplot2 object for the alignment visualization.
#' @param cascade_plot ggplot2 object for the cascade visualization.
#' @param indicators_plot ggplot2 object for the indicators circle visualization.
#' @param colors List of color definitions.
#' @return A patchwork composite object containing all individual visualizations arranged in an optimized two-row layout.
#' @export
visualize_composite <- function(
    project_title,
    report_date,
    dynamics_plot,
    alignment_plot,
    cascade_plot,
    indicators_plot,
    colors
) {

  # ============================================================================
  # STEP 1: INPUT VALIDATION AND SAFETY CONVERSION
  # ============================================================================
  # Ensure all input objects are compatible ggplot objects and provide
  # graceful handling of edge cases and invalid inputs

  # Define safety converter function for robust plot object handling
  # This function ensures that all inputs can be properly integrated into
  # the composite layout, even if some components are missing or invalid
  ensure_ggplot <- function(plot_obj, plot_name = "Unknown") {
    # Check if the object is a valid ggplot or patchwork object
    if (!inherits(plot_obj, "gg") && !inherits(plot_obj, "patchwork")) {
      # Issue warning about the conversion for debugging purposes
      warning("Converting non-ggplot object to placeholder for: ", plot_name)

      # Create a placeholder plot that clearly indicates the missing component
      # This maintains the composite structure while highlighting the issue
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5,
                        label = paste("PLACEHOLDER:", plot_name, "\n",
                                      "Expected ggplot object"),
                        size = 4,
                        color = "red",
                        hjust = 0.5, vjust = 0.5) +
               theme_void() +
               theme(panel.border = element_rect(color = "red", fill = NA)))
    }
    return(plot_obj)
  }

  # ============================================================================
  # STEP 2: PLOT OBJECT VALIDATION AND CONVERSION
  # ============================================================================
  # Apply safety conversion to all input plot objects to ensure
  # compatibility and provide clear feedback about any issues

  # Validate and convert each plot component with descriptive names
  # This provides clear identification of any problematic components
  dynamics_plot <- ensure_ggplot(dynamics_plot, "Dynamics Plot")
  alignment_plot <- ensure_ggplot(alignment_plot, "Alignment Plot")
  cascade_plot <- ensure_ggplot(cascade_plot, "Cascade Plot")
  indicators_plot <- ensure_ggplot(indicators_plot, "Indicators Plot")

  # ============================================================================
  # STEP 3: BOTTOM ROW COMPOSITION
  # ============================================================================
  # Create the analytical visualization row containing the three main
  # analytical components with proportional sizing for optimal presentation

  # Compose the bottom row with three analytical visualizations
  # The width ratios (1:1.5:1) are optimized based on typical content density:
  # - Dynamics: Circular/radar format works well in square space (1)
  # - Alignment: Often contains more detailed information needing width (1.5)
  # - Cascade: Hierarchical format works well in square space (1)
  composite_bottom_row <- wrap_elements(dynamics_plot) +
    wrap_elements(alignment_plot) +
    wrap_elements(cascade_plot) +
    plot_layout(widths = c(1, 1.5, 1))

  # ============================================================================
  # STEP 4: TOP ROW ASSIGNMENT
  # ============================================================================
  # Assign the indicators plot to the top row position for prominent
  # display of key quantitative metrics

  # Position indicators plot in top row for immediate visibility
  # Indicators often contain the most immediately interpretable information,
  # making them ideal for the primary position in the composite layout
  composite_top_row <- indicators_plot

  # ============================================================================
  # STEP 5: COMPOSITE ASSEMBLY AND LAYOUT
  # ============================================================================
  # Combine the top and bottom rows into the final composite layout
  # with appropriate proportional sizing for balanced presentation

  # Assemble the complete composite using vertical arrangement
  # Height ratio (3:4) gives slightly more space to the analytical row
  # while ensuring the indicators remain prominently visible
  project_composite <-
    wrap_elements(composite_top_row) /      # Top: Indicators (full width)
    wrap_elements(composite_bottom_row) +   # Bottom: Three analytical plots
    plot_layout(heights = c(3, 4))          # Proportional height allocation

  # ============================================================================
  # STEP 6: RETURN COMPLETED COMPOSITE
  # ============================================================================
  # Return the fully assembled composite visualization ready for
  # display, further customization, or output to file

  # Return the completed composite dashboard for use
  # The patchwork object can be displayed directly, saved, or further customized
  return(project_composite)
}
