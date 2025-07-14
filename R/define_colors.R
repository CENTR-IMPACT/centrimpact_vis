#' Define Color Palettes for CEnTR-IMPACT Visualizations
#'
#' This function creates a comprehensive set of color palettes specifically designed
#' for the CEnTR-IMPACT visualization system. It returns a list containing multiple
#' color palettes that maintain visual harmony and semantic consistency across
#' all visualizations in the package.
#'
#' @return A list containing the following color palettes:
#' \item{solid_palette}{A vector of 10 primary colors for main visual elements}
#' \item{fill_palette}{A vector of 10 corresponding lighter colors for fills and backgrounds}
#'
#' @details
#' The color palettes are designed with the following semantic associations:
#' \itemize{
#'   \item{Espresso (#4E342E) - Rich brown representing grounding and stability}
#'   \item{Forest Green (#3B6B35) - Symbolizing growth and sustainability}
#'   \item{Denim Blue (#3F5E78) - Conveying trust and reliability}
#'   \item{Deep Red (#990000) - Representing impact and emphasis}
#'   \item{Rust (#A64B42) - Warm earthy tone for connection to community}
#'   \item{Burnt Orange (#E18B4E) - Signifying energy and engagement}
#'   \item{Olive Green (#636b2f) - Representing harmony and balance}
#'   \item{Rainbow Indigo (#1E325C) - For depth and insight}
#'   \item{Soft Beige (#E8D8C3) - Neutral tone for background elements}
#'   \item{Stone Gray (#4A4A4A) - For text and details requiring neutrality}
#' }
#'
#' The fill palette contains lighter versions of these colors for use in area fills,
#' backgrounds, and other supporting visual elements to maintain hierarchy and readability.
#'
#' @examples
#' colors <- define_colors()
#' main_colors <- as.character(unlist(colors$solid_palette))
#' scales::show_col(main_colors)
#'
#' # Access the main color palette
#' main_colors <- colors$solid_palette
#'
#' # Access the fill palette
#' fill_colors <- colors$fill_palette
#'
#' # View the colors
#' scales::show_col(main_colors)
#'
#' @export
define_colors <- function() {
  # Create a container for all color-related objects
  result <- list()

  # ----- Main Color Palette -----
  # Each color in the main palette has specific semantic associations
  result$solid_palette <- c(
    "#4E342E", # Espresso - A rich brown representing grounding and stability
    "#3B6B35", # Forest Green - Symbolizing growth and sustainability
    "#3F5E78", # Denim Blue - Conveying trust and reliability
    "#990000", # Deep Red - Representing impact and emphasis
    "#A64B42", # Rust - A warm earthy tone for connection to community
    "#E18B4E", # Burnt Orange - Signifying energy and engagement
    "#636b2f", # Olive Green - Representing harmony and balance
    "#1E325C", # Rainbow Indigo - For depth and insight
    "#E8D8C3", # Soft Beige - A neutral tone for background elements
    "#4A4A4A" # Stone Gray - For text and details requiring neutrality
  )

  # ----- Fill Palette -----
  # Lighter versions of the main colors for area fills and backgrounds
  # These maintain the semantic associations while being less visually dominant
  result$fill_palette <- c(
    "#8e8380", # Espresso (lighter tint) - For filled areas requiring brown tones
    "#8ba086", # Forest Green (lighter tint) - For areas representing growth
    "#8c99a7", # Denim Blue (lighter tint) - For trustworthy background fills
    "#b26f6b", # Deep Red (lighter tint) - For emphasis without overwhelming
    "#bd908b", # Rust (lighter tint) - For community-related area fills
    "#f3d1b8", # Burnt Orange (lighter tint) - For engagement metrics fills
    "#9da183", # Olive Green (lighter tint) - For harmony-related fills
    "#6b7894", # Rainbow Indigo (lighter tint) - For insight-related fills
    "#f0e5d7", # Soft Beige (lighter tint) - For subtle background variation
    "#888888" # Stone Gray (lighter tint) - For neutral fills and backgrounds
  )

  # ----- Overall Background and Foreground Colors -----
  # These are used consistently across all visualizations for unified design
  result$background_color <- "#F2ECD7" # A warm, paper-like tone that reduces eye strain
  result$foreground_color <- "#4A4A4A" # A soft black for better readability than pure black

  return(result)
}
