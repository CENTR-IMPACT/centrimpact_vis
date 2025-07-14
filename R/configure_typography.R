#' Configure Typography Settings for CEnTR-IMPACT Visualizations
#'
#' This function sets up the typography system for the CEnTR-IMPACT visualization package.
#' It registers Google Fonts for consistent text rendering across different output formats.
#' By default, it uses Lora (serif), Lato (sans-serif), and IBM Plex Mono (monospace) fonts.
#'
#' @param font_df Optional data frame containing custom font specifications. Should have columns:
#'   \describe{
#'     \item{serif}{Character string specifying the serif font family name from Google Fonts}
#'     \item{sans}{Character string specifying the sans-serif font family name from Google Fonts}
#'     \item{mono}{Character string specifying the monospace font family name from Google Fonts}
#'   }
#'   If NULL, uses the default font stack.
#'
#' @return Invisibly returns NULL. This function is called for its side effects of
#'   registering fonts with the `showtext` and `sysfonts` packages.
#'
#' @details
#' This function performs the following actions:
#' 1. Sets up the specified Google Fonts using `sysfonts::font_add_google()`
#' 2. Registers the fonts with the following internal names:
#'    - "seriffont" for serif text
#'    - "sansfont" for sans-serif text
#'    - "monofont" for monospace text
#'
#' @examples
#' # Use default fonts (Lora, Lato, IBM Plex Mono)
#' configure_typography()
#'
#' # Use custom fonts
#' custom_fonts <- data.frame(
#'   serif = "Merriweather",
#'   sans = "Open Sans",
#'   mono = "Source Code Pro"
#' )
#' configure_typography(custom_fonts)
#'
#' @seealso
#' \code{\link[showtext]{showtext_auto}} for enabling font rendering
#' \code{\link[sysfonts]{font_add_google}} for adding Google Fonts
#'
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#'
#' @export
configure_typography <- function(font_df = NULL) {
  if (is.null(font_df)) {
    serifname <- "Lora"
    sansname <- "Lato"
    mononame <- "IBM Plex Mono"
  } else {
    serifname <- font_df$serif
    sansname <- font_df$sans
    mononame <- font_df$mono
  }
  font_add_google(serifname, "seriffont")
  font_add_google(sansname, "sansfont")
  font_add_google(mononame, "monofont")
  return(NULL)
}
