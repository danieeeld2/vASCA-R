#' Generate colorblind-friendly colors from the Okabe-Ito palette
#'
#' @param n_elems Integer. Number of colors to return.
#'
#' @return A character vector of hex color codes from the Okabe-Ito palette.
#'
#' @examples
#' okabe_ito_hex(5)
#'
#' @author Daniel Alconchel Vázquez
okabe_ito_hex <- function(n_elems) {
  okabe_ito_colors <- c(
    "#191919", # Black
    "#E69F00", # Orange
    "#56B4EA", # Light blue
    "#009E73", # Teal
    "#F0E442", # Yellow
    "#0072B2", # Blue
    "#D55E00", # Vermillion
    "#CC79A7"  # Pink
  )
  
  # If there are more colors requested than available in the palette,
  # repeat the palette
  if (n_elems > length(okabe_ito_colors)) {
    # Use rep() to repeat the elements as needed
    repeats <- ceiling(n_elems / length(okabe_ito_colors))
    map <- okabe_ito_colors[rep(1:length(okabe_ito_colors), repeats)[1:n_elems]]
  } else {
    # Simply take the first n_elems colors
    map <- okabe_ito_colors[1:n_elems]
  }
  
  return(map)
}
