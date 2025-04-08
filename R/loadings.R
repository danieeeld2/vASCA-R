#' Compute and plot loadings
#'
#' @param model A list with model parameters. 
#'   \itemize{
#'     \item \code{lvs}: Numeric vector with Latent Variables considered (e.g. \code{1:2} selects the first two LVs).
#'     \item \code{loads}: Matrix [MxA] of model parameters.
#'     \item \code{scores}: Matrix [NxA] of data scores.
#'   }
#' @param plottype Character. Type of plot: \code{"Scatter"} (default) or \code{"Bars"}.
#' @param tit Character. Title for the plots. Empty by default.
#' @param label Character vector [Mx1]. Name of the variables (indices are used by default).
#' @param classes Numeric vector [Mx1]. Groups for different visualization (a single group by default).
#' @param blur Numeric. Avoid blur when adding labels. The higher, the more labels are printed. \code{Inf} shows all labels (default is 1).
#' @param color Character or vector. Color for data. Options include \code{"hsv"}, \code{"parula"}, \code{"okabeIto"} for color palettes, or a vector of specific colors.
#'
#' @return A list of ggplot objects representing the loadings plots
#'
#' @examples
#' # Example with simulated data:
#' model <- list(
#'   lvs = 1:2,
#'   loads = matrix(rnorm(20), nrow = 10),
#'   scores = matrix(rnorm(40), nrow = 20),
#'   type = "PCA"
#' )
#' loadings(model)
#'
#' @author Daniel Alconchel Vázquez
#' @export
#' @importFrom ggplot2 ggtitle
loadings <- function(model,
                     plottype = "Scatter",
                     tit = " ",
                     label = NULL,
                     classes = NULL,
                     blur = 1,
                     color = NULL) {

  # Parameters checking
  if (missing(model)) {
    stop("Error in the number of arguments. Type '?loadings' for more info.")
  }

  N <- nrow(model$scores)
  M <- nrow(model$loads)

  # Set default values if not provided
  if (is.null(label)) {
    label <- 1:M
  }

  if (is.null(classes)) {
    classes <- rep(1, M)
  }

  # Convert vectors to column vectors (in R this is just making sure they're vectors)
  if (length(label) == M && !is.matrix(label)) {
    label <- as.vector(label)
  }

  if (length(classes) == M && !is.matrix(classes)) {
    classes <- as.vector(classes)
  }

  # Validate dimensions of input data
  if (length(label) != M) {
    stop("Dimension Error: parameter 'label' must be of length M.")
  }

  if (length(classes) != M) {
    stop("Dimension Error: parameter 'classes' must be of length M.")
  }

  if (!is.null(blur) && length(blur) != 1) {
    stop("Dimension Error: parameter 'blur' must be a single value.")
  }

  # Main code
  P <- model$loads

  # Select the dimension label based on model type
  if (!("type" %in% names(model)) || model$type == "PCA") {
    dim <- "PC"
  } else if (model$type == "PLS") {
    dim <- "LV"
  } else {
    dim <- "PC"
  }

  # Create plots
  figH <- list()

  if (length(model$lvs) == 1 || plottype == "Bars") {
    for (i in 1:length(model$lvs)) {
      fig <- plotVec(P[, i],
                    EleLabel = label,
                    ObsClass = classes,
                    XYLabel = c("", sprintf("Loadings %s %d", dim, model$lvs[i])),
                    Color = color)

      fig <- fig + ggtitle(tit)

      figH[[length(figH) + 1]] <- fig
    }
  } else if (plottype == "Scatter") {
    for (i in 1:(length(model$lvs) - 1)) {
      for (j in (i + 1):length(model$lvs)) {
        fig <- plotScatter(cbind(P[, i], P[, j]),
                          EleLabel = label,
                          ObsClass = classes,
                          XYLabel = c(sprintf("Loadings %s %d", dim, model$lvs[i]),
                                      sprintf("Loadings %s %d", dim, model$lvs[j])),
                          BlurIndex = blur,
                          Color = color)

        fig <- fig + ggtitle(tit)

        figH[[length(figH) + 1]] <- fig
      }
    }
  }

  return(figH)
}