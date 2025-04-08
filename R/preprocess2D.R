#' Preprocess 2D Data
#'
#' This function preprocesses 2-way data by applying various methods of preprocessing
#' such as mean centering, auto-scaling, or Pareto scaling. Weights can be applied after
#' the preprocessing step.
#'
#' @param x A matrix of size NxM representing the 2D data set (N samples and M features).
#' @param Preprocessing A numeric value indicating the preprocessing method:
#'   \itemize{
#'     \item 0: No preprocessing
#'     \item 1: Mean centering
#'     \item 2: Auto-scaling (default)
#'     \item 3: Pareto-scaling
#'   }
#' @param Weights A numeric vector of length M, representing the weights to apply after preprocessing.
#'   The default is a vector of ones.
#'
#' @return A list with the following components:
#' \item{xcs}{A matrix of the preprocessed data.}
#' \item{average}{A vector containing the sample averages according to the preprocessing method.}
#' \item{scale}{A vector containing the sample scale according to the preprocessing method.}
#'
#' @examples
#' # Example usage:
#' x <- matrix(rnorm(100), nrow = 10, ncol = 10)  # Generate random data
#' result <- preprocess2D(x, Preprocessing = 2)  # Apply auto-scaling
#' result$xcs  # Preprocessed data
#' result$average  # Sample averages
#' result$scale  # Sample scales
#'
#' @author Daniel Alconchel Vázquez
#' @export
preprocess2D <- function(x, Preprocessing = 2, Weights = rep(1, ncol(x))) {
  N <- nrow(x)
  M <- ncol(x)

  # Validate dimensions of inputs
  stopifnot(length(Weights) == M)
  stopifnot(Preprocessing >= 0 && Preprocessing <= 3 && Preprocessing == floor(Preprocessing))

  if (N == 1 && Preprocessing == 2) {
    Preprocessing <- 1
  }

  if (Preprocessing == 1) {  # Mean centering
    nanM <- is.na(x)
    anM <- 1 - nanM
    x[nanM] <- 0
    average <- colSums(x, na.rm = TRUE) / colSums(anM, na.rm = TRUE)
    scale <- rep(1, M)
    xcs <- sweep(x, 2, average)
    xcs[nanM] <- NA
  } else if (Preprocessing == 2) {  # Auto-scaling
    nanM <- is.na(x)
    anM <- 1 - nanM
    x[nanM] <- 0
    average <- colSums(x, na.rm = TRUE) / colSums(anM, na.rm = TRUE)
    xc <- sweep(x, 2, average)
    xc[nanM] <- 0
    scale <- sqrt(colSums(xc^2, na.rm = TRUE) / (colSums(anM, na.rm = TRUE) - 1))
    scale[scale == 0] <- sqrt(1 / (2 * colSums(anM, na.rm = TRUE) - 1))
    xcs <- sweep(xc, 2, scale, FUN = "/")
    xcs[nanM] <- NA
  } else if (Preprocessing == 3) {  # Pareto-scaling
    nanM <- is.na(x)
    anM <- 1 - nanM
    x[nanM] <- 0
    average <- colSums(x, na.rm = TRUE) / colSums(anM, na.rm = TRUE)
    xc <- sweep(x, 2, average)
    xc[nanM] <- 0
    scale <- sqrt(sqrt(colSums(xc^2, na.rm = TRUE) / (colSums(anM, na.rm = TRUE) - 1)))
    scale[scale == 0] <- sqrt(sqrt(1 / (2 * colSums(anM, na.rm = TRUE) - 1)))
    xcs <- sweep(xc, 2, scale, FUN = "/")
    xcs[nanM] <- NA
  } else {  # No preprocessing
    average <- rep(0, M)
    scale <- rep(1, M)
    xcs <- x
  }

  # Apply weights
  xcs <- sweep(xcs, 2, Weights, FUN = "*")
  
  return(list(xcs = xcs, average = average, scale = scale))
}
