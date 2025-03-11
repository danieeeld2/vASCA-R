preprocess2D <- function(x, Preprocessing = 2, Weights = rep(1, ncol(x))) {
  # Preprocess 2-way data.
  #
  # xcs = preprocess2D(x)  # minimum call
  #
  # INPUTS:
  # x: matrix of size NxM (billinear data set)
  # Preprocessing: 0 = no preprocessing, 1 = mean-centering, 2 = auto-scaling (default)
  # Weights: vector of length M (weights applied after preprocessing)
  #
  # OUTPUTS:
  # xcs: preprocessed data
  # average: sample average according to the preprocessing method
  # scale: sample scale according to the preprocessing method

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
