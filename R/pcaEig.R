#' Principal Component Analysis using Eigen Decomposition
#'
#' @param xcs A numeric matrix representing the preprocessed bilinear data set (NxM).
#' @param PCs (Optional) A numeric or character vector specifying the principal components to be considered. Default is the range from 1 to the rank of the matrix (excluding 0).
#'
#' @return A list containing the following components:
#' \item{var}{The total variance of the data.}
#' \item{lvs}{The indices of the latent variables (principal components).}
#' \item{loads}{A matrix of loadings (eigenvectors).}
#' \item{scores}{A matrix of scores (transformed data).}
#' \item{type}{The type of analysis, which is "PCA" in this case.}
#'
#' @examples
#' # Example usage
#' xcs <- matrix(rnorm(100), ncol = 10)  # Example preprocessed data
#' model <- pcaEig(xcs, PCs = 1:2)
#'
#' @author Daniel Alconchel Vázquez
#' @export
pcaEig <- function(xcs, ...) {
  ## Arguments checking

  # Set default values
  routine_name <- "pcaEig"
  if (nargs() < 1) {
    stop(sprintf("Error in the number of arguments. Type 'help(\"%s\")' for more info.", routine_name))
  }
  N <- nrow(xcs)
  M <- ncol(xcs)

  # Introduce optional inputs as parameters (name-value pair)
  params <- list(...)
  pcs <- NULL
  if ("PCs" %in% names(params)) {
    pcs_value <- params$PCs
    if (is.character(pcs_value) && grepl("^[0-9]+:[0-9]+$", pcs_value)) {
      range_vals <- as.numeric(unlist(strsplit(pcs_value, ":")))
      pcs <- seq(range_vals[1], range_vals[2])
      if (range_vals[1] == range_vals[2]) {
        pcs <- c(range_vals[1])
      }
    } else {
      pcs <- as.numeric(pcs_value)
    }
  } else {
    pcs <- 1:qr(xcs)$rank # Changed default to 1:rank
  }

  # Extract inputs from inputParser for code legibility
  # pcs already extracted

  # Preprocessing
  if (any(is.na(pcs))) {
    stop(sprintf("Value Error: parameter 'Pcs' contains NA values. Type 'help(\"%s\")' for more info.", routine_name))
  }
  pcs <- unique(as.vector(pcs))
  pcs <- pcs[pcs > 0] # Changed to pcs > 0
  pcs <- pcs[pcs <= ncol(xcs)]
  pcs <- pcs[pcs <= qr(xcs)$rank]
  A <- length(pcs)

  # Convert to row matrix after preprocessing
  if (is.vector(pcs) && length(pcs) > 0) {
    pcs <- t(as.matrix(pcs))
  }

  # Validate dimensions of input data
  if (nrow(pcs) != 1 || ncol(pcs) != A) {
    stop(sprintf("Dimension Error: parameter 'Pcs' must be 1-by-%d. Type 'help(\"%s\")' for more info.", A, routine_name))
  }

  # Validate values of input data
  if (any(pcs < 0) || any(pcs != as.integer(pcs))) {
    stop(sprintf("Value Error: parameter 'Pcs' must contain positive integers. Type 'help(\"%s\")' for more info.", routine_name))
  }


  ## Main code

  if (N > M) {
    XX <- t(xcs) %*% xcs
    eigen_decomp <- eigen(XX)
    p <- as.matrix(eigen_decomp$vectors[, order(Re(eigen_decomp$values), decreasing = TRUE), drop = FALSE])
    D <- eigen_decomp$values[order(Re(eigen_decomp$values), decreasing = TRUE)]
    t <- as.matrix(xcs %*% p, drop = FALSE)
  } else {
    XX <- xcs %*% t(xcs)
    eigen_decomp <- eigen(XX)
    t <- as.matrix(eigen_decomp$vectors[, order(Re(eigen_decomp$values), decreasing = TRUE), drop = FALSE])
    D <- eigen_decomp$values[order(Re(eigen_decomp$values), decreasing = TRUE)]
    s <- sqrt(Re(D))
    t <- t %*% diag(s)
    p <- t(xcs) %*% t
    p <- as.matrix(p, drop = FALSE)
  }

  if (length(pcs) == 0) {
    stop(sprintf("Error: No valid principal components selected. Type 'help(\"%s\")' for more info.", routine_name))
  }

  p <- p[, as.vector(pcs), drop = FALSE] # Use drop = FALSE to ensure it's a matrix
  t <- t[, as.vector(pcs), drop = FALSE] # Use drop = FALSE to ensure it's a matrix

  model <- list()
  model$var <- sum(diag(XX))
  model$lvs <- 1:ncol(p)
  model$loads <- p
  model$scores <- t
  model$type <- "PCA"

  return(model)
}