pcaEig <- function(xcs, ...) {
  # Principal Component Analysis based on the eigendecompostion of XX.
  #
  # model = pcaEig(xcs)     % minimum call
  #
  #
  #
  # INPUTS:
  #
  # xcs: [NxM] preprocessed billinear data set
  #
  #
  # Optional INPUTS (parameter):
  #
  # 'PCs': [1xA] Principal Components considered (e.g. pcs = 1:2 selects the
  #   first two PCs). By default, pcs = 0:min(size(xcs))
  #
  #
  # OUTPUTS:
  #
  # model: structure that contains model information

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
    pcs <- 0:qr(xcs)$rank
  }

  # Extract inputs from inputParser for code legibility
  # pcs already extracted

  # Preprocessing
  pcs <- unique(as.vector(pcs))
  pcs <- pcs[pcs != 0]
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
    p <- eigen_decomp$vectors
    D <- eigen_decomp$values
    ind <- order(Re(D), decreasing = TRUE)
    kk <- Re(D)[ind]
    p <- p[, ind]
    t <- xcs %*% p
  } else {
    XX <- xcs %*% t(xcs)
    eigen_decomp <- eigen(XX)
    t <- eigen_decomp$vectors
    D <- eigen_decomp$values
    s <- sqrt(Re(D))
    ind <- order(s, decreasing = TRUE)
    kk <- s[ind]
    t <- eigen_decomp$vectors[, ind] %*% diag(s[ind]) # Equivalent to t(:,ind).*(ones(N,1)*s(ind)')
    p <- t(xcs) %*% t
    for (i in 1:ncol(p)) {
      p[, i] <- p[, i] / sqrt(t(p[, i]) %*% p[, i])
    }
  }

  if (length(pcs) == 0) {
    stop(sprintf("Error: No valid principal components selected. Type 'help(\"%s\")' for more info.", routine_name))
  }

  p <- p[, as.vector(pcs)]
  t <- t[, as.vector(pcs)]

  model <- list()
  model$var <- sum(diag(XX))
  if(!is.null(ncol(p))){
    model$lvs <- 1:ncol(p)
  } else {
    model$lvs <- 1
  }
  model$loads <- p
  model$scores <- t
  model$type <- "PCA"

  return(model)
}
