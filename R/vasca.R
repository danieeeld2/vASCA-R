#' Variable-selection ASCA (VASCA)
#' 
#' @description Variable-selection ANOVA Simultaneous Component Analysis (VASCA) is a data analysis 
#' algorithm for the analysis of multivariate data coming from a designed experiment. 
#' Reference: Camacho J, Vitale R, Morales-Jiménez D, Gómez-Llorente C. Variable-selection 
#' ANOVA Simultaneous Component Analysis (VASCA). Bioinformatics. 2023 Jan 
#' 1;39(1):btac795.
#' 
#' @param parglmoVS A list with the factor and interaction matrices, p-values
#'        and explained variance. Obtained with parallel general linear model
#'        with variable selection (parglmVS).
#' @param siglev Significance level (0.01 by default). If negative, it
#'        determines the number of variables selected.
#' 
#' @return A list that contains scores, loadings, singular values and projections
#'         of the factors and interactions.
#' 
vasca <- function(parglmoVS, siglev = 0.01) {
  # Argument checking
  if (missing(parglmoVS)) {
    stop("At least one argument is required. Type '?vasca' for more info.")
  }

  if (!is.numeric(siglev) || length(siglev) != 1) {
    stop("Dimension Error: parameter 'siglev' must be 1-by-1.")
  }

  # Main code
  vascao <- parglmoVS

  # Initialize factors as a list
  if (is.null(vascao$factors) || length(vascao$factors) < vascao$nFactors) {
    vascao$factors <- vector("list", vascao$nFactors)
    for (i in 1:vascao$nFactors) {
      vascao$factors[[i]] <- list()
    }
  }

  # Initialize interactions as a list of lists
  if (is.null(vascao$interactions) || length(vascao$interactions) < vascao$nInteractions) {
    vascao$interactions <- vector("list", vascao$nInteractions)
    for (i in 1:vascao$nInteractions) {
      vascao$interactions[[i]] <- list()
    }
  }

  # Do PCA on level averages for each factor
  if (vascao$nFactors > 0) {
    for (factor in 1:vascao$nFactors) {
      pvals <- parglmoVS$p[parglmoVS$ordFactors[factor,], factor]

      if (siglev > 0) {
        M <- which.min(pvals)
        M <- M[length(M)]
        thres <- siglev
      } else {
        M <- -siglev
        thres <- Inf
      }

      if (pvals[M] <= thres) {
        vascao$factors[[factor]]$stasig <- TRUE
        ind <- parglmoVS$ordFactors[factor, 1:M]
        inds_ord <- sort(ind, index.return = TRUE)
        inds <- inds_ord$x
        ord <- inds_ord$ix

        # Check the structure of matrix for proper indexing
        if (is.list(vascao$factors[[factor]]$matrix)) {
          # If matrix is a list (when nFactors=1)
          if (length(vascao$factors[[factor]]$matrix) >= max(inds)) {
            # Create matrix manually by selecting elements from the list
            mat_cols <- lapply(inds, function(i) vascao$factors[[factor]]$matrix[[i]])
            xf <- do.call(cbind, mat_cols)
          } else {
            # Handle out-of-bounds index case
            vascao$factors[[factor]]$stasig <- FALSE
            next
          }
        } else {
          # Use normal indexing if matrix is already a matrix
          xf <- vascao$factors[[factor]]$matrix[, inds]
        }
        
        # Verify xf is valid before proceeding
        if (!is.null(xf) && ncol(xf) > 0 && nrow(xf) > 0) {
          model <- pcaEig(xf, PCs = 1:rankMatrix(xf))
          
          # Copy all fields from model to vascao$factors[[factor]]
          for (fname in names(model)) {
            vascao$factors[[factor]][[fname]] <- model[[fname]]
          }

          vascao$factors[[factor]]$ind <- ind
          
          # Handle residuals access in the same way as matrix
          if (is.list(vascao$residuals)) {
            res_cols <- lapply(inds, function(i) vascao$residuals[[i]])
            res_matrix <- do.call(cbind, res_cols)
            vascao$factors[[factor]]$scoresV <- (xf + res_matrix) %*% model$loads
          } else {
            vascao$factors[[factor]]$scoresV <- (xf + vascao$residuals[, inds]) %*% model$loads
          }

          ord2 <- order(ord)
          vascao$factors[[factor]]$loadsSorted <- model$loads[ord2, ]
        } else {
          vascao$factors[[factor]]$stasig <- FALSE
        }
      } else {
        vascao$factors[[factor]]$stasig <- FALSE
      }
    }
  }
  
  # Do PCA on interactions
  if (vascao$nInteractions > 0) {
    for (interaction in 1:vascao$nInteractions) {
      pvals <- parglmoVS$p[parglmoVS$ordInteractions[interaction, ], interaction + vascao$nFactors]
      M <- which.min(pvals)
      M <- M[length(M)]
      
      if (pvals[M] <= siglev) {
        vascao$interactions[[interaction]]$stasig <- TRUE
        ind <- parglmoVS$ordInteractions[interaction, 1:M]
        inds_ord <- sort(ind, index.return = TRUE)
        inds <- inds_ord$x
        ord <- inds_ord$ix
        
        # Check the structure of matrix for proper indexing
        if (is.list(vascao$interactions[[interaction]]$matrix)) {
          # If matrix is a list (when nInteractions=1)
          if (length(vascao$interactions[[interaction]]$matrix) >= max(inds)) {
            # Create matrix manually by selecting elements from the list
            mat_cols <- lapply(inds, function(i) vascao$interactions[[interaction]]$matrix[[i]])
            xf <- do.call(cbind, mat_cols)
          } else {
            # Handle out-of-bounds index case
            vascao$interactions[[interaction]]$stasig <- FALSE
            next
          }
        } else {
          # Use normal indexing if matrix is already a matrix
          xf <- vascao$interactions[[interaction]]$matrix[, inds]
        }
        
        # Process contributing factors for this interaction
        if (vascao$interactions[[interaction]]$factors > 0) {
          for (factor in 1:vascao$interactions[[interaction]]$factors) {
            # Check if factor's matrix is a list and handle appropriately
            if (is.list(vascao$factors[[factor]]$matrix)) {
              if (length(vascao$factors[[factor]]$matrix) >= max(inds)) {
                fact_cols <- lapply(inds, function(i) vascao$factors[[factor]]$matrix[[i]])
                fact_matrix <- do.call(cbind, fact_cols)
                xf <- xf + fact_matrix
              }
            } else {
              xf <- xf + vascao$factors[[factor]]$matrix[, inds]
            }
          }
        }
        
        # Verify xf is valid before proceeding
        if (!is.null(xf) && ncol(xf) > 0 && nrow(xf) > 0) {
          model <- pcaEig(xf, PCs = 1:rankMatrix(xf))
          
          # Copy all fields from model to vascao$interactions[[interaction]]
          for (fname in names(model)) {
            vascao$interactions[[interaction]][[fname]] <- model[[fname]]
          }
          
          vascao$interactions[[interaction]]$ind <- ind
          
          # Handle residuals access in the same way as matrix
          if (is.list(vascao$residuals)) {
            res_cols <- lapply(inds, function(i) vascao$residuals[[i]])
            res_matrix <- do.call(cbind, res_cols)
            vascao$interactions[[interaction]]$scoresV <- (xf + res_matrix) %*% model$loads
          } else {
            vascao$interactions[[interaction]]$scoresV <- (xf + vascao$residuals[, inds]) %*% model$loads
          }
          
          ord2 <- order(ord)
          vascao$interactions[[interaction]]$loadsSorted <- model$loads[ord2, ]
        } else {
          vascao$interactions[[interaction]]$stasig <- FALSE
        }
      } else {
        vascao$interactions[[interaction]]$stasig <- FALSE
      }
    }
  }
  
  vascao$type <- "VASCA"
  return(vascao)
}

#' Rank of a matrix
#' 
#' @param X Data matrix
#' @return The rank of the matrix
#' 
rankMatrix <- function(X) {
  # Check for null or empty matrix
  if (is.null(X) || length(X) == 0) {
    return(0)
  }
  return(qr(X)$rank)
}