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
#' @export 
vasca <- function(parglmoVS, siglev = 0.01) {
  # Argument validation
  if (missing(parglmoVS)) {
    stop("At least one argument is required. Type '?vasca' for more info.")
  }

  if (!is.numeric(siglev) || length(siglev) != 1) {
    stop("Dimension Error: parameter 'siglev' must be a single numeric value.")
  }

  # Create output structure
  vascao <- parglmoVS

  # Initialize factors as a list if not properly structured
  if (is.null(vascao$factors) || length(vascao$factors) < vascao$nFactors) {
    vascao$factors <- vector("list", vascao$nFactors)
    for (i in 1:vascao$nFactors) {
      vascao$factors[[i]] <- list(stasig = FALSE) # Default initialization
    }
  }

  # Handle interactions structure - robust conversion from data.frame if needed
  if (!is.null(vascao$interactions)) {
    if (is.data.frame(vascao$interactions)) {
      # Convert data.frame to properly structured list
      tmp <- vector("list", nrow(vascao$interactions))
      for (i in seq_along(tmp)) {
        tmp[[i]] <- list(
          Dvars = if (!is.null(vascao$interactions$Dvars)) vascao$interactions$Dvars[[i]] else integer(),
          factors = if (!is.null(vascao$interactions$factors)) vascao$interactions$factors[[i]] else integer(),
          matrix = if (!is.null(vascao$interactions$matrix)) vascao$interactions$matrix[[i]] else NULL,
          stasig = FALSE
        )
      }
      vascao$interactions <- tmp
    } else if (is.list(vascao$interactions)) {
      # Ensure each interaction has required structure
      for (i in seq_along(vascao$interactions)) {
        if (is.null(vascao$interactions[[i]]$stasig)) {
          vascao$interactions[[i]]$stasig <- FALSE
        }
      }
    }
  }

  # Initialize interactions if null or doesn't match nInteractions
  if (is.null(vascao$interactions) || length(vascao$interactions) < vascao$nInteractions) {
    vascao$interactions <- vector("list", vascao$nInteractions)
    for (i in 1:vascao$nInteractions) {
      vascao$interactions[[i]] <- list(
        Dvars = integer(),
        factors = integer(),
        matrix = NULL,
        stasig = FALSE
      )
    }
  }

  # Process factors with PCA
  if (vascao$nFactors > 0) {
    for (factor in 1:vascao$nFactors) {
      pvals <- parglmoVS$p[parglmoVS$ordFactors[factor,], factor]
      ordered_var_indices_for_factor <- parglmoVS$ordFactors[factor,]

      # Select variables based on significance level
      if (siglev > 0) {
        significant_p_indices <- which(pvals <= siglev)
        if (length(significant_p_indices) > 0) {
          ind <- ordered_var_indices_for_factor[significant_p_indices]
        } else {
          vascao$factors[[factor]]$stasig <- FALSE
          next
        }
      } else {
        num_select <- -siglev
        if (length(pvals) >= num_select) {
          ind <- ordered_var_indices_for_factor[1:num_select]
        } else {
          vascao$factors[[factor]]$stasig <- FALSE
          next
        }
      }

      # Sort selected indices
      inds_ord <- sort(ind, index.return = TRUE)
      inds <- inds_ord$x
      ord <- inds_ord$ix

      # Extract matrix data with proper structure checks
      xf <- tryCatch({
        if (is.list(vascao$factors[[factor]]$matrix)) {
          if (length(vascao$factors[[factor]]$matrix) >= max(inds)) {
            mat_cols <- lapply(inds, function(i) vascao$factors[[factor]]$matrix[[i]])
            do.call(cbind, mat_cols)
          } else {
            stop("Matrix index out of bounds")
          }
        } else {
          vascao$factors[[factor]]$matrix[, inds]
        }
      }, error = function(e) {
        warning("Failed to extract matrix for factor ", factor, ": ", e$message)
        NULL
      })

      # Perform PCA if valid data
      if (!is.null(xf) && ncol(xf) > 0 && nrow(xf) > 0) {
        rk <- rankMatrix(xf)
        if (rk > 0) {
          model <- tryCatch({
            pcaEig(xf, PCs = 1:rk)
          }, error = function(e) {
            warning("PCA failed for factor ", factor, ": ", e$message)
            NULL
          })

          if (!is.null(model)) {
            vascao$factors[[factor]]$stasig <- TRUE
            for (fname in names(model)) {
              vascao$factors[[factor]][[fname]] <- model[[fname]]
            }
            vascao$factors[[factor]]$ind <- ind

            # Calculate scoresV
            if (is.list(vascao$residuals)) {
              res_cols <- lapply(inds, function(i) vascao$residuals[[i]])
              res_matrix <- do.call(cbind, res_cols)
              vascao$factors[[factor]]$scoresV <- (xf + res_matrix) %*% model$loads
            } else {
              vascao$factors[[factor]]$scoresV <- (xf + vascao$residuals[, inds]) %*% model$loads
            }

            # Sort loadings
            ord2 <- order(ord)
            vascao$factors[[factor]]$loadsSorted <- model$loads[ord2, ]
          } else {
            vascao$factors[[factor]]$stasig <- FALSE
          }
        } else {
          vascao$factors[[factor]]$stasig <- FALSE
        }
      } else {
        vascao$factors[[factor]]$stasig <- FALSE
      }
    }
  }

  # Process interactions with PCA
  if (vascao$nInteractions > 0) {
    for (interaction in 1:vascao$nInteractions) {
      # Skip if interaction structure is not properly initialized
      if (is.null(vascao$interactions[[interaction]])) next
      
      col_index <- interaction + vascao$nFactors
      p_inter <- parglmoVS$p[, col_index]

      # Select variables based on significance level
      if (siglev > 0) {
        sig_rows <- which(p_inter <= siglev)
        if (length(sig_rows) > 0) {
          ind <- parglmoVS$ordInteractions[sig_rows]
        } else {
          vascao$interactions[[interaction]]$stasig <- FALSE
          next
        }
      } else {
        num_select <- -siglev
        ordered_p_indices <- order(p_inter)
        if (length(ordered_p_indices) >= num_select) {
          ind <- parglmoVS$ordInteractions[ordered_p_indices[1:num_select]]
        } else {
          vascao$interactions[[interaction]]$stasig <- FALSE
          next
        }
      }

      # Sort selected indices
      inds_ord <- sort(ind, index.return = TRUE)
      inds <- inds_ord$x
      ord <- inds_ord$ix

      # Extract interaction matrix with proper checks
      IM <- tryCatch({
        if (is.list(vascao$interactions[[interaction]]$matrix)) {
          if (length(vascao$interactions[[interaction]]$matrix) >= 1) {
            vascao$interactions[[interaction]]$matrix[[1]]
          } else {
            stop("No matrix available")
          }
        } else {
          vascao$interactions[[interaction]]$matrix
        }
      }, error = function(e) {
        warning("Failed to extract matrix for interaction ", interaction, ": ", e$message)
        NULL
      })

      if (is.null(IM) || ncol(IM) < max(inds)) {
        vascao$interactions[[interaction]]$stasig <- FALSE
        next
      }

      xf <- IM[, inds]

      # Add contributing factors if they exist
      if (!is.null(vascao$interactions[[interaction]]$factors) && 
          length(vascao$interactions[[interaction]]$factors) > 0) {
        for (factor in vascao$interactions[[interaction]]$factors) {
          if (factor <= length(vascao$factors)) {
            fact_matrix <- tryCatch({
              if (is.list(vascao$factors[[factor]]$matrix)) {
                if (length(vascao$factors[[factor]]$matrix) >= max(inds)) {
                  mat_cols <- lapply(inds, function(i) vascao$factors[[factor]]$matrix[[i]])
                  do.call(cbind, mat_cols)
                }
              } else {
                vascao$factors[[factor]]$matrix[, inds]
              }
            }, error = function(e) NULL)
            
            if (!is.null(fact_matrix)) {
              xf <- xf + fact_matrix
            }
          }
        }
      }

      # Perform PCA if valid data
      if (!is.null(xf) && ncol(xf) > 0 && nrow(xf) > 0) {
        rk <- rankMatrix(xf)
        if (rk > 0) {
          model <- tryCatch({
            pcaEig(xf, PCs = 1:rk)
          }, error = function(e) {
            warning("PCA failed for interaction ", interaction, ": ", e$message)
            NULL
          })

          if (!is.null(model)) {
            vascao$interactions[[interaction]]$stasig <- TRUE
            for (fname in names(model)) {
              vascao$interactions[[interaction]][[fname]] <- model[[fname]]
            }
            vascao$interactions[[interaction]]$ind <- ind

            # Calculate scoresV
            if (is.list(vascao$residuals)) {
              res_cols <- lapply(inds, function(i) vascao$residuals[[i]])
              res_matrix <- do.call(cbind, res_cols)
              vascao$interactions[[interaction]]$scoresV <- (xf + res_matrix) %*% model$loads
            } else {
              vascao$interactions[[interaction]]$scoresV <- (xf + vascao$residuals[, inds]) %*% model$loads
            }

            # Sort loadings
            ord2 <- order(ord)
            vascao$interactions[[interaction]]$loadsSorted <- model$loads[ord2, ]
          } else {
            vascao$interactions[[interaction]]$stasig <- FALSE
          }
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
  # Check for null, empty or all-NA matrix
  if (is.null(X) || length(X) == 0 || all(is.na(X))) return(0)
  
  # Convert to matrix if not already
  if (!is.matrix(X)) X <- as.matrix(X)
  
  # Calculate rank using QR decomposition
  qr(X)$rank
}