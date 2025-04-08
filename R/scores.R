#' Compute and plot scores.
#'
#' @param model A list with model parameters:
#'   - scores: Matrix [N x A] of data scores
#'   - lvs: Vector of latent variables to use (e.g., 1:2)
#'   - type: "PCA" or "PLS" (optional)
#'
#' @param obstest Optional [L x M] matrix with external observations
#' @param plottype "Scatter" (default) or "Bars"
#' @param plotcal Logical. If TRUE (default), plot both calibration and test. If FALSE, only test
#' @param tit Plot title (default: "")
#' @param label Labels for observations
#' @param classes Classes for visualization
#' @param blur Label density control (default: 1)
#' @param color Color palette or vector
#'
#' @return List of ggplot objects
#'
scores <- function(model,
                   obstest = NULL,
                   plottype = "Scatter",
                   plotcal = TRUE,
                   tit = "",
                   blur = 1,
                   label = NULL,
                   classes = NULL,
                   color = NULL) {

  # Basic input validation
  if (missing(model)) stop("Model argument is required")
  
  # Extract model components
  T_cal <- model$scores
  N <- nrow(T_cal)
  M <- ifelse(!is.null(model$loads), nrow(model$loads), ncol(T_cal))
  A <- length(model$lvs)
  
  # Validate blur parameter
  if (!is.null(blur)) {
    if (length(blur) != 1 || !is.numeric(blur)) {
      stop("Parameter 'BlurIndex' must be a single numeric value")
    }
  }
  
  # Determine data dimensions
  L <- ifelse(is.null(obstest), 0, nrow(obstest))
  
  # Check if test data is embedded in the model
  if (is.null(obstest) && !is.null(model$test)) {
    obstest <- model$test
    L <- nrow(obstest)
  }
  
  original_plotcal <- plotcal  # Save original value to decide on label initialization
  if (!plotcal && L == 0) {
    warning("plotcal=FALSE specified but no test data provided. Using calibration data instead.")
    plotcal <- TRUE  # Force plotcal=TRUE to show calibration data
  }
  
  # K depends on plotcal and the presence of test data
  if (plotcal) {
    K <- N + L
  } else {
    K <- L # K should be L if plotting only test data
  }
  
  # Set default labels if not provided
  if (is.null(label)) {
    if (plotcal) {
      label <- c(1:N, 1:L)
    } else {
      label <- 1:L
    }
    label <- as.character(label)
  } else {
    # Ensure label has the correct length
    if (length(label) < K) {
      warning("Not enough labels provided. Extending with numeric indices.")
      additional_labels <- as.character((length(label) + 1):K)
      label <- c(label, additional_labels)
    } else if (length(label) > K) {
      warning("Too many labels provided. Truncating to match observation count.")
      label <- label[1:K]
    }
  }
    
  # Set default classes if not provided
  if (is.null(classes)) {
    if (plotcal) {
      classes <- rep(1, K) # Default to single class if plotcal=TRUE and no classes provided
    } else {
      classes <- rep(1, L) # Use single class for test data if plotcal is FALSE
    }
  } else {
    # Ensure classes has the correct length
    if (length(classes) < K) {
      warning("Not enough class labels provided. Repeating pattern cyclically.")
      classes_pattern <- classes
      classes <- rep(classes_pattern, length.out = K)
    } else if (length(classes) > K) {
      warning("Too many class labels provided. Truncating to match observation count.")
      classes <- classes[1:K]
    }
  }
  
  # Convert row arrays to column vectors if needed
  if (length(label) > 0 && !is.vector(label)) {
    label <- as.vector(label)
  }
  if (length(classes) > 0 && !is.vector(classes)) {
    classes <- as.vector(classes)
  }
  
  # Verify dimensions of inputs
  if (K > 0) {
    if (length(label) != K) {
      stop(sprintf("Label length must match number of observations (got %d, need %d)", 
                  length(label), K))
    }
    if (length(classes) != K) {
      stop(sprintf("Classes length must match number of observations (got %d, need %d)", 
                  length(classes), K))
    }
  }
  
  # Calculate diagonal of scores variance 
  d <- diag(t(T_cal) %*% T_cal)
  
  # Use alternative scores (scoresV) if provided
  if (!is.null(model$scoresV)) {
    T_cal <- model$scoresV
  }
  
  # Process test data if available
  if (!is.null(obstest)) {
    if (!is.null(model$av)) {
      testcs <- scale(obstest, center = model$av, scale = model$sc)
    } else {
      testcs <- obstest
    }
    TT <- testcs %*% model$loads
  } else {
    TT <- NULL
  }
  
  # Combine calibration and test data
  if (plotcal) {
    if (is.null(TT)) {
      Tt <- T_cal
    } else {
      Tt <- rbind(T_cal, TT)
    }
  } else {
    Tt <- TT # If plotcal is FALSE, use only test data
  }
  
  # Initialize plot list
  figH <- list()
  
  # Set dimension label prefix (PC for PCA, LV for PLS)
  dim <- ifelse(!is.null(model$type) && model$type == "PLS", "LV", "PC")
  
  # Generate bar plots
  if (plottype == "Bars" || A == 1) {
    for (i in seq_along(model$lvs)) {
      var_explained <- 100 * d[i] / model$var
      fig <- plotVec(Tt[, i],
                   EleLabel = label,
                   ObsClass = classes,
                   XYLabel = c("", sprintf("Scores %s %d (%.0f%%)", dim, model$lvs[i], var_explained)),
                   Color = color) + ggplot2::ggtitle(tit) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      figH[[length(figH) + 1]] <- fig
    }
  } 
  # Generate scatter plots for multiple LVs
  else if (plottype == "Scatter") {
    for (i in 1:(length(model$lvs)-1)) {
      for (j in (i+1):length(model$lvs)) {
        var_explained_i <- 100 * d[i] / model$var
        var_explained_j <- 100 * d[j] / model$var
        fig <- plotScatter(data.frame(x = Tt[, i], y = Tt[, j]),
                         EleLabel = label,
                         ObsClass = classes,
                         XYLabel = c(sprintf("Scores %s %d (%.0f%%)", dim, model$lvs[i], var_explained_i),
                                    sprintf("Scores %s %d (%.0f%%)", dim, model$lvs[j], var_explained_j)),
                         BlurIndex = blur,
                         Color = color) + ggplot2::ggtitle(tit) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
        figH[[length(figH) + 1]] <- fig
      }
    }
  }
  
  return(figH)
}