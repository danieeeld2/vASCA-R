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
  M <- nrow(model$loads)
  A <- length(model$lvs)
  
  # Determine data dimensions
  L <- ifelse(is.null(obstest), 0, nrow(obstest))
  K <- ifelse(plotcal, N + L, L)
  
  # Set default labels and classes if not provided
  if (is.null(label)) {
    label <- if (plotcal) c(1:N, 1:L) else 1:L
    label <- as.character(label)
  }
  
  if (is.null(classes)) {
    classes <- if (plotcal) c(rep(1, N), rep(2, L)) else rep(1, L)
  }
  
  # Validate input dimensions
  if (length(label) != K) stop("Label length must match number of observations")
  if (length(classes) != K) stop("Classes length must match number of observations")
  
  # Calculate explained variance
  d <- diag(t(T_cal) %*% T_cal)
  
  # Process test data if provided
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
  
  # Combine calibration and test data based on plotcal parameter
  Tt <- if (plotcal) rbind(T_cal[, model$lvs], TT) else TT
  
  # Determine dimension label prefix (PC for PCA, LV for PLS)
  dim <- ifelse(!is.null(model$type) && model$type == "PLS", "LV", "PC")
  
  # Initialize plot list
  figH <- list()
  
  # Generate bar plots for single LV or when plottype is "Bars"
  if (plottype == "Bars" || A == 1) {
    for (i in seq_along(model$lvs)) {
      fig <- plotVec(Tt[, i],
                    EleLabel = label,
                    ObsClass = classes,
                    XYLabel = c("", sprintf("Scores %s %d (%.0f%%)", dim, model$lvs[i], 100*d[i]/model$var)),
                    Color = color)
      figH[[length(figH) + 1]] <- fig + ggtitle(tit)
    }
  } 
  # Generate scatter plots for multiple LVs
  else if (plottype == "Scatter") {
    for (i in 1:(length(model$lvs)-1)) {
      for (j in (i+1):length(model$lvs)) {
        fig <- plotScatter(cbind(Tt[, i], Tt[, j]),
                          EleLabel = label,
                          ObsClass = classes,
                          XYLabel = c(sprintf("Scores %s %d (%.0f%%)", dim, model$lvs[i], 100*d[i]/model$var),
                                     sprintf("Scores %s %d (%.0f%%)", dim, model$lvs[j], 100*d[j]/model$var)),
                          BlurIndex = blur,
                          Color = color)
        figH[[length(figH) + 1]] <- fig + ggtitle(tit)
      }
    }
  }
  
  return(figH)
}