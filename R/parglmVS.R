#################### Auxiliary function for interactions ####################

# Generates all possible interactions of a given order from a set of factors.
allinter <- function(factors, order) {
  if (order > 2) {
    interactions <- allinter(factors, order - 1)
    for (i in 1:length(interactions)) {
      for (j in factors[which(factors > max(interactions[[i]]))]) {
        interactions[[length(interactions) + 1]] <- c(interactions[[i]], j)
      }
    }
  } else {
    interactions <- list()
    for (i in factors) {
      for (j in factors[which(factors > i)]) {
        interactions[[length(interactions) + 1]] <- c(i, j)
      }
    }
  }
  return(interactions)
}

# Computes interaction terms for a given dataset D based on specified factors.
computaDint <- function(interactions, factors, D) { # Compute coding matrix
  if (length(interactions) > 1) {
    deepD <- computaDint(interactions[2:length(interactions)], factors, D)
    Dout <- matrix(0, nrow = nrow(D), ncol = 0)
    for (k in factors[[interactions[1]]]$Dvars) {
      for (l in 1:ncol(deepD)) {
        Dout <- cbind(Dout, D[,k] * deepD[,l])
      }
    }
  } else {
    Dout <- D[, factors[[interactions]]$Dvars]
  }
  return(Dout)
}

###############################################################################

###############################################################################
# Parallel General Linear Model to obtain multivariate factor and interaction 
# matrices in a crossed experimental design and permutation test for incremental
# multivariate statistical significance that allows variable selection. This 
# is the basis of VASCA (Variable-selection ASCA). Missing data is considered.
#
# T = parglmVS(X, F)   # minimum call
#
# See also: parglm, parglmMC, asca, apca, createDesign
#
#
# INPUTS:
#
# X: [NxM] billinear data set for model fitting, where each row is a
# measurement, each column a variable
#
# F: [NxF] design matrix, cell or array, where columns correspond to 
# factors and rows to levels.
#
#
# Optional INPUTS (parameters):
#
# 'Model': This paremeter is similar to 'model' of anovan. It could be:
#       'linear': only main effects are provided (by default)
#       'interaction': two order interactions are provided
#       'full': all potential interactions are provided
#       [1x1]: maximum order of interactions considered
#       [ix2]: array with two order interactions
#       cell: with each element a vector of factors
#
# 'Preprocessing': [1x1] preprocesing:
#       0: no preprocessing 
#       1: mean-centering 
#       2: auto-scaling (default)  
#
# 'Permutations': [1x1] number of permutations (1000 by default)
#
# 'Ts': [1x1] Use SSQ (0) or the F-value (otherwise, by default) as test statistic  
#       0: Sum-of-squares of the factor/interaction
#       1: F-ratio of the SS of the factor/interaction divided by the SS of 
#       the residuals (by default)
#       2: F-ratio following the factors/interactions hierarchy (only for
#       random models or unconstrained mixed model, see Montogomery)
#
# 'Ordinal': [1xF] whether factors are nominal or ordinal
#       0: nominal (default)
#       1: ordinal
# 
# 'Fmtc': [1x1] correct for multiple-tesis when multifactorial (multi-way)
# analysis
#       0: do not correct (default)
#       1: Bonferroni 
#       2: Holm step-up or Hochberg step-down
#       3: Benjamini-Hochberg step-down (FDR)
#       4: Q-value from Benjamini-Hochberg step-down
#
# 'Coding': [1xF] type of coding of factors
#       0: sum/deviation coding (default)
#       1: reference coding (reference is the last level)
#
# 'Nested': [nx2] pairs of neted factors, e.g., if factor 2 is nested in 1,
#   and 3 in 2, then nested = [1 2; 2 3]
#
#
# OUTPUTS:
#
# T (table): ANOVA-like output table
#
# parglmoMV (structure): structure with the factor and interaction
# matrices, p-values (corrected, depending on fmtc) and explained variance 
###############################################################################

parglmVS <- function(X, F, ...) {
  
  ######################### Arguments Checking #########################

  # Set default values
  routine <- sys.calls()
  stopifnot(nargs() >= 2)
  N <- nrow(X)
  M <- ncol(X)

  nFactors <- ncol(F)  # number of factors

  # Introduce optional inputs as parameters (name-value pair)
  args <- list(...)
  model <- if ("model" %in% names(args)) args$model else "linear"
  prep <- if ("preprocessing" %in% names(args)) args$preprocessing else 2
  nPerm <- if ("permutations" %in% names(args)) args$permutations else 1000
  ts <- if ("ts" %in% names(args)) args$ts else 1
  ordinal <- if ("ordinal" %in% names(args)) args$ordinal else rep(0, ncol(F))
  fmtc <- if ("fmtc" %in% names(args)) args$fmtc else 0
  coding <- if ("coding" %in% names(args)) args$coding else rep(0, ncol(F))
  nested <- if ("nested" %in% names(args)) args$nested else NULL

  if (identical(model, "linear")) {
    interactions <- NULL
  }

  f <- 1:nFactors

  if (!is.null(nested)) f <- f[-nested[, 2]]
  if (identical(model, "interaction")) {
    interactions <- allinter(f, 2)
  }

  if (identical(model, "full")) {
    interactions <- allinter(f, length(f))
  }

  if (is.numeric(model) && length(model) == 1 && model >= 2 && model <= nFactors) {
    interactions <- allinter(f, model)
  }

  if (is.numeric(model) && !length(model) == 1) {
    for (i in 1:nrow(model)) {
      interactions[[i]] <- model[i, ]
    }
  }

  if (is.list(model)) {
    interactions <- model
  }

  # Validate dimensions of input data
  if (length(prep) != 1) stop(paste("Dimension Error: parameter 'Preprocessing' must be 1-by-1. Type 'help ", routine[1]$name, "' for more info.", sep = ""))
  if (length(nPerm) != 1) stop(paste("Dimension Error: parameter 'Permutations' must be 1-by-1. Type 'help ", routine[1]$name, "' for more info.", sep = ""))
  if (length(ts) != 1) stop(paste("Dimension Error: parameter 'Ts' must be 1-by-1. Type 'help ", routine[1]$name, "' for more info.", sep = ""))
  if (length(ordinal) != ncol(F)) stop(paste("Dimension Error: parameter 'Ordinal' must be 1-by-F. Type 'help ", routine[1]$name, "' for more info.", sep = ""))
  if (length(fmtc) != 1) stop(paste("Dimension Error: parameter 'Fmtc' must be 1-by-1. Type 'help ", routine[1]$name, "' for more info.", sep = ""))
  if (length(coding) != ncol(F)) stop(paste("Dimension Error: parameter 'Coding' must be 1-by-F. Type 'help ", routine[1]$name, "' for more info.", sep = ""))

  ########################### Main Code ###########################

  # Number of interactions
  if (is.null(interactions) || !is.matrix(interactions) || nrow(interactions) == 0) {
    nInteractions <- 0
    interactions <- matrix()
  } else {
      nInteractions <- nrow(interactions)
  }

  # Correct for multiple tests if 'fmtc' is TRUE
  if (fmtc) {
      mtcc <- nFactors + nInteractions  
  } else {
      mtcc <- 1
  }

  # Initialize matrices for sum of squares and F-values
  SSQFactors <- array(0, dim = c(nPerm * mtcc + 1, nFactors, M))  # sum of squares for factors
  SSQInteractions <- array(0, dim = c(nPerm * mtcc + 1, nInteractions, M))  # sum of squares for interactions
  SSQresiduals <- array(0, dim = c(nPerm * mtcc + 1, M))  # sum of squares for residuals
  FFactors <- array(0, dim = c(nPerm * mtcc + 1, nFactors, M))  # F-value for factors
  FInteractions <- array(0, dim = c(nPerm * mtcc + 1, nInteractions, M))  # F-value for interactions
  pFactor <- matrix(0, nFactors, M)  # p-values for factors
  pInteraction <- matrix(0, nInteractions, M)  # p-values for interactions

  # Create parglmo structure (structure to store all the key-information)
  parglmo <- list(factors = list(), interactions = list(), data = NULL, prep = NULL, scale = NULL, design = NULL, nFactors = 0, nInteractions = 0, nPerm = 0, ts = NULL, ordinal = NULL, fmtc = NULL, coding = NULL, nested = NULL)
  parglmo$factors <- vector("list", nFactors)
  parglmo$interactions <- vector("list", nInteractions)

  # Preprocess the data
  preprocessed_data <- preprocess2D(X, Preprocessing = prep)
  Xs <- preprocessed_data$xcs
  m <- preprocessed_data$average
  dt <- preprocessed_data$scale
  X <- X / (matrix(1, nrow = nrow(X), ncol = 1) %*% dt)

  # Initialize parglmo structure
  parglmo$data <- X
  parglmo$prep <- prep
  parglmo$scale <- dt
  parglmo$design <- F
  parglmo$nFactors <- nFactors
  parglmo$nInteractions <- nInteractions
  parglmo$nPerm <- nPerm
  parglmo$ts <- ts
  parglmo$ordinal <- ordinal
  parglmo$fmtc <- fmtc
  parglmo$coding <- coding
  parglmo$nested <- nested

  ######################### Design Matrix Construction #########################
  # This section constructs the design matrix (D) for a regression model. 
  # It processes ordinal and categorical factors, handling nested structures 
  # and applying appropriate coding schemes for factor levels.
  ##############################################################################

  n <- 1
  D <- matrix(1, nrow = nrow(X), ncol = 1) # Initialize D matrix with a column of 1s
  for (f in 1:nFactors) {
    # Process ordinal factors
    if (ordinal[f]) {
      # If the factor is ordinal, apply mean-centering preprocessing,  
      # add the processed values to the design matrix D,  
      # and update the factor's column indices. 
      processed_F <- preprocess2D(F[, f, drop = FALSE], Preprocessing = 1)
      D <- cbind(D, processed_F$xcs)
      parglmo$factors[[f]]$Dvars <- n + 1
      n <- n + 1
    } else { # Process categorical factors
      # If not nested, apply the appropriate coding scheme for the factor levels
      if (is.null(nested) || length(which(nested[,2] == f)) == 0) { 
        # For factors that are not nested, create dummy variables for each level, 
        # expand the design matrix D, and apply the appropriate coding for the factor's levels.
        uF <- unique(F[,f])
        parglmo$nLevels[f] <- length(uF)
        n_levels <- length(uF)

        if (n_levels > 1) {
          # Expand D matrix
          D <- cbind(D, matrix(0, nrow = nrow(D), ncol = n_levels - 1))

          # Assign 1s for dummy variables
          for (i in 2:n_levels) {
            rows <- which(F[,f] == uF[i])
            D[rows, n + i - 1] <- 1
          }

          parglmo$factors[[f]]$Dvars <- (n + 1):(n + n_levels - 1)

          # Apply coding scheme
          rows_base <- which(F[,f] == uF[1])
          if (coding[f] == 1) {
            D[rows_base, parglmo$factors[[f]]$Dvars] <- 0
          } else {
            D[rows_base, parglmo$factors[[f]]$Dvars] <- -1
          }

          n <- n + n_levels - 1
        } else {
          # If there is only one level, set the number of levels to 0 because there are no dummy variables
          parglmo$factors[[f]]$Dvars <- numeric(0) 
        }
      } else { # if nested
        # For nested factors, create dummy variables for each level within each reference factor,
        # expand the design matrix D, and apply the appropriate coding for each nested level.
        ind <- which(nested[,2] == f)
        ref <- nested[ind,1]
        urF <- unique(F[,ref])
        parglmo$nLevels[f] <- 0
        parglmo$factors[[f]]$Dvars <- numeric(0)

        for (j in 1:length(urF)) {
          rind <- which(F[,ref] == urF[j])
          uF <- unique(F[rind,f])
          n_levels_nested <- length(uF)
          parglmo$nLevels[f] <- parglmo$nLevels[f] + n_levels_nested

          if (n_levels_nested > 1) {
            # Expand D matrix
            D <- cbind(D, matrix(0, nrow = nrow(D), ncol = n_levels_nested - 1))

            # Assign 1s for dummy variables
            for (i in 2:n_levels_nested) {
              nested_rows <- rind[which(F[rind,f] == uF[i])]
              D[nested_rows, n + i - 1] <- 1
            }

            parglmo$factors[[f]]$Dvars <- c(parglmo$factors[[f]]$Dvars, (n + 1):(n + n_levels_nested - 1))

            # Apply coding scheme
            rows <- rind[which(F[rind,f] == uF[1])]
            if (coding[f] == 1) {
              D[rows, n + (1:(n_levels_nested - 1))] <- 0
            } else {
              D[rows, n + (1:(n_levels_nested - 1))] <- -1
            }

            n <- n + n_levels_nested - 1
          }
        }
      }
    }
  }

  # If there are interactions, compute the corresponding interaction terms and expand the design matrix D.
  # Store the interaction variables and update the design matrix with the interaction terms.

  if (nInteractions > 0) {
    for (i in 1:nInteractions) {
      Dout <- computaDint(interactions[[i]], parglmo$factors, D)
      D <- cbind(D, Dout)
      parglmo$interactions[[i]]$Dvars <- (n+1):ncol(D)
      parglmo$interactions[[i]]$factors <- interactions[[i]]
      n <- ncol(D)
    }
  }

  # Calculate the degrees of freedom (df) for each factor and interaction.
  # Adjust the residual degrees of freedom (Rdf) accordingly.
  # If the residual degrees of freedom are negative, print a warning.

  Tdf <- nrow(X)
  Rdf <- Tdf - 1
  df <- numeric(nFactors)

  for (f in 1:nFactors) {
    if (ordinal[f]) {
      df[f] <- 1
    } else {
      df[f] <- length(parglmo$factors[[f]]$Dvars)
    }
    Rdf <- Rdf - df[f]
  }

  dfint <- numeric(0)
  if (nInteractions > 0) {
    dfint <- numeric(nInteractions)
    for (i in 1:nInteractions) {
      dfint[i] <- prod(df[parglmo$interactions[[i]]$factors])
      Rdf <- Rdf - dfint[i]
    }
  }

  if (Rdf < 0) {
    cat("Warning: degrees of freedom exhausted\n")
    return(NULL)
  }
  
  # Handle missing data by replacing NaN values.
  # For each row with missing data, compute a conditional mean replacement (CMR)
  # based on rows with the same values in the design matrix D. 
  # If CMR is not possible, use unconditional mean replacement for the missing values.

  r_c <- which(is.na(X), arr.ind = TRUE)
  r <- r_c[, 1]  # Rows with NaN
  c <- r_c[, 2]  # Columns with NaN
  Xnan <- X
  ru <- unique(r)

  for (i in 1:length(ru)) {
    ind <- which(r == ru[i])
    ref_row <- D[r[ind[1]], ]
    diff_matrix <- sweep(D, 2, ref_row, "-")^2
    row_sums <- rowSums(diff_matrix)
    ind2 <- which(row_sums == 0)
    
    for (j in 1:length(c[ind])) {
      curr_col <- c[ind[j]]
      ind3 <- which(is.na(X[ind2, curr_col]))
      
      if (length(ind2) > length(ind3)) {
        # use conditional mean replacement
        X[r[ind[j]], curr_col] <- mean(X[ind2, curr_col], na.rm = TRUE)
      } else {
        # use unconditional mean replacement if CMR not possible
        X[r[ind[j]], curr_col] <- mean(X[, curr_col], na.rm = TRUE)
      }
    }
  }

  parglmo$data <- X
  parglmo$Xnan <- Xnan
  SSQX <- colSums(X^2, na.rm = TRUE)

  # Load MASS package if not already loaded (install if necessary)
  # Required for ginv function

  if (!requireNamespace("MASS", quietly = TRUE)) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    install.packages("MASS")
  }
  library(MASS)

  # GLM model calibration using least squares (LS) method, considering only fixed factors.
  # Compute the generalized inverse of the design matrix and calculate the regression coefficients (B).
  # The residuals (Xresiduals) are obtained by subtracting the fitted values from the original data.
  # Store the design matrix (D), regression coefficients (B), and the fitted model mean in the parglmo object.

  pD <- ginv(t(D) %*% D) %*% t(D)
  B <- pD %*% X
  Xresiduals <- X - D %*% B
  parglmo$D <- D
  parglmo$B <- B
  parglmo$mean <- parglmo$D %*% parglmo$B[,1]

  # Create Effect Matrices
  parglmo$inter <- D[,1] %*% t(B[1,])
  SSQinter <- colSums(parglmo$inter^2)
  SSQresiduals[1, ] <- colSums(Xresiduals^2)

  for (f in 1:nFactors) {
    Dvars <- parglmo$factors[[f]]$Dvars
    parglmo$factors[[f]]$matrix <- D[,Dvars,drop=FALSE] %*% B[Dvars,,drop=FALSE]
    SSQFactors[1,f,] <- colSums(parglmo$factors[[f]]$matrix^2)
    FFactors[1,f,] <- (SSQFactors[1,f,]/df[f]) / (SSQresiduals[1,]/Rdf)
  }

  # Interactions
  if (length(parglmo$interactions) > 0) {
    for (i in 1:nInteractions) {
        parglmo$interactions[[i]]$matrix <- D[, parglmo$interactions[[i]]$Dvars] %*% B[parglmo$interactions[[i]]$Dvars, ]
        SSQInteractions[1, i, ] <- colSums(parglmo$interactions[[i]]$matrix^2)
        FInteractions[1, i, ] <- (SSQInteractions[1, i, ] / dfint[i]) / (SSQresiduals[1, ] / Rdf)
    }
  } 

  SSQinter <- matrix(colSums(parglmo$inter^2), nrow = 1)  
  SSQX <- matrix(colSums(X^2, na.rm = TRUE), nrow = 1)  

  # Calculate the percentage effects for different components (interactions, factors, and residuals).
  # If interactions exist, compute the sum of squares (SSQ) for interactions, factors, and residuals,
  # and adjust their dimensions to match. Then, compute the effects as a percentage of the total sum of squares.
  # If no interactions are present, the calculation excludes interaction effects.
  # Finally, store the residuals from the GLM model for further analysis.

  if (nInteractions) {
    # Vectorize the SSQ values
    SSQFactors_first <- as.vector(SSQFactors[1, 1, ])
    SSQInteractions_first <- as.vector(SSQInteractions[1, 1, ])
    
    # Adjust dimensions to match
    len <- max(length(SSQinter), length(SSQFactors_first), length(SSQInteractions_first), length(SSQresiduals[1,]))
    
    # Adjust the denominator to have the correct length
    denom <- rep(SSQX, length.out = len)
    
    # Calculate the percentage effects
    parglmo$effects <- 100 * (cbind(
      matrix(SSQinter, nrow = len, ncol = 1),
      matrix(SSQFactors_first, nrow = len, ncol = 1),
      matrix(SSQInteractions_first, nrow = len, ncol = 1),
      matrix(SSQresiduals[1,], nrow = len, ncol = 1)
    ) / matrix(denom, nrow = len, ncol = 4))
    
  } else {
    # Vectorize the SSQ values
    SSQFactors_first <- as.vector(SSQFactors[1, 1, ])
    
    # Adjust dimensions to match
    len <- max(length(SSQinter), length(SSQFactors_first), length(SSQresiduals[1,]))
    
    # Adjust the denominator to have the correct length
    denom <- rep(SSQX, length.out = len)
    
    # Calculate the percentage effects
    parglmo$effects <- 100 * (cbind(
      matrix(SSQinter, nrow = len, ncol = 1),
      matrix(SSQFactors_first, nrow = len, ncol = 1),
      matrix(SSQresiduals[1,], nrow = len, ncol = 1)
    ) / matrix(denom, nrow = len, ncol = 3))
  }

  parglmo$residuals <- Xresiduals
  
  # Perform permutations for statistical significance testing.
  # In each iteration, permute the rows of the dataset to create a new version of the data.
  # For each row with missing values (NaN), replace them with either the conditional or unconditional mean,
  # depending on the available data. The residuals of the permuted data are then calculated.
  # The sum of squared residuals for the permuted data is stored for further comparison with the original model.

  for (j in 1:nPerm*mtcc) {
    perms <- sample(nrow(Xnan))  # permuted data (permute whole rows)
    X <- Xnan[perms, ]
    r_c <- which(is.na(X), arr.ind = TRUE)
    r <- r_c[, 1]
    c <- r_c[, 2]
    ru <- unique(r)
    
    for (i in 1:length(ru)) {
      ind <- which(r == ru[i])
      ind2 <- which(apply(sweep(D, 2, D[r[ind[1]], ], "-")^2, 1, sum) == 0)
      
      for (f in 1:length(c[ind])) {
        ind3 <- which(is.na(X[ind2, c[ind[f]]]))
        
        if (length(ind2) > length(ind3)) {
          X[r[ind[f]], c[ind[f]]] <- mean(X[ind2, c[ind[f]]], na.rm = TRUE)  # use conditional mean replacement
        } else {
          X[r[ind[f]], c[ind[f]]] <- mean(X[, c[ind[f]]], na.rm = TRUE)  # use unconditional mean replacement if CMR not possible
        }
      }
    }
    
    B <- pD %*% X
    Xresiduals <- X - D %*% B
    SSQresiduals[1 + j, ] <- colSums(Xresiduals^2)
    
    # Calculate the effect of each factor for the permuted data.
    # For each factor, compute the contribution to the model by multiplying the design matrix (D) 
    # with the corresponding coefficients (B). The sum of squared factors (SSQf) and F-statistics (Ff) 
    # are computed to evaluate the significance of the factors' contribution to the model.
    # The results are stored for further analysis and comparison with the original model.

    factors <- vector("list", nFactors)
    SSQf <- matrix(0, nFactors, M)
    Ff <- matrix(0, nFactors, M)

    for (f in 1:nFactors) {
      factors[[f]]$matrix <- D[, parglmo$factors[[f]]$Dvars, drop = FALSE] %*% B[parglmo$factors[[f]]$Dvars, , drop = FALSE]
      SSQf[f, ] <- colSums(factors[[f]]$matrix^2)
      Ff[f, ] <- (SSQf[f, ]/df[f])/(SSQresiduals[1 + j, ]/Rdf)
    }
    
    SSQFactors[1 + j, , ] <- SSQf
    FFactors[1 + j, , ] <- Ff
    
    # Calculate the effect of each interaction for the permuted data.
    # For each interaction, compute its contribution to the model by multiplying the relevant columns 
    # of the design matrix (D) with the corresponding coefficients (B). The sum of squared interactions 
    # (SSQi) and F-statistics (Fi) are computed to evaluate the significance of the interactions' contribution.
    # The results are stored for further analysis and comparison with the original model.

    interacts <- vector("list", nInteractions)
    SSQi <- matrix(0, nInteractions, M)
    Fi <- matrix(0, nInteractions, M)
    
    if(length(parglmo$interactions) > 0){
      for (i in 1:nInteractions) {
        interacts[[i]]$matrix <- D[, parglmo$interactions[[i]]$Dvars] %*% B[parglmo$interactions[[i]]$Dvars, ]
        SSQi[i, ] <- colSums(interacts[[i]]$matrix^2)
        Fi[i, ] <- (SSQi[i, ]/dfint[i])/(SSQresiduals[1 + j, ]/Rdf)
      }
    }
    
    SSQInteractions[1 + j, , ] <- SSQi
    FInteractions[1 + j, , ] <- Fi
  }

  # Based on the 'ts' flag, select either the F-statistics or sum of squared effects for factors and interactions.
  # If 'ts' is TRUE, the F-statistics for factors and interactions are stored in 'tsFactors' and 'tsInteractions'.
  # Otherwise, the sum of squared effects (SSQ) for factors and interactions are selected.

  if (ts) {
    tsFactors <- FFactors
    tsInteractions <- FInteractions
  } else {
    tsFactors <- SSQFactors
    tsInteractions <- SSQInteractions
  }

  # Order variables by relevance
  ordFactors <- matrix(0, nrow = nFactors, ncol = M)
  ordInteractions <- matrix(0, nrow = nInteractions, ncol = M)

  # Iterating over each factor 'f' to process the F-statistics for factors
  for (f in 1:nFactors) {
    
    # Sorting the indices of 'tsFactors' for factor f in descending order
    ordFactors[f, ] <- order(tsFactors[1, f, ], decreasing = TRUE)
    
    # Calculating F-statistics for factor f using the sorted values
    for (var in 1:M) {
      FFactors[1, f, ordFactors[f, var]] <- (sum(SSQFactors[1, f, ordFactors[f, 1:var]], na.rm = TRUE) / df[f]) / 
                                            (sum(SSQresiduals[1, ordFactors[f, 1:var]], na.rm = TRUE) / Rdf)
    }
    
    # Processing permutations and calculating F-statistics for each permutation
    for (j in 1:(nPerm * mtcc)) {
      ord <- order(tsFactors[1 + j, f, ], decreasing = TRUE)  # Sorting for permutation j
      SSQFactors[1 + j, f, ] <- SSQFactors[1 + j, f, ord]     # Sorting SSQFactors according to 'ord'
      
      # Calculating F-statistics for permutation j and factor f
      for (var in 1:M) {
        FFactors[1 + j, f, var] <- (sum(SSQFactors[1 + j, f, 1:var], na.rm = TRUE) / df[f]) / 
                                  (sum(SSQresiduals[1 + j, ord[1:var]], na.rm = TRUE) / Rdf)
      }
    }
  }

  if(nInteractions > 0){
    for (i in 1:nInteractions) {
      # Sorting 'tsInteractions' for interaction i in descending order
      ordInteractions[i, ] <- order(tsInteractions[1, i, ], decreasing = TRUE)
      
      # Calculating F-statistics for interaction i
      for (var in 1:M) {
        FInteractions[1, i, ordInteractions[i, var]] <- (sum(SSQInteractions[1, i, ordInteractions[i, 1:var]], na.rm = TRUE) / dfint[i]) / 
                                                        (sum(SSQresiduals[1, ordInteractions[i, 1:var]], na.rm = TRUE) / Rdf)
      }
      
      # Processing permutations and calculating F-statistics for each permutation
      for (j in 1:(nPerm * mtcc)) {
        # Sorting 'tsInteractions' for permutation j and interaction i
        ord <- order(tsInteractions[1 + j, i, ], decreasing = TRUE)
        SSQInteractions[1 + j, i, ] <- SSQInteractions[1 + j, i, ord]
        
        # Calculating F-statistics for permutation j and interaction i
        for (var in 1:M) {
          FInteractions[1 + j, i, var] <- (sum(SSQInteractions[1 + j, i, 1:var], na.rm = TRUE) / dfint[i]) / 
                                          (sum(SSQresiduals[1 + j, ord[1:var]], na.rm = TRUE) / Rdf)
        }
      }
    }
  }

  parglmo$ordFactors <- ordFactors
  if (nInteractions > 0) {
    parglmo$ordInteractions <- ordInteractions
  }

  # Calculate multivariate p-values for factors and interactions
  # This block calculates the p-values based on the F-statistics or SSQ values for both factors and interactions,
  # using permutations to determine the significance. If 'ts' is TRUE, the p-values are calculated using the F-statistics
  # for each factor or interaction. If 'ts' is FALSE, the p-values are based on the cumulative sums of squares (SSQ).
  # The p-values are computed for each factor and interaction, considering all permutations and the appropriate degrees of freedom.

  # Calculate multivariate p-values
  for (f in 1:nFactors) {
    for (var in 1:M) {
      if (ts) {
        # Calculate p-value when ts is TRUE
        pFactor[f, ordFactors[f, var]] <- (sum(FFactors[2:(nPerm * mtcc + 1), f, var] >= 
                                              FFactors[1, f, ordFactors[f, var]]) + 1) / (nPerm * mtcc + 1)
      } else {
        # Calculate p-value when ts is FALSE
        pFactor[f, ordFactors[f, var]] <- (sum(rowSums(SSQFactors[2:(nPerm * mtcc + 1), f, 1:var, drop = FALSE], dims = 2) >= 
                                              sum(SSQFactors[1, f, ordFactors[f, 1:var]])) + 1) / (nPerm * mtcc + 1)
      }
    }
  }

  if(nInteractions > 0){
    for (i in 1:nInteractions) {
      for (var in 1:M) {
        if (ts) {
          # Calculate p-value when ts is TRUE
          pInteraction[i, ordInteractions[i, var]] <- (sum(FInteractions[2:(nPerm * mtcc + 1), i, var] >= 
                                                          FInteractions[1, i, ordInteractions[i, var]]) + 1) / (nPerm * mtcc + 1)
        } else {
          # Calculate p-value when ts is FALSE
          pInteraction[i, ordInteractions[i, var]] <- (sum(rowSums(SSQInteractions[2:(nPerm * mtcc + 1), i, 1:var, drop = FALSE], dims = 2) >= 
                                                          sum(SSQInteractions[1, i, ordInteractions[i, 1:var]])) + 1) / (nPerm * mtcc + 1)
        }
      }
    }
  }

  # Multiple test correction for several factors/interactions
  # This block applies a multiple test correction method to the p-values obtained for the factors and interactions.
  # The p-values are adjusted according to the specified correction method: 
  # 1) Bonferroni, 2) Holm/Hochberg, 3) Benjamini & Hochberg, or 4) Q-value from Benjamini & Hochberg.
  # The correction is applied based on the value of 'fmtc', and the p-values are stored in the 'parglmo$p' matrix.

  parglmo$p <- cbind(t(pFactor), t(pInteraction))
  if (mtcc > 1) {
    switch(fmtc,
      "1" = { # Bonferroni
        parglmo$p <- pmin(1, parglmo$p * mtcc)
      },
      "2" = { # Holm/Hochberg
        indx <- order(apply(parglmo$p, 2, min))
        for (ind in 1:mtcc) {
          parglmo$p[, indx[ind]] <- pmin(1, parglmo$p[, indx[ind]] * (mtcc - ind + 1))
        }
      },
      "3" = { # Benjamini & Hochberg
        mv <- apply(parglmo$p, 2, min)
        indmv <- apply(parglmo$p, 2, which.min)
        indx <- order(mv)
        parglmo$p[, indx[mtcc]] <- parglmo$p[, indx[mtcc]]
        for (ind in (mtcc - 1):1) {
          parglmo$p[indmv[indx[ind]], indx[ind]] <- pmin(1, min(parglmo$p[indmv[indx[ind]], indx[ind]] * mtcc / ind, 
                                                              parglmo$p[indmv[indx[ind + 1]], indx[ind + 1]]))
          parglmo$p[, indx[ind]] <- parglmo$p[, indx[ind]] * parglmo$p[indmv[indx[ind]], indx[ind]] / 
                                    parglmo$p[indmv[indx[ind]], indx[ind]]
        }
      },
      "4" = { # Q-value from Benjamini & Hochberg
        mv <- apply(parglmo$p, 2, min)
        indmv <- apply(parglmo$p, 2, which.min)
        indx <- order(mv)
        parglmo$p[, indx[mtcc]] <- parglmo$p[, indx[mtcc]]
        for (ind in (mtcc - 1):1) {
          parglmo$p[indmv[indx[ind]], indx[ind]] <- pmin(1, min(parglmo$p[indmv[indx[ind]], indx[ind]] * mtcc / ind, 
                                                              parglmo$p[indmv[indx[ind + 1]], indx[ind + 1]]))
          parglmo$p[, indx[ind]] <- parglmo$p[, indx[ind]] * parglmo$p[indmv[indx[ind]], indx[ind]] / 
                                    parglmo$p[indmv[indx[ind]], indx[ind]]
        }
      }
    )
  }
  
  # This section generates an ANOVA-like output table, summarizing the results of the analysis. 
  # It calculates the sum of squares (SSQ), degrees of freedom (DoF), mean squares (MSQ), 
  # F-statistics, and p-values for factors, interactions, and residuals, and creates a formatted table of the results.

  # Create the name vector starting with 'Mean'
  name <- c('Mean')

  # Add the names of the factors
  for (f in 1:nFactors) {
    name <- c(name, sprintf('Factor %d', f))
  }

  # Add the names of the interactions, checking that nInteractions > 0
  if (nInteractions > 0) {
    for (i in 1:nInteractions) {
      factors_str <- gsub(' ', '-', toString(parglmo$interactions[[i]]$factors))
      name <- c(name, sprintf('Interaction %s', factors_str))
    }
  }

  # Add 'Residuals' and 'Total'
  name <- c(name, 'Residuals', 'Total')

  # Calculate the sum of squares (SSQ) based on whether interactions exist
  if (nInteractions > 0) {
    SSQ_inter_sum <- sum(t(SSQinter))
    SSQ_factors_sum <- sum(aperm(SSQFactors[1,,, drop = FALSE], c(3, 2, 1)))
    SSQ_interactions_sum <- sum(aperm(SSQInteractions[1,,, drop = FALSE], c(3, 2, 1)))
    SSQ_residuals_sum <- sum(t(SSQresiduals[1, , drop = FALSE]))
    SSQ_x_sum <- sum(t(SSQX))
    SSQ <- c(SSQ_inter_sum, SSQ_factors_sum, SSQ_interactions_sum, SSQ_residuals_sum, SSQ_x_sum)
  } else {
    SSQ_inter_sum <- sum(t(SSQinter))
    SSQ_factors_sum <- sum(aperm(SSQFactors[1,,, drop = FALSE], c(3, 2, 1)))
    SSQ_residuals_sum <- sum(t(SSQresiduals[1, , drop = FALSE]))
    SSQ_x_sum <- sum(t(SSQX))
    SSQ <- c(SSQ_inter_sum, SSQ_factors_sum, SSQ_residuals_sum, SSQ_x_sum)
  }

  # Calculate means, degrees of freedom, mean squares, and F-statistics
  par <- c(colMeans(parglmo$effects), 100)
  DoF <- c(1, df, dfint, Rdf, Tdf)
  MSQ <- SSQ / DoF
  F_factors_max <- max(FFactors[1, 1,])
  if (any(dim(FInteractions) == 0)) {
    F_interactions_max <- NaN
  } else {
    F_interactions_max <- max(FInteractions[1, 1,])
  }
  F <- c(NaN, F_factors_max, F_interactions_max, NaN)
  pValue <- c(NaN, min(parglmo$p), NaN, NaN)

  # Create the ANOVA-like table
  T <- data.frame(
    Source = name,
    SumSq = SSQ,
    AvPercSumSq = par,
    df = DoF,
    MeanSq = MSQ,
    MaxF = F,
    minPvalue = pValue,
    stringsAsFactors = FALSE
  )

  # Return results as a list containing the table and the model
  results <- list(T = T, parglmo = parglmo)
  return(results)
}