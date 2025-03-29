parglmVS_run <- function(...) {
  # This script runs the parglmVS function and saves the results to a CSV file.
  #
  # It takes command-line arguments to specify the input data and optional parameters:
  #
  # Usage: parglmVS_run <X> <F> [Name-Value Pairs]
  #   - X: Path to the data matrix CSV file OR R expression to generate the data matrix.
  #   - F: Path to the design matrix CSV file OR R expression to generate the design matrix.
  #
  # Optional Parameters:
  #   - Model: Model type ('linear', 'interaction', 'full', or custom interactions).
  #   - Preprocessing: Preprocessing type (0: none, 1: mean-centering, 2: auto-scaling).
  #   - Permutations: Number of permutations (default: 1000).
  #   - Ts: Test statistic (0: SSQ, 1: F-value, 2: F-ratio with hierarchy).
  #   - Ordinal: Whether factors are ordinal (default: rep(0, ncol(F))).
  #   - Fmtc: Multiple test correction (0: none, 1: Bonferroni, 2: Holm, 3: Benjamini-Hochberg, 4: Q-value).
  #   - Coding: Coding type (0: sum/deviation, 1: reference).
  #   - Nested: Nested factors (default: NULL. Provide pairs numbers separated by commas (e.g: 2,1,3,2)).
  #
  # Example usage in R:
  #   Rscript parglmVS_run.R 'matrix(runif(4000), nrow=40, ncol=100)' 'matrix(rnorm(40), nrow=40, ncol=1)'
  #   Rscript parglmVS_runners/parglmVS_run.R 'matrix(runif(4000), nrow=40, ncol=100)' 'matrix(rnorm(40), nrow=40, ncol=1)'   Model "interaction"   Preprocessing 1   Permutations 500   Ts 2   Fmtc 3
  #
  # You can also replace 'matrix(runif(4000), nrow=40, ncol=100)' and 'matrix(rnorm(40), nrow=40, ncol=1)' with the paths to CSV files.
  #
  # For Nested parameter, provide pairs of numbers separated by commas (e.g., '2,1,3,2').
  #
  # Outputs:
  #   - Saves the results as 'parglmVS_r.csv'

  # Parse command-line arguments
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 2) {
    stop(paste("Not enough arguments. Usage: parglmVS_run <X> <F> [Name-Value Pairs]\n\n",
               "Optional Parameters:\n",
               "- Model: Model type ('linear', 'interaction', 'full', or custom interactions).\n",
               "- Preprocessing: Preprocessing type (0: none, 1: mean-centering, 2: auto-scaling).\n",
               "- Permutations: Number of permutations (default: 1000).\n",
               "- Ts: Test statistic (0: SSQ, 1: F-value, 2: F-ratio with hierarchy).\n",
               "- Ordinal: Whether factors are ordinal (default: rep(0, ncol(F))).\n",
               "- Fmtc: Multiple test correction (0: none, 1: Bonferroni, 2: Holm, 3: Benjamini-Hochberg, 4: Q-value).\n",
               "- Coding: Coding type (0: sum/deviation, 1: reference).\n",
               "- Nested: Nested factors (default: NULL. Format: Nested 2,1,3,2)."))
  }

  # Function to check if a string looks like a file path
  is_file_path <- function(x) {
    grepl("\\.csv$", x) || grepl("/", x) || grepl("\\\\", x)
  }

  # Parse X
  argX <- args[1]
  if (is_file_path(argX)) {
    tryCatch({
      X <- as.matrix(read.csv(argX, header = FALSE))
      cat(paste("Loaded X from:", argX, "\n"))
    }, error = function(e) {
      stop(paste("Error loading X from CSV file:", e$message))
    })
  } else {
    tryCatch({
      X <- eval(parse(text = argX))
      cat(paste("Evaluated X:", argX, "\n"))
    }, error = function(e) {
      stop(paste("Error evaluating X:", e$message))
    })
  }

  # Parse F
  argF <- args[2]
  if (is_file_path(argF)) {
    tryCatch({
      F <- as.matrix(read.csv(argF, header = FALSE))
      cat(paste("Loaded F from:", argF, "\n"))
    }, error = function(e) {
      stop(paste("Error loading F from CSV file:", e$message))
    })
  } else {
    tryCatch({
      F <- eval(parse(text = argF))
      cat(paste("Evaluated F:", argF, "\n"))
    }, error = function(e) {
      stop(paste("Error evaluating F:", e$message))
    })
  }

  # Set default values for optional arguments
  optionalArgs <- list(
    model = "linear",
    preprocessing = 2,
    permutations = 1000,
    Ts = 1,
    ordinal = rep(0, ncol(F)),
    Fmtc = 0,
    coding = 0,
    nested = NULL
  )

  # Parse optional name-value pairs
  if (length(args) > 2) {
    for (i in seq(3, length(args), by = 2)) {
      key <- tolower(args[i])  # Convert key to lowercase
      value <- args[i + 1]

      # Handle keys explicitly that need to preserve case
      if (key == "preprocessing" || key == "permutations" || key == "model" || key == "coding" || key == "nested" || key == "ordinal") {
        if (key == "preprocessing" || key == "permutations" || key == "ordinal") {
          value <- as.integer(value)
        } else if (key == "model") {
          value <- as.character(value)
        } else if (key == "nested") {
          numbers <- as.numeric(strsplit(value, ",")[[1]])
          if(length(numbers) %% 2 != 0) {
            stop("Nested parameter must contain pairs of numbers (e.g., '2,1,3,2')")
          }
          value <- matrix(numbers, ncol = 2, byrow = TRUE)
        }
        optionalArgs[[key]] <- value
      } else if (key == "ts") {  # Handle Ts explicitly
        optionalArgs[["Ts"]] <- as.integer(value)
      } else if (key == "fmtc") {  # Handle Fmtc explicitly
        optionalArgs[["Fmtc"]] <- as.integer(value)
      } 
    }
  }

  # Adjust ordinal and coding dimensions
  if (length(optionalArgs$coding) == 1 && ncol(F) > 1) {
    optionalArgs$coding <- rep(optionalArgs$coding, ncol(F))
  }

  if (length(optionalArgs$ordinal) == 1 && ncol(F) > 1) {
    optionalArgs$ordinal <- rep(optionalArgs$ordinal, ncol(F))
  }

  # Load the parglmVS function
  source("../R/parglmVS.R")

  # Load the required packages
  source("../R/createDesign.R")
  source("../R/preprocess2D.R")

  # Call parglmVS function with optional arguments
  result <- parglmVS(X, F, model = optionalArgs$model, preprocessing = optionalArgs$preprocessing,
                     permutations = optionalArgs$permutations, ts = optionalArgs$Ts,
                     ordinal = optionalArgs$ordinal, fmtc = optionalArgs$Fmtc,
                     coding = optionalArgs$coding, nested = optionalArgs$nested)

  # Save results to CSV file
  write.table(result$T[, 2:ncol(result$T)], file = "parglmVS_r.csv", sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, na = "NA")
}

# Run the parglmVS_run function
parglmVS_run()
