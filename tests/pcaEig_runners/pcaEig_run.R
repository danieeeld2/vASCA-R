pcaEig_run <- function(...) {
  # This function runs pcaEig function and saves the results to a CSV file.
  #
  # Usage:
  #   pcaEig_run(X, ...):
  #     - X: A matrix or path to a CSV file
  #     - Optional arguments can be passed for additional parameters like PCs.
  #
  # Optional Parameters:
  #   - 'PCs': Principal Components to consider (default: all PCs)
  #
  # Example usage in R:
  #   RScript pcaEig_run.R 'matrix(runif(4000), nrow=40, ncol=100)'  # Pass a matrix
  #   RScript pcaEig_run.R 'matrix(runif(4000), nrow=40, ncol=100)' PCs=c(1,2,3)  # Pass a matrix with specified PCs
  #
  # You can also replace the matrix with a path to a CSV file:
  #   RScript pcaEig_run.R 'path/to/data.csv'  # Pass a CSV file
  #   RScript pcaEig_run.R 'path/to/data.csv' PCs=c(1,2,3)  # Pass a CSV file with specified PCs
  #
  # Outputs:
  #   - Saves the results as 'pcaEig_r.csv'
  #   - Results include: scores, loads, variance, and other model information

  # Check there is at least one argument
  if (length(commandArgs(trailingOnly = TRUE)) < 1) {
    stop("Not enough arguments. Usage: pcaEig_run(X, ...)\n")
  }

  # Take first argument as X
  Xs <- commandArgs(trailingOnly = TRUE)[1]

  # Check if X is a file or a matrix
  if (file.exists(Xs)) {
    # Load data from CSV file
    X <- as.matrix(read.csv(Xs, header = FALSE))
    cat("Loaded X from:", Xs, "\n")
  } else {
    # If X is a string representation of the matrix, evaluate it
    X <- eval(parse(text = Xs))
    cat("Evaluated X:", Xs, "\n")
  }

  # Parse optional arguments
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1) {
    pcs <- NULL
    if (length(args) != 3) {
      stop("Invalid number of arguments. Usage: pcaEig_run(X, PCs)\n")
    }
    
    pcs_values <- args[3]
    # Check format
    if (grepl("^[0-9]+:[0-9]+$", pcs_values)) {
      range_vals <- as.numeric(unlist(strsplit(pcs_values, ":")))
      pcs <- seq(range_vals[1], range_vals[2]) 
    } else {
      pcs <- as.numeric(pcs_values)
    }
  } else {
    pcs <- NULL
  }

  # Load the pcaEig function
  source("../R/pcaEig.R")

  # Call pcaEig function
  model <- pcaEig(X, PCs = pcs)

  # Extract relevant results from the pcaEig model
  scores <- model$scores
  loadings <- model$loads
  variance <- model$var  

  # Open the file and write the values in the requested format
  write.table(variance, "pcaEig_r.csv", sep = ",", row.names = FALSE, col.names = FALSE)
  write.table(scores, "pcaEig_r.csv", sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)
  write.table(loadings, "pcaEig_r.csv", sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE)

  cat("Results saved to pcaEig_r.csv\n")
}

# Run the function
pcaEig_run()