pcaEig_run <- function(X, ...) {
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

  # Check if X is a file or a matrix
  if (file.exists(X)) {
    # Load data from CSV file
    X <- as.matrix(read.csv(X, header = FALSE))
    cat("Loaded X from:", X, "\n")
  } else {
    # If X is a string representation of the matrix, evaluate it
    X <- eval(parse(text = X))
    cat("Evaluated X:", X, "\n")
  }
  
  # Parse optional arguments
  args <- list(...)
  pcs <- args$PCs
  
  if (is.null(pcs)) {
    pcs <- 1:ncol(X)  # Default: all PCs
  }

  # Load the pcaEig function
  source("../R/pcaEig.R")
  
  # Call pcaEig function
  model <- pcaEig(X, PCs = pcs)
  
  # Extract relevant results from the pcaEig model
  scores <- model$scores
  loadings <- model$loadings
  variance <- model$variance  # Adjust this if your pcaEig function stores it differently
  
  # Save the results to a CSV file
  write.csv(data.frame(type = "PCA", value = "model"), "pcaEig_r.csv", row.names = FALSE)
  write.csv(data.frame(type = "variance", value = variance), "pcaEig_r.csv", append = TRUE, row.names = FALSE)
  
  # Write scores to CSV
  scores_df <- as.data.frame(scores)
  write.csv(scores_df, "pcaEig_r.csv", append = TRUE, row.names = FALSE)
  
  # Write loadings to CSV
  loadings_df <- as.data.frame(loadings)
  write.csv(loadings_df, "pcaEig_r.csv", append = TRUE, row.names = FALSE)
  
  cat("Results saved to pcaEig_r.csv\n")
}

# Run the function 
pcaEig_run()