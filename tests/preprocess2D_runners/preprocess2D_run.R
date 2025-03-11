# This script processes a dataset using the preprocess2D function in R.
#
# It takes command-line arguments to specify the dataset and optional parameters:
#
# Usage: Rscript run_preprocess2D.R <dataset> [Preprocessing] [Weights]
#   - <dataset>: Path to the dataset (must be a CSV file).
#   - [Preprocessing]: (Optional) Integer value {0, 1, 2, 3} specifying the preprocessing type.
#   - [Weights]: (Optional) A numeric vector specifying feature weights.
#
# Argument handling:
# - If only the dataset is provided, the default preprocessing type is 2 (auto-scaling),
#   and all weights are set to 1.
# - If the second argument is a valid preprocessing type, it is used; otherwise, it is
#   interpreted as the weights vector.
# - If both optional parameters are given, the first is treated as the preprocessing type
#   and the second as the weights vector.
#
# Example usage:
#   Rscript run_preprocess2D.R dataset.csv             # Default settings
#   Rscript run_preprocess2D.R dataset.csv 1           # Preprocessing = 1
#   Rscript run_preprocess2D.R dataset.csv "0.5,1.5,1.0,0.8"  # Custom weights
#   Rscript run_preprocess2D.R dataset.csv 1 "0.5,1.5,1.0,0.8"  # Both params

args <- commandArgs(trailingOnly=TRUE)

# Ensure at least one argument is provided (the dataset)
if (length(args) < 1) {
  stop("Not enough arguments. Usage: Rscript run_preprocess2D.R <dataset> [Preprocessing] [Weights]")
}

# Take first argument (It should be the dataset)
dataset <- args[1]

# Check if the dataset is a CSV file
if (!grepl("\\.csv$", dataset)) {
  stop("Dataset must be a CSV file")
}

# Load the preprocess2D function
source("../R/preprocess2D.R")

# Read data from the CSV file
X <- as.matrix(read.csv(dataset, header=FALSE))
M <- ncol(X)  # Number of columns

# Default values
preprocessing <- 2      # Default: auto-scaling
weights <- rep(1, M)    # Default: vector of ones

# Parse optional parameters
paramList <- list()

if (length(args) >= 2) {
  val <- suppressWarnings(as.numeric(args[2]))
  if (!is.na(val) && val %in% c(0, 1, 2, 3)) {
    preprocessing <- val
    paramList$Preprocessing <- preprocessing
  } else {
    weights <- as.numeric(strsplit(args[2], ",")[[1]])
    if (length(weights) != M) {
      stop("Invalid weights vector. Must be a numeric vector of length equal to the number of columns in the dataset.")
    }
    paramList$Weights <- weights
  }
}

if (length(args) >= 3) {
  weights <- as.numeric(strsplit(args[3], ",")[[1]])
  if (length(weights) != M) {
    stop("Invalid weights vector. Must be a numeric vector of length equal to the number of columns in the dataset.")
  }
  paramList$Weights <- weights
}

# Run the preprocess2D function with optional parameters
result <- do.call(preprocess2D, c(list(X), paramList))

# Save the results to CSV files
write.table(result$xcs, "preprocess2D_r.csv", row.names=FALSE, col.names=FALSE, sep=",")
write.table(t(result$average), "average_r.csv", row.names=FALSE, col.names=FALSE, sep=",")
write.table(t(result$scale), "scale_r.csv", row.names=FALSE, col.names=FALSE, sep=",")
