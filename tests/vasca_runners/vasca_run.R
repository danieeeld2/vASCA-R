# This script runs the vasca function and saves the results to a JSON file.
#
# It takes command-line arguments to specify the input data and optional parameters:
#
# Usage: Rscript vasca_run.R <parglmVS> <siglev>
#   - <parglmVS>: The output structure from the parglmVS function (in JSON format).
#   - <siglev>: Significance level (0.01 by default).
#
# Example usage:
#   Rscript vasca_run.R parglmVS_output.json 0.05
#   Rscript vasca_run.R parglmVS_output.json  # uses default siglev of 0.01
#
# Outputs:
#   - Saves the results as 'vasca_r.json'

# Install jsonlite if not installed
# Install jsonlite if not installed
if (!requireNamespace("jsonlite", quietly = TRUE)) {
    install.packages("jsonlite", repos = "https://cloud.r-project.org")
}

library(jsonlite)

# Main function
vasca_run <- function() {
    args <- commandArgs(trailingOnly = TRUE)
    
    if (length(args) < 1) {
        stop("Not enough arguments. Usage: Rscript vasca_run.R <parglmVS> <siglev>")
    }
    
    # Read input JSON file
    parglmVS_path <- args[1]
    if (!file.exists(parglmVS_path)) {
        stop("parglmVS must be a valid file.")
    }
    
    parglmVS <- fromJSON(parglmVS_path)
    cat("Loaded parglmVS from:", parglmVS_path, "\n")
    
    # Read siglev or use default value
    siglev <- ifelse(length(args) > 1, as.numeric(args[2]), 0.01)
    if (is.na(siglev)) {
        stop("The significance level must be a valid number.")
    }
    cat("Using significance level:", siglev, "\n")

    # Import vasca dependencies
    source('../R/vasca.R')
    source('../R/pcaEig.R')
    
    # Call the vasca function
    vascao <- vasca(parglmVS, siglev)
    
    # Save results to a JSON file
    write_json(vascao, "vasca_r.json", pretty = TRUE)
    cat("Results saved to vasca_r.json\n")
}

# Execute main function
vasca_run()
