createDesign_run <- function(...) {
    # This script generates a design matrix using the createDesign function.
    #
    # It takes command-line arguments to specify the levels of factors and optional parameters:
    #
    # Usage: Rscript createDesign_run.R <levels> [Replicates]
    #   - <levels>: String representation of a list with factor levels.
    #   - [Replicates]: (Optional) Integer value specifying the number of replicates.
    #
    # Example usage:
    #   Rscript createDesign_run.R "{list(c(1,2,3,4), c(1,2,3))}"  # Default replicates = 1
    #   Rscript createDesign_run.R "{list(c(1,2,3,4), c(1,2,3))}" "4"  # Replicates = 4
    #
    # Outputs:
    #   - Saves the generated design matrix as 'design_matrix_r.csv'
    
    args <- commandArgs(trailingOnly = TRUE)
    
    # Check number of arguments (at least levels must be provided)
    if (length(args) < 1) {
        stop("Not enough arguments. Usage: Rscript createDesign_run.R <levels> [Replicates]")
    }
    
    # Parse levels (convert string to list)
    levels <- eval(parse(text = args[1]))
    if (!is.list(levels)) {
        stop("Levels must be provided as a list string.")
    }
    
    # Default number of replicates
    reps <- 1
    
    # Parse optional replicates argument
    if (length(args) >= 2) {
        reps <- as.integer(args[2])
        if (is.na(reps) || reps < 1) {
            stop("Invalid value for Replicates. Must be a positive integer.")
        }
    }
    
    # Source the createDesign function
    source("../R/createDesign.R")
    
    # Call createDesign function
    F <- createDesign(levels, replicates = reps)
    
    # Save result to CSV file
    write.table(F, "design_matrix_r.csv", row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE) # Ignore row and column names
}

createDesign_run()
