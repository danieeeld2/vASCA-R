#' Create a balanced design matrix for experimental designs
#'
#' @param levels A list of factor levels, each specified as a vector
#' @param replicates Integer specifying the number of replicates per combination (default is 1)
#'
#' @return A matrix representing the experimental design
#'
#' @examples
#' # Two factors with 4 and 3 levels respectively, and 4 replicates:
#' levels <- list(c(1, 2, 3, 4), c(1, 2, 3))
#' createDesign(levels, replicates = 4)
#'
#' @author Daniel Alconchel Vázquez
#' @export
createDesign <- function(levels, replicates = 1) {
    # Check that levels is a list
    if (!is.list(levels)) {
    stop("levels must be a list of factor levels")
    }

    # Create the design using expand.grid.
    # Note: The Matlab code builds the design such that the last factor (levels[[m]])
    # varies fastest. To do this with expand.grid (which varies the first argument fastest),
    # we reverse the list and then reorder the columns.
    design <- expand.grid(rev(levels))
    # Reorder columns to match the original order (first element in 'levels' becomes first column)
    design <- design[, rev(seq_along(design)), drop = FALSE]

    # If replicates > 1, replicate each row accordingly.
    if (replicates > 1) {
    design <- design[rep(seq_len(nrow(design)), each = replicates), , drop = FALSE]
    rownames(design) <- NULL
    }

    # Return as a matrix (numeric if possible)
    return(as.matrix(design))
}