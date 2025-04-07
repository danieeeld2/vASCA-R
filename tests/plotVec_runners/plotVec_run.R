#!/usr/bin/env Rscript

#' Vector Plotting Runner Function
#' 
#' This script runs the plotVec function and saves the plot as an image.
#' 
#' It takes command-line arguments to specify the input data and optional parameters:
#' 
#' Usage: plotVec_run.R <vec> [Name-Value Pairs]
#'   - <vec>: String representation of the vector data OR path to a CSV file.
#'   - [Name-Value Pairs]: Optional parameters for plotVecR.
#' 
#' Optional Parameters:
#'   - 'EleLabel': [Nx1] name of the elements (numbers are used by default)
#'   - 'ObsClass': [Nx1, str(N), {N}] groups for different visualization
#'   - 'XYLabel': {2} xlabel and ylabel (nothing by default)
#'   - 'LimCont': [NxL or Lx1] control limits (nothing by default)
#'   - 'PlotType': 'Lines' or 'Bars' (default)
#'   - 'ClassType': 'Numerical' or 'Categorical' (auto-detected by default)
#'   - 'VecLabel': [Mx1] name of the vectors (numbers are used by default)
#'   - 'Multiplicity': [Nx1] multiplicity of each row (1s by default)
#'   - 'Markers': [1x3] thresholds for marker sizes (default c(20,50,100))
#'   - 'Color': Color palette ('hsv', 'parula', or 'okabeIto' (default))
#' 
#' Example usage in R:
#'   Rscript plotVec_run.R "matrix(rnorm(100), ncol=2)"
#'   Rscript plotVec_run.R "matrix(rnorm(100), ncol=2)" "PlotType" "Lines" "Color" "hsv"
#'   Rscript plotVec_run.R "data.csv" "XYLabel" "c('Index','Value')" "ObsClass" "c(1,1,1,2,2)"
#' 
#' Outputs:
#'   - Saves the plot as 'plotVec_r.png'

plotVec_run <- function(...) {
  # Capture command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) < 1) {
    stop("Not enough arguments. Usage: plotVec_run.R <vec> [Name-Value Pairs]")
  }

  # Parse vec argument (either CSV file or R expression)
  argVec <- args[1]
  if (file.exists(argVec) && grepl("\\.csv$", argVec)) {
    vec <- as.matrix(read.csv(argVec, header = FALSE))
  } else {
    vec <- eval(parse(text = argVec))
  }

  # Load required functions
  source("../R/okabeIto.R")
  source("../R/plotVec.R")       
  
  # Parse optional arguments
  optional_args <- list()
  if (length(args) > 1) {
    i <- 2
    while (i < length(args)) {
      if (i + 1 <= length(args)) {
        name <- args[i]
        value <- args[i + 1]
        
        # Try to evaluate R expressions
        parsed_value <- tryCatch({
          eval(parse(text = value))
        }, error = function(e) {
          # If evaluation fails, use as character
          if (tolower(value) == "true") return(TRUE)
          if (tolower(value) == "false") return(FALSE)
          return(value)
        })
        
        optional_args[[name]] <- parsed_value
        i <- i + 2
      } else {
        stop(paste("Missing value for parameter:", args[i]))
      }
    }
  }

  options(ggrepel.max.overlaps = Inf)

  # Generate plot
  plot_output <- do.call(plotVec, c(list(vec = vec), optional_args))
  
  # Save plot
  if (dev.cur() > 1) dev.off()  # Close any open graphics devices
  ggsave("plotVec_r.png", plot = plot_output, width = 7, height = 7, dpi = 300)
}

# Execute the function
plotVec_run()