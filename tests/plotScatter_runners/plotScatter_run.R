# This script runs the plotScatter function and saves the plot as an image.
# 
# It takes command-line arguments to specify the input data and optional parameters:
# 
# Usage: plotScatter_run <X> [Name-Value Pairs]
#   - <X>: String representation of the X data OR path to a CSV file.
#   - [Name-Value Pairs]: Optional parameters for plotScatter.
# 
# Optional Parameters:
#   - 'EleLabel': [Nx1] name of the elements (numbers are used by default)
#   - 'ObsClass': [Nx1, str(N), {N}] groups for different visualization (a single group by default)
#   - 'XYLabel': {2} xlabel and ylabel (nothing by default)
#   - 'LimCont': {2} control limits on x and y axis (nothing by default)
#   - 'FilledMarkers': bool, only when 'PlotMult' is 'none'
#       false: empty marks (by default)
#       true: filled marks
#   - 'PlotMult': str
#       'none': do not plot multiplicity (by default)
#       'size': plot multiplicity info in the size of the markers.
#       'shape': plot multiplicity info in the shape of the markers.
#       'zaxis': plot multiplicity information in the Z axis.
#       'zsize': plot multiplicity info in the size of the markers and classes in Z-axis
#   - 'ClassType': str
#       'Numerical': plot for numerical classes (consistent with a colorbar)
#       'Categorical': plot for categorical classes (consistent with a legend)
#   - 'Multiplicity': [Nx1] multiplicity of each row (1s by default)
#   - 'Markers': [1x3] thresholds for the different multiplicity levels
#       maxv(1): threshold between very low and low multiplicity (20 by default)
#       maxv(2): threshold between low and medium multiplicity (50 by default)
#       maxv(3): threshold between medium and high multiplicity (100 by default)
#   - 'BlurIndex': [1x1] to avoid blur when adding labels. It reflects the
#       minimum distance (normalized to [0,1]) where a cluttered label is allowed to be visualized.
#       By default 0.3 is chosen.
#   - 'Color': Choose a color for your data.
#       'hsv' for hsv palette
#       'parula' for parula palette
#       'okabeIto' for color blindness (by default for multiple classes)
# 
# Example usage in R:
#   Rscript plotScatter_run.R "cbind(runif(100), runif(100))"
#   Rscript plotScatter_run.R "cbind(runif(100), runif(100))" "Color" "r" "PlotMult" "size"
# 
# Also, you can parse the data from a CSV file
# 
# Outputs:
#   - Saves the plot as 'plotScatter_r.png'

plotScatter_run <- function(...) {  
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    stop("Not enough arguments. Usage: plotScatter_run <X> [Name-Value Pairs]")
  }

  # Parse X
  argX <- args[1]
  # Check if X is a file path
  if (file.exists(argX) && grepl("\\.csv$", argX)) {
    X <- read.csv(argX, header = FALSE)
  } else {
    X <- eval(parse(text = argX))
  }

  source("../R/okabeIto.R")
  source("../R/textScatter.R")
  source("../R/plotScatter.R")
  
  # Parse optional arguments
  optional_args <- list()
  if (length(args) > 1) {
    i <- 2
    while (i < length(args)) {
      if (i + 1 <= length(args)) {
        name <- args[i]
        value <- args[i + 1]
        # Evaluar expresiones R pasadas como strings
        if (grepl("^c\\(", value) || grepl("^seq\\(", value) || grepl("^\\d+$", value)) {
          value <- eval(parse(text = value))
        } else if (tolower(value) == "true") {
          value <- TRUE
        } else if (tolower(value) == "false") {
          value <- FALSE
        }
        optional_args[[name]] <- value
        i <- i + 2
      } else {
        stop(paste("Missing value for parameter:", args[i]))
      }
    }
  }

  options(ggrepel.max.overlaps = Inf)

  # Run the plotScatterR function with the data and optional arguments
  plot_output <- do.call(plotScatterR, c(list(bdata = X), optional_args)) 

  # Save the plot as a PNG file
  if (dev.cur() > 1) dev.off()  # Close any open graphics devices
  ggsave("plotScatter_r.png", plot = plot_output, width = 7, height = 7, dpi = 300)
}

plotScatter_run()