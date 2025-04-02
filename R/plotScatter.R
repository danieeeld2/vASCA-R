#' Scatter plot function in R (ggplot2 implementation)
#'
#' @param bdata Matrix or data frame with bidimensional data to plot (Nx2)
#' @param ... Optional parameters
#'
#' @return A ggplot object
#'
#' @examples
#' # Basic usage
#' plotScatter(matrix(rnorm(200), ncol=2))
#' 
#' # With labels and classes
#' plotScatter(matrix(rnorm(10), ncol=2), 
#'             EleLabel=c("one","two","three","four","five"),
#'             ObsClass=c(1,1,1,2,2), 
#'             XYLabel=c("Y","X"))
#'
plotScatter <- function(bdata, ...) {
  # Check if required packages are available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2", repos = "https://cran.r-project.org", dependencies = TRUE)
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    install.packages("ggrepel", repos = "https://cran.r-project.org", dependencies = TRUE)
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    install.packages("scales", repos = "https://cran.r-project.org", dependencies = TRUE)
  }
  
  library(ggplot2)
  library(ggrepel)
  library(scales)
  
  # Parameter checking
  if (missing(bdata)) {
    stop("Error in the number of arguments. Parameter 'bdata' is required.")
  }
  
  # Convert bdata to matrix if it's not already
  if (!is.matrix(bdata) && !is.data.frame(bdata)) {
    bdata <- as.matrix(bdata)
  }
  
  # Check dimensions
  if (ncol(bdata) != 2) {
    stop("Dimension Error: parameter 'bdata' must be N-by-2.")
  }
  
  # Get number of rows
  N <- nrow(bdata)
  
  # Parse optional parameters
  params <- list(...)
  
  # Set default values for parameters
  EleLabel <- if ("EleLabel" %in% names(params)) params$EleLabel else 1:N
  ObsClass <- if ("ObsClass" %in% names(params)) params$ObsClass else rep(1, N)
  XYLabel <- if ("XYLabel" %in% names(params)) params$XYLabel else c("", "")
  LimCont <- if ("LimCont" %in% names(params)) params$LimCont else NULL
  Multiplicity <- if ("Multiplicity" %in% names(params)) params$Multiplicity else rep(1, N)
  Markers <- if ("Markers" %in% names(params)) params$Markers else c(20, 50, 100)
  BlurIndex <- if ("BlurIndex" %in% names(params)) params$BlurIndex else 0.3
  Color <- if ("Color" %in% names(params)) params$Color else NULL
  FilledMarkers <- if ("FilledMarkers" %in% names(params)) params$FilledMarkers else FALSE
  PlotMult <- if ("PlotMult" %in% names(params)) params$PlotMult else "none"
  ClassType <- if ("ClassType" %in% names(params)) params$ClassType else "default"
  
  # Check dimensions and convert types as needed
  if (length(EleLabel) == 1 && EleLabel[1] == 1 && length(EleLabel) != N) {
    EleLabel <- 1:N
  }
  if (length(ObsClass) == 1 && ObsClass[1] == 1 && length(ObsClass) != N) {
    ObsClass <- rep(1, N)
  }
  if (length(Multiplicity) == 1 && Multiplicity[1] == 1 && length(Multiplicity) != N) {
    Multiplicity <- rep(1, N)
  }
  
  # Convert row vectors to column vectors
  if (is.matrix(EleLabel) && nrow(EleLabel) == 1) {
    EleLabel <- t(EleLabel)
  }
  if (is.matrix(ObsClass) && nrow(ObsClass) == 1) {
    ObsClass <- t(ObsClass)
  }
  if (is.matrix(Multiplicity) && nrow(Multiplicity) == 1) {
    Multiplicity <- t(Multiplicity)
  }
  
  # Convert single values to vectors if needed
  if (length(EleLabel) == 1 && N > 1) {
    EleLabel <- rep(EleLabel, N)
  }
  if (length(ObsClass) == 1 && N > 1) {
    ObsClass <- rep(ObsClass, N)
  }
  if (length(Multiplicity) == 1 && N > 1) {
    Multiplicity <- rep(Multiplicity, N)
  }
  
  # Determine class type
  if (ClassType == "default") {
    if (!is.numeric(ObsClass) || length(unique(ObsClass)) < 10) {
      ClassType <- "Categorical"
    } else {
      ClassType <- "Numerical"
    }
  }
  
  # Convert to characters if needed
  if (!is.null(EleLabel) && is.numeric(EleLabel)) {
    EleLabel <- as.character(EleLabel)
  }
  if (!is.null(ObsClass) && is.numeric(ObsClass) && ClassType == "Categorical") {
    ObsClass <- as.character(ObsClass)
  }
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    x = bdata[, 1],
    y = bdata[, 2],
    class = ObsClass,
    label = EleLabel,
    mult = Multiplicity
  )
  
  # Sort data for colorbar if numerical classes
  if (ClassType == "Numerical" && is.numeric(ObsClass)) {
    plot_data <- plot_data[order(plot_data$class), ]
  }
  
  # Set up the plot
  p <- ggplot(plot_data, aes(x = x, y = y))
  
  # Define bin thresholds for multiplicity
  bins <- c(0, 1, Markers, Inf)
  
  # Define base styling
  p <- p + theme_bw() +
    theme(
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Add origin lines
  p <- p + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black")
  
  # Get unique classes
  unique_classes <- unique(ObsClass)
  n_classes <- length(unique_classes)
  
  # Set color palette
  if (is.null(Color)) {
    if (ClassType == "Categorical") {
      if (n_classes == 1) {
        color_values <- scales::hue_pal()(1)
      } else if (n_classes <= 8) {
        color_values <- okabe_ito_hex(n_classes)
      } else {
        color_values <- scales::hue_pal()(n_classes)
      }
    } else {
      color_values <- scales::viridis_pal(option = "plasma")(n_classes)
    }
  } else {
    if (Color == "hsv") {
      color_values <- scales::hue_pal()(n_classes)
    } else if (Color == "parula") {
      color_values <- scales::viridis_pal(option = "plasma")(n_classes)
    } else if (Color == "okabeIto") {
      color_values <- okabe_ito_hex(n_classes)
    } else {
      # Default to hsv if unrecognized
      color_values <- scales::hue_pal()(n_classes)
    }
  }
  
  # Add points based on PlotMult parameter
  if (PlotMult == "none") {
    if (ClassType == "Categorical") {
      p <- p + geom_point(aes(color = factor(class)), 
                         fill = if(FilledMarkers) NA else "white",
                         shape = if(FilledMarkers) 19 else 1)
      p <- p + scale_color_manual(values = color_values, name = "Class")
    } else {
      p <- p + geom_point(aes(color = class),
                         fill = if(FilledMarkers) NA else "white",
                         shape = if(FilledMarkers) 19 else 1)
      p <- p + scale_color_gradientn(colors = color_values, name = "Class")
    }
  } else if (PlotMult == "size") {
    if (ClassType == "Categorical") {
      p <- p + geom_point(aes(color = factor(class), size = mult), fill = NA)
      p <- p + scale_color_manual(values = color_values, name = "Class")
    } else {
      p <- p + geom_point(aes(color = class, size = mult), fill = NA)
      p <- p + scale_color_gradientn(colors = color_values, name = "Class")
    }
    p <- p + scale_size_continuous(name = "Multiplicity", range = c(2, 15))
  } else if (PlotMult == "shape") {
    # Create multiplicity categories
    plot_data$mult_cat <- cut(plot_data$mult, 
                              breaks = c(-Inf, 1, Markers, Inf),
                              labels = c("≤ 1", "≤ 20", "≤ 50", "≤ 100", "> 100"))
    
    if (ClassType == "Categorical") {
      p <- p + geom_point(data = plot_data, 
                        aes(color = factor(class), shape = mult_cat), 
                        size = 3, fill = NA)
      p <- p + scale_color_manual(values = color_values, name = "Class")
    } else {
      p <- p + geom_point(data = plot_data, 
                        aes(color = class, shape = mult_cat), 
                        size = 3, fill = NA)
      p <- p + scale_color_gradientn(colors = color_values, name = "Class")
    }
    p <- p + scale_shape_manual(values = c(3, 4, 5, 1, 2), name = "Multiplicity")
  } else if (PlotMult == "zaxis") {
    # In 2D we'll use transparency for z-axis representation
    if (ClassType == "Categorical") {
      p <- p + geom_point(aes(color = factor(class), alpha = mult), size = 3)
      p <- p + scale_color_manual(values = color_values, name = "Class")
    } else {
      p <- p + geom_point(aes(color = class, alpha = mult), size = 3)
      p <- p + scale_color_gradientn(colors = color_values, name = "Class")
    }
    p <- p + scale_alpha_continuous(name = "Multiplicity (Z)")
  } else if (PlotMult == "zsize") {
    # Use both size and transparency
    if (ClassType == "Categorical") {
      p <- p + geom_point(aes(color = factor(class), size = mult, alpha = as.numeric(factor(class))), fill = NA)
      p <- p + scale_color_manual(values = color_values, name = "Class (Z)")
    } else {
      p <- p + geom_point(aes(color = class, size = mult, alpha = class), fill = NA)
      p <- p + scale_color_gradientn(colors = color_values, name = "Class (Z)")
    }
    p <- p + scale_size_continuous(name = "Multiplicity", range = c(2, 15))
    p <- p + scale_alpha_continuous(name = "Z-axis", range = c(0.3, 1))
  }
  
  # Add labels using the text_scatter_ggplot function
  # Use only the repelling label functionality from this function
  p <- p + geom_text_repel(
    aes(label = label),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.1,
    force = 10 * (1 - BlurIndex),
    max.overlaps = round(N * BlurIndex),
    segment.color = "gray50"
  )
  
  # Add control limits if specified
  if (!is.null(LimCont)) {
    # X limits
    if (!is.null(LimCont[[1]]) && length(LimCont[[1]]) > 0) {
      for (lim in LimCont[[1]]) {
        p <- p + geom_vline(xintercept = lim, linetype = "dashed", color = "red", size = 1)
      }
    }
    
    # Y limits
    if (!is.null(LimCont[[2]]) && length(LimCont[[2]]) > 0) {
      for (lim in LimCont[[2]]) {
        p <- p + geom_hline(yintercept = lim, linetype = "dashed", color = "red", size = 1)
      }
    }
  }
  
  # Add axis labels
  if (!is.null(XYLabel) && length(XYLabel) >= 2) {
    p <- p + labs(x = XYLabel[1], y = XYLabel[2])
  }
  
  # Legend placement
  if (n_classes < 2) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = "right")
  }
  
  # Return the plot
  return(p)
}

# Demonstration
p <- plotScatter(matrix(rnorm(10), ncol=2),
           EleLabel=c("uno","dos","tres","cuatro","cinco"),
           ObsClass=c(1,1,1,2,2),
           XYLabel=c("Y","X"),
           Multiplicity=c(1,20,50,100,5),
           PlotMult="size")

# Print the plot
print(p)