#' Vector Plotting Function (R version of MATLAB's plotVec with improved axis labels)
#'
#' Creates bar or line plots for vectors with optional grouping, labels, and control limits.
#' Includes improved handling of crowded axis labels.
#'
#' @param vec Matrix or data frame of vectors to plot (N rows x M columns)
#' @param EleLabel Names of the vector elements (default is row numbers)
#' @param ObsClass Groups for different visualization (default is single group)
#' @param XYLabel List of x and y axis labels (default is no labels)
#' @param LimCont Control limits (default is no limits)
#' @param PlotType "Lines" for line plot or "Bars" for bar plot (default)
#' @param ClassType "Numerical" for colorbar or "Categorical" for legend (auto-detected)
#' @param VecLabel Names of the vectors (default is column numbers)
#' @param Multiplicity Multiplicity of each row (default is 1)
#' @param Markers Thresholds for marker sizes (default c(20, 50, 100))
#' @param Color Color palette: "hsv", "parula", or "okabeIto" (default)
#' @param max_x_labels Maximum number of labels to show on x-axis (default 20)
#'
#' @return ggplot object containing the plot
#'
#' @examples
#' # Basic bar plot
#' plotVec(matrix(rnorm(100), ncol=1))
#'
#' # Line plot with control limits and limited x labels
#' plotVec(matrix(rnorm(300), ncol=3), PlotType="Lines", LimCont=c(1,-1,3), max_x_labels=10)
plotVec <- function(vec, EleLabel=NULL, ObsClass=NULL, XYLabel=NULL, LimCont=NULL,
                     PlotType="Bars", ClassType="default", VecLabel=NULL,
                     Multiplicity=NULL, Markers=c(20,50,100), Color=NULL, max_x_labels=20) {
  
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2", repos = "https://cran.r-project.org", dependencies = TRUE)
  }
  library(ggplot2)
  
  # Function to approximate MATLAB's parula colormap
  parula_colors <- function(n) {
    # Approximate the parula colormap from MATLAB
    colorRampPalette(c("#352A87", "#0F5CDD", "#1481D6", "#06A4CA", 
                       "#2EB7A4", "#87BF77", "#D1BB59", "#FEC832", "#F9FB0E"))(n)
  }
  
  # Okabe-Ito colorblind friendly palette
  okabe_ito_hex <- function(n) {
    colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                "#0072B2", "#D55E00", "#CC79A7", "#000000")
    if (n <= length(colors)) {
      return(colors[1:n])
    } else {
      # If more colors needed, recycle with slight variations
      return(colorRampPalette(colors)(n))
    }
  }
  
  # Convert input to matrix if needed
  if (!is.matrix(vec)) vec <- as.matrix(vec)
  # Handle single row vectors
  if (nrow(vec) == 1) vec <- t(vec)
  
  N <- nrow(vec)
  M <- ncol(vec)
  
  # Set default values
  if (is.null(EleLabel)) EleLabel <- 1:N
  if (is.null(ObsClass)) ObsClass <- rep(1, N)
  if (is.null(XYLabel)) XYLabel <- c("", "")
  if (is.null(VecLabel)) VecLabel <- 1:M
  if (is.null(Multiplicity)) Multiplicity <- rep(1, N)
  
  # Ensure consistent types
  EleLabel <- as.character(EleLabel)
  if (is.numeric(ObsClass)) {
    # Store original numeric classes for potential color mapping
    numeric_classes <- ObsClass
  }
  ObsClass <- as.character(ObsClass)
  VecLabel <- as.character(VecLabel)
  
  # Determine class type if not explicitly specified
  if (ClassType == "default") {
    if (is.numeric(numeric_classes) && length(unique(numeric_classes)) >= 10) {
      ClassType <- "Numerical"
    } else {
      ClassType <- "Categorical"
    }
  }
  
  # Create data frame for ggplot
  plot_data <- data.frame(
    Value = as.vector(vec),
    Element = rep(1:N, M),
    ElementLabel = rep(EleLabel, M),
    Variable = factor(rep(1:M, each = N)),
    VariableLabel = factor(rep(VecLabel, each = N)),
    Class = rep(ObsClass, M),
    Multiplicity = rep(Multiplicity, M)
  )
  
  if (exists("numeric_classes")) {
    plot_data$NumericClass <- rep(numeric_classes, M)
  }
  
  # Base plot
  p <- ggplot(plot_data, aes(x = Element, y = Value))
  
  # Add multiplicity indicators (similar to MATLAB's approach)
  if (!all(Multiplicity == 1)) {
    # Create bins according to markers
    bins <- c(0, 1, Markers, Inf)
    sizes <- 2 * sqrt(1:length(bins))
    
    for (i in 1:(length(bins)-1)) {
      mult_data <- plot_data[plot_data$Multiplicity > bins[i] & 
                             plot_data$Multiplicity <= bins[i+1] & 
                             !duplicated(plot_data$Element), ]
      if (nrow(mult_data) > 0) {
        p <- p + geom_point(data = mult_data, 
                           aes(x = Element, y = 0),
                           shape = 21, fill = "black", size = sizes[i])
      }
    }
  }
  
  # Handle different plot types and class types
  uniqueClasses <- unique(ObsClass)
  nClasses <- length(uniqueClasses)
  
  # Determine color palette
  if (is.null(Color)) {
    if (ClassType == "Categorical" && nClasses <= 8) {
      Color <- "okabeIto"
    } else if (ClassType == "Categorical") {
      Color <- "hsv" 
    } else {
      Color <- "parula"
    }
  }
  
  # Generate appropriate colors
  if (Color == "okabeIto") {
    colors <- okabe_ito_hex(max(nClasses, M))
  } else if (Color == "hsv") {
    colors <- rainbow(max(nClasses, M))
  } else if (Color == "parula") {
    colors <- parula_colors(max(nClasses, M))
  } else {
    # Default to a standard colorbrewer palette if RColorBrewer is available
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(max(nClasses, M))
    } else {
      colors <- rainbow(max(nClasses, M))
    }
  }
  
  if (ClassType == "Categorical") {
    # For categorical class types
    if (PlotType == "Bars") {
      if (nClasses > 1) {
        # Group by class
        p <- p + geom_col(aes(fill = Class), position = position_dodge2(preserve = "single"), width = 0.8)
        p <- p + scale_fill_manual(values = colors[1:nClasses])
      } else {
        # Group by variable
        p <- p + geom_col(aes(fill = VariableLabel), position = position_dodge2(preserve = "single"), width = 0.8)
        p <- p + scale_fill_manual(values = colors[1:M])
      }
    } else { # Lines
      if (nClasses > 1) {
        # Group by class
        p <- p + geom_line(aes(color = Class, group = interaction(Class, Variable)), 
                          linewidth = 0.75 + 1/M) +
                 geom_point(aes(color = Class), size = 3)
        p <- p + scale_color_manual(values = colors[1:nClasses])
      } else {
        # Group by variable
        p <- p + geom_line(aes(color = VariableLabel, group = Variable), 
                          linewidth = 0.75 + 1/M) +
                 geom_point(aes(color = VariableLabel), size = 3)
        p <- p + scale_color_manual(values = colors[1:M])
      }
    }
  } else { # Numerical class type
    if (PlotType == "Bars") {
      p <- p + geom_col(aes(fill = NumericClass), position = position_dodge2(preserve = "single"), width = 0.8)
      p <- p + scale_fill_gradientn(colors = colors)
    } else { # Lines
      p <- p + geom_line(aes(color = NumericClass, group = Variable), 
                        linewidth = 0.75 + 1/M) +
               geom_point(aes(color = NumericClass), size = 3)
      p <- p + scale_color_gradientn(colors = colors)
    }
  }
  
  # Add control limits
  if (!is.null(LimCont)) {
    if (length(LimCont) == 1) {
      # Single control limit
      p <- p + geom_hline(yintercept = LimCont, color = "red", 
                         linetype = "dashed", linewidth = 1)
    } else if (length(LimCont) == N) {
      # Variable control limits
      limit_data <- data.frame(
        x = 1:N,
        y = LimCont
      )
      p <- p + geom_line(data = limit_data, aes(x = x, y = y), 
                        color = "red", linetype = "dashed", linewidth = 1)
    } else if (length(LimCont) > 1 && length(LimCont) < N) {
      # Multiple constant control limits
      for (lim in LimCont) {
        p <- p + geom_hline(yintercept = lim, color = "red", 
                           linetype = "dashed", linewidth = 1)
      }
    }
  }
  
  # Calculate how many labels to skip for readability
  if (N > max_x_labels) {
    step_size <- ceiling(N / max_x_labels)
    selected_indices <- seq(1, N, by = step_size)
    
    # Create custom breaks and labels
    custom_breaks <- selected_indices
    custom_labels <- EleLabel[selected_indices]
    
    p <- p + scale_x_continuous(breaks = custom_breaks, labels = custom_labels)
  } else {
    p <- p + scale_x_continuous(breaks = 1:N, labels = EleLabel)
  }
  
  # Calculate appropriate text angle based on label length
  max_label_length <- max(nchar(EleLabel))
  if (max_label_length > 3 || N > 15) {
    text_angle <- 45
    hjust_val <- 1
    vjust_val <- 1
  } else {
    text_angle <- 0
    hjust_val <- 0.5
    vjust_val <- 0.5
  }
  
  # Add labels and a MATLAB-like theme
  p <- p + 
    labs(x = XYLabel[1], y = XYLabel[2], 
         fill = if(ClassType == "Categorical") "Class" else NULL,
         color = if(ClassType == "Categorical") "Class" else NULL) +
    theme(
      # MATLAB-like light gray background
      panel.background = element_rect(fill = "#F2F2F2", color = "black"),
      # More visible grid lines
      panel.grid.major = element_line(color = "white", linewidth = 0.8),
      panel.grid.minor = element_line(color = "white", linewidth = 0.5),
      # Plot border
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      # Text styling
      axis.text.x = element_text(angle = text_angle, hjust = hjust_val, vjust = vjust_val, size = 11),
      axis.title = element_text(size = 16),
      text = element_text(size = 14, family = "sans"),
      plot.title = element_text(size = 16, face = "bold"),
      # No margin expansion
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Make sure axis is tight (similar to MATLAB's axis tight)
  p <- p + coord_cartesian(expand = FALSE)
  
  return(p)
}