#' Scatter plot function in R (ggplot2 implementation)
#'
#' @param bdata Matrix or data frame with bidimensional data to plot (Nx2)
#' @param EleLabel Optional vector of element labels. If NULL, defaults to 1:N.
#' @param ObsClass Optional vector of class labels for each observation. If NULL, defaults to 1 for all observations.
#' @param XYLabel Optional vector of length 2 specifying the x and y axis labels. Default is c("", "").
#' @param LimCont Optional list of two vectors for control limits in the x and y axes, respectively. Default is NULL.
#' @param Multiplicity Optional vector specifying the multiplicity (size of points). Default is NULL, which implies size=1 for all.
#' @param Markers Optional vector of 3 numeric values specifying the marker sizes for different multiplicity ranges. Default is c(20, 50, 100).
#' @param BlurIndex Optional numeric value specifying the blur effect for the points. Default is 0.3.
#' @param Color Optional string specifying the color scale. Possible values are 'hsv', 'parula', 'viridis'. Default is NULL (uses 'viridis').
#' @param FilledMarkers Optional logical indicating whether to use filled markers (TRUE) or not (FALSE). Default is FALSE.
#' @param PlotMult Optional string specifying how to visualize multiplicity. Possible values are "none", "size", "shape", "zaxis", "zsize". Default is "none".
#' @param ClassType Optional string specifying the type of class data. Possible values are "default", "Numerical", or "Categorical". Default is "default".
#'
#' @return A ggplot object representing the scatter plot.
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
#' @import ggplot2
#' @import ggrepel
#' @import scales
#' @import viridis
#' 
#' @author Daniel Alconchel Vázquez
#' @export
plotScatter <- function(bdata, EleLabel = NULL, ObsClass = NULL, XYLabel = c("", ""),
                        LimCont = NULL, Multiplicity = NULL, Markers = c(20, 50, 100),
                        BlurIndex = 0.3, Color = NULL, FilledMarkers = FALSE,
                        PlotMult = "none", ClassType = "default") {

  # Check if required packages are installed and load them
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2", repos = "https://cran.r-project.org", dependencies = TRUE)
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    install.packages("ggrepel", repos = "https://cran.r-project.org", dependencies = TRUE)
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    install.packages("scales", repos = "https://cran.r-project.org", dependencies = TRUE)
  }
  if (!requireNamespace("viridis", quietly = TRUE)) {
    install.packages("viridis", repos = "https://cran.r-project.org", dependencies = TRUE)
  }
  
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(viridis)

  N <- nrow(bdata)

  # Handle default values
  if (is.null(EleLabel)) {
    EleLabel <- 1:N
  }
  if (is.null(ObsClass)) {
    ObsClass <- rep(1, N)
  }
  if (is.null(Multiplicity)) {
    Multiplicity <- rep(1, N)
  }

  # Convert to column vectors if necessary
  if (is.vector(EleLabel)) {
    EleLabel <- as.vector(EleLabel)
  }
  if (is.vector(ObsClass)) {
    ObsClass <- as.vector(ObsClass)
  }
  if (is.vector(Multiplicity)) {
    Multiplicity <- as.vector(Multiplicity)
  }
  if (is.vector(Markers)) {
    Markers <- as.vector(Markers)
  }
  if (is.vector(BlurIndex)) {
    BlurIndex <- as.numeric(BlurIndex)
  }

  # Check type of plot
  if (PlotMult == "none") {
    opt <- ifelse(FilledMarkers, "010", "000")
  } else if (PlotMult == "size") {
    opt <- "100"
  } else if (PlotMult == "shape") {
    opt <- "101"
  } else if (PlotMult == "zaxis") {
    opt <- "110"
  } else if (PlotMult == "zsize") {
    opt <- "111"
  } else {
    stop("ValueError: invalid value for parameter 'PlotMult'.")
  }

  # Check type of class
  if (ClassType == "Numerical") {
    opt <- paste0("0", opt)
  } else if (ClassType == "Categorical") {
    opt <- paste0("1", opt)
  } else if (ClassType == "default") {
    if (!is.numeric(ObsClass) || length(unique(ObsClass)) < 10) {
      opt <- paste0("1", opt)
    } else {
      opt <- paste0("0", opt)
    }
  } else {
    stop("ValueError: parameter 'ClassType' must contain either 'Numerical' or 'Categorical'.")
  }

  # Convert num arrays to str
  if (!is.null(EleLabel) && is.numeric(EleLabel)) {
    EleLabel <- as.character(EleLabel)
  }
  if (!is.null(ObsClass) && is.numeric(ObsClass) && substr(opt, 1, 1) == "1") {
    ObsClass <- as.character(ObsClass)
  }

  # Validate dimensions of input data
  stopifnot(ncol(bdata) == 2)
  if (!is.null(EleLabel)) {
    stopifnot(length(EleLabel) == N || length(EleLabel) == N + 1)
  }
  if (!is.null(ObsClass)) {
    stopifnot(length(ObsClass) == N || length(ObsClass) == N + 1)
  }
  stopifnot(length(XYLabel) == 2)
  if (!is.null(LimCont)) {
    stopifnot(is.list(LimCont) && length(LimCont) == 2)
  }
  if (is.null(Multiplicity)) {
    stopifnot(length(Multiplicity) == N)
  }
  if (is.null(Markers)) {
    stopifnot(length(Markers) == 3)
  }
  stopifnot(length(BlurIndex) == 1)

  # Create data frame for ggplot
  df <- as.data.frame(bdata)
  colnames(df) <- c("x", "y")
  df$EleLabel <- EleLabel
  df$ObsClass <- ObsClass
  df$Multiplicity <- Multiplicity

  # Sort data for colorbar
  if (substr(opt, 1, 1) == "0") {
    if (is.factor(df$ObsClass)) {
      df <- df[order(as.numeric(df$ObsClass)), ]
    } else if (is.character(df$ObsClass)) {
      df <- df[order(as.numeric(df$ObsClass)), ]
    } else {
      df <- df[order(df$ObsClass), ]
    }
    cax <- range(as.numeric(df$ObsClass))
    df$ObsClass <- as.numeric(df$ObsClass)  # Ensure numeric for continuous scale
  }

  # Get ordering of classes
  unique_classes <- unique(df$ObsClass)
  if (is.factor(df$ObsClass)) {
    df$ordClasses <- as.numeric(df$ObsClass)
  } else {
    df$ordClasses <- factor(df$ObsClass, levels = unique_classes)
  }
  unique_ord_classes <- unique(df$ordClasses)

  # Define mult bins, markers, colors and sizes
  bins <- c(0, 1, Markers, Inf)
  markers <- c(24, 25, 23, 21, 22) # Corresponding to ^, v, d, o, s in R (filled)

  n_elems <- length(unique_ord_classes)

  # Base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::theme_bw()

  # Handle plotting based on options
  switch(substr(opt, 2, 4),
         "000" = { # 2D plot, No multiplicity info, filled marks
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ObsClass), size = 3)
         },
         "010" = { # 2D plot, No multiplicity info, empty marks
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ObsClass), shape = 1, size = 3)
         },
         "100" = { # 2D plot, Multiplicity in size
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ObsClass, size = Multiplicity))
         },
         "101" = { # 2D plot, Multiplicity in markers
           df$mult_group <- cut(df$Multiplicity, breaks = bins, labels = FALSE, include.lowest = TRUE, right = FALSE)
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ObsClass, shape = factor(df$mult_group)), size = 3) +
             ggplot2::scale_shape_manual(values = markers[1:length(unique(df$mult_group))], name = "Multiplicity")
         },
         "110" = { # 3D plot, Multiplicity in Z-axis (represented by color gradient)
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ObsClass, alpha = Multiplicity / max(Multiplicity, na.rm = TRUE)), size = 3) +
             ggplot2::scale_alpha_continuous(name = "Multiplicity")
         },
         "111" = { # 3D plot, Multiplicity in size, classes in Z-axis (not directly representable in 2D ggplot)
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ObsClass, size = Multiplicity), alpha = 0.7)
         }
  )

  # Handle color scales
  if (substr(opt, 1, 1) == "0") {
    # Numerical data - use continuous color scale
    if (length(unique(df$ObsClass)) < 2) {
      p <- p + ggplot2::scale_color_continuous(guide = "none")
    } else {
      if (!is.null(Color)) {
        if (Color == 'hsv') {
          p <- p + ggplot2::scale_color_gradientn(colors = grDevices::hsv(seq(0, 1, length.out = 256)), 
                               name = "ObsClass")
        } else if (Color == 'parula') {
          p <- p + viridis::scale_color_viridis(option = "plasma", name = "ObsClass")
        } else {
          p <- p + viridis::scale_color_viridis(name = "ObsClass")
        }
      } else {
        p <- p + viridis::scale_color_viridis(name = "ObsClass")
      }
    }
  } else {
    # Categorical data - use discrete color scale
    if (is.null(Color)) {
      if (n_elems == 1) {
        color_palette <- grDevices::hsv(h = 0.6, s = 1, v = 1)
      } else if (n_elems <= 8) {
        color_palette <- okabe_ito_hex(n_elems)
      } else {
        color_palette <- grDevices::hsv(h = seq(0, 1, length.out = n_elems + 1)[1:n_elems], s = 1, v = 1)
      }
    } else {
      if (Color == 'hsv') {
        color_palette <- grDevices::hsv(h = seq(0, 1, length.out = n_elems + 1)[1:n_elems], s = 1, v = 1)
      } else if (Color == 'parula') {
        color_palette <- viridis::viridis(n_elems)
      } else if (Color == 'okabeIto') {
        color_palette <- okabe_ito_hex(n_elems)
      } else {
        stop(paste("Color palette '", Color, "' not recognized."))
      }
    }
    
    if (length(unique(df$ObsClass)) < 2) {
      p <- p + ggplot2::scale_color_manual(values = color_palette, guide = "none")
    } else {
      p <- p + ggplot2::scale_color_manual(values = color_palette, name = "ObsClass")
    }
  }

  # Add labels
  if (!is.null(EleLabel)) {
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = EleLabel),
      size = 3,
      box.padding = 0.5,
      max.overlaps = Inf,
      min.segment.length = 0,
      segment.alpha = 0.5
    )
  }

  # Plot control limits
  if (!is.null(LimCont)) {
    if (!is.null(LimCont[[1]])) {
      for (limit in LimCont[[1]]) {
        p <- p + ggplot2::geom_vline(xintercept = limit, linetype = "dashed", color = "red")
      }
    }
    if (!is.null(LimCont[[2]])) {
      for (limit in LimCont[[2]]) {
        p <- p + ggplot2::geom_hline(yintercept = limit, linetype = "dashed", color = "red")
      }
    }
  } else {
    p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black")
  }

  # Final theme settings
  p <- p + 
    ggplot2::labs(x = XYLabel[1], y = XYLabel[2]) +
    ggplot2::theme(
      legend.position = "right",
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 14)
    )

  return(p)
}