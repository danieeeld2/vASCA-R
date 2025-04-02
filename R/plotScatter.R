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
  
  library(ggplot2)
  library(ggrepel)
  library(scales)

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
  if (!is.null(Multiplicity)) {
    stopifnot(length(Multiplicity) == N)
  }
  if (!is.null(Markers)) {
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

  # Define color palette
  if (is.null(Color)) {
    if (substr(opt, 1, 1) == "1") {
      if (n_elems == 1) {
        color_palette <- grDevices::hsv(h = 0.6, s = 1, v = 1) # Similar to winter(1)
      } else if (n_elems <= 8) {
        color_palette <- okabe_ito_hex(n_elems)
      } else {
        color_palette <- grDevices::hsv(seq(0, 1, length.out = n_elems)) # Similar to hsv
      }
    } else {
      color_palette <- viridis::viridis(n_elems) # Similar to parula
    }
  } else {
    if (Color == 'hsv') {
      color_palette <- grDevices::hsv(seq(0, 1, length.out = n_elems))
    } else if (Color == 'parula') {
      color_palette <- viridis::viridis(n_elems)
    } else if (Color == 'okabeIto') {
      color_palette <- okabe_ito_hex(n_elems)
    } else {
      stop(paste("Color palette '", Color, "' not recognized."))
    }
  }

  color_scale <- scale_color_manual(values = color_palette, name = "ObsClass")

  sizes <- ifelse(df$Multiplicity > bins[4], 2.5 * 4^2 * pi,
                  ifelse(df$Multiplicity > bins[3], 2.5 * 3^2 * pi,
                         ifelse(df$Multiplicity > bins[2], 2.5 * 2^2 * pi,
                                ifelse(df$Multiplicity > bins[1], 2.5 * 1^2 * pi, 0))))


  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::theme_bw()

  switch(substr(opt, 2, 4),
         "000" = { # 2D plot, No multiplicity info, filled marks
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ordClasses), size = 3)
         },
         "010" = { # 2D plot, No multiplicity info, empty marks
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ordClasses), shape = 1, size = 3)
         },
         "100" = { # 2D plot, Multiplicity in size
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ordClasses, size = Multiplicity))
         },
         "101" = { # 2D plot, Multiplicity in markers
           df$mult_group <- cut(df$Multiplicity, breaks = bins, labels = FALSE, include.lowest = TRUE, right = FALSE)
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ordClasses, shape = factor(mult_group)), size = 3) +
             ggplot2::scale_shape_manual(values = markers[1:length(unique(df$mult_group))], name = "Multiplicity")
         },
         "110" = { # 3D plot, Multiplicity in Z-axis (represented by color gradient)
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ordClasses, alpha = Multiplicity / max(Multiplicity)), size = 3) +
             ggplot2::scale_alpha_continuous(name = "Multiplicity")
         },
         "111" = { # 3D plot, Multiplicity in size, classes in Z-axis (not directly representable in 2D ggplot)
           p <- p + ggplot2::geom_point(ggplot2::aes(color = ordClasses, size = Multiplicity), alpha = 0.7)
         }
  )

  p <- p + color_scale +
    ggplot2::labs(x = XYLabel[1], y = XYLabel[2]) +
    ggplot2::theme(legend.position = "right",
                   axis.text = ggplot2::element_text(size = 14),
                   axis.title = ggplot2::element_text(size = 18),
                   legend.text = ggplot2::element_text(size = 12),
                   legend.title = ggplot2::element_text(size = 14))

  # Add labels using textScatterR
  if (!is.null(EleLabel)) {
    p <- text_scatter_ggplot(data = df[, c("x", "y", "EleLabel", "ObsClass", "Multiplicity")],
                               x_col = "x", y_col = "y", ele_label = df$EleLabel,
                               obs_class = df$ObsClass, plot_mult = PlotMult,
                               multiplicity = df$Multiplicity, blur_index = BlurIndex) +
      ggplot2::labs(x = XYLabel[1], y = XYLabel[2]) +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) # Remove default ggplot grid for cleaner look with text_scatter
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

  # Set color axis for numerical classes
  if (substr(opt, 1, 1) == "0") {
    if (length(unique(df$ObsClass)) < 2) {
      p <- p + ggplot2::scale_color_discrete(guide = "none")
    } else {
      p <- p + ggplot2::scale_color_viridis_d(name = "ObsClass") # Using viridis for numerical
    }
  } else {
    if (length(unique(df$ObsClass)) < 2) {
      p <- p + ggplot2::scale_color_discrete(guide = "none")
    }
  }

  return(p)
}