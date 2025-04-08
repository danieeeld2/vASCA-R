if (!require(ggplot2)) {
  install.packages("ggplot2", repos = "https://cran.r-project.org", dependencies = TRUE)
}
if (!require(ggrepel)) {
  install.packages("ggrepel", repos = "https://cran.r-project.org", dependencies = TRUE)
}

library(ggplot2)
library(ggrepel)  # For label repulsion

#' Create a scatter plot with labels using ggplot2
#'
#' @param data Data frame or matrix with plotting coordinates
#' @param x_col Column index or name for x-axis coordinates (default: 1)
#' @param y_col Column index or name for y-axis coordinates (default: 2)
#' @param ele_label Vector of element labels (row names used by default)
#' @param obs_class Vector of class assignments for points (default: 1)
#' @param plot_mult One of "none", "size", "shape", "zaxis", "zsize" (default: "none")
#' @param multiplicity Vector of point weights/frequencies (default: all 1)
#' @param blur_index Controls label density (0-1, default: 0.3)
#'
#' @return A ggplot object with the scatter plot and labels
text_scatter_ggplot <- function(data, x_col = 1, y_col = 2, ele_label = NULL, 
                               obs_class = NULL, plot_mult = "none", 
                               multiplicity = NULL, blur_index = 0.3) {
  
  # Ensure data is a data frame
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  # Set column names for consistency
  if (is.numeric(x_col) && is.numeric(y_col)) {
    colnames(data)[c(x_col, y_col)] <- c("x", "y")
  } else {
    data$x <- data[[x_col]]
    data$y <- data[[y_col]]
  }
  
  N <- nrow(data)
  
  # Handle default values and conversions
  if (is.null(ele_label)) {
    data$label <- rownames(data)
    if (is.null(data$label)) {
      data$label <- as.character(1:N)
    }
  } else {
    data$label <- ele_label
  }
  
  if (is.null(obs_class)) {
    data$class <- rep(1, N)
  } else {
    data$class <- obs_class
  }
  
  if (is.null(multiplicity)) {
    data$mult <- rep(1, N)
  } else {
    data$mult <- multiplicity
  }
  
  # Calculate label repulsion force based on blur_index
  # Higher blur_index means less repulsion (more labels allowed)
  repel_force <- 1 - blur_index
  
  # Create the base plot
  p <- ggplot(data, aes(x = x, y = y)) +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50")
  
  # Add points based on plot_mult parameter
  if (plot_mult == "none") {
    p <- p + geom_point(aes(color = factor(class)))
  } else if (plot_mult == "size") {
    p <- p + geom_point(aes(color = factor(class), size = mult))
  } else if (plot_mult == "shape") {
    p <- p + geom_point(aes(color = factor(class), shape = cut(mult, breaks = c(-Inf, 1, 20, 50, 100, Inf))))
  } else if (plot_mult %in% c("zaxis", "zsize")) {
    # For 3D representation, we use color gradient for z-axis in 2D
    if (plot_mult == "zaxis") {
      # Use multiplicity as z-axis
      p <- p + geom_point(aes(color = factor(class), alpha = mult/max(mult)))
    } else {
      # Use class as z-axis and size as multiplicity
      p <- p + geom_point(aes(color = factor(class), size = mult))
    }
  }
  
  # Add labels with ggrepel to avoid overlapping
  # The force and max.overlaps parameters control label density
  max_overlaps <- round((N * blur_index)^2)  # More overlaps allowed with higher blur_index
  max_overlaps <- max(1, min(max_overlaps, 100))  # Keep within reasonable bounds
  
  p <- p + ggrepel::geom_text_repel(
    aes(label = label),
    size = 3,
    force = repel_force * 10,
    max.overlaps = max_overlaps,
    box.padding = 0.5,
    point.padding = 0.1,
    segment.color = "gray50",
    direction = "both"
  )
  
  # Customize appearance
  p <- p + labs(color = "Class", size = "Multiplicity", shape = "Multiplicity", alpha = "Multiplicity") +
    theme(legend.position = "right")
  
  return(p)
}

# Demo of text_scatter_ggplot function
X <- data.frame(x = rnorm(100), y = rnorm(100))
EleLabel <- paste("Point", 1:100)
ObsClass <- sample(1:3, 100, replace = TRUE)
Multiplicity <- sample(1:5, 100, replace = TRUE)
BlurIndex <- 0.3
plot_mult <- "size"
p <- text_scatter_ggplot(X, ele_label = EleLabel, obs_class = ObsClass, 
                           plot_mult = plot_mult, multiplicity = Multiplicity, 
                           blur_index = BlurIndex)



