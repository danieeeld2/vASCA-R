scores_run <- function(...) {
    #' Run the scores function and save the plots as images
    #'
    #' @description 
    #' This script runs the scores function and saves the plot as an image.
    #' It takes command-line arguments to specify the input model and optional parameters.
    #'
    #' @usage 
    #' Rscript scores_run.R <model.json> ["Name" "Value"] ["Name" "Value"] ...
    #'   - <model.json>: Path to a JSON file containing the model structure.
    #'   - ["Name" "Value"]: Optional parameters for scores function.
    #'
    #' @details
    #' When providing a JSON file:
    #'   - The JSON should contain a structure with fields:
    #'       - lvs: array of latent variables
    #'       - loads: loadings matrix (variables x components)
    #'       - scores: scores matrix
    #'       - var: total variance
    #'       - av: centering parameters (optional)
    #'       - sc: scaling parameters (optional)
    #'
    #' Optional Parameters:
    #'   - "ObsTest": [LxM] data set with observations to be visualized
    #'   - "PlotType": "Scatter" (default) or "Bars"
    #'   - "PlotCal": TRUE/FALSE to plot calibration data (TRUE by default)
    #'   - "Title": Title for the plots. Empty by default
    #'   - "ObsLabel": Names of the observations (indices are used by default)
    #'   - "ObsClass": Groups for different visualization (a single group by default)
    #'   - "BlurIndex": Avoid blur when adding labels (1 by default)
    #'   - "Color": Color palette ("hsv", "parula", "okabeIto" or custom)
    #'
    #' @examples
    #' # Run from command line:
    #' # Rscript scores_run.R model.json "PlotType" "Bars" "PlotCal" "FALSE"

    # Load required libraries
    if (!require("jsonlite")) {
        cat("Installing jsonlite package...\n")
        install.packages("jsonlite", repos = "https://cloud.r-project.org")
        library(jsonlite)
    }

    if (!require("ggplot2")) {
        cat("Installing ggplot2 package...\n")
        install.packages("ggplot2", repos = "https://cloud.r-project.org")
        library(ggplot2)
    }

    # Get command line arguments
    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) < 1) {
        stop("Usage: Rscript scores_run.R <model.json> [\"Name\" \"Value\"] [\"Name\" \"Value\"] ...")
    }

    # Json file loading
    arg_model <- args[1]
    if (!file.exists(arg_model)) {
        stop(sprintf("File not found: %s", arg_model))
    }

    # Try to load the JSON file
    tryCatch({
        model_json <- readLines(arg_model, warn = FALSE)
        model <- fromJSON(paste(model_json, collapse = ""))
        cat(sprintf("Model loaded from: %s\n", arg_model))
        
        # Print model information for debugging
        cat("Structure of model:\n")
        str(model)
        
    }, error = function(e) {
        stop(sprintf("JSON parsing failed: %s", e$message))
    })

    # Model validation
    required_fields <- c("lvs", "loads", "scores", "var")
    for (field in required_fields) {
        if (!(field %in% names(model))) {
            stop(sprintf("Missing required field: %s", field))
        }
    }

    if (!("av" %in% names(model))) {
        model$av <- numeric(0)
    }
    if (!("sc" %in% names(model))) {
        model$sc <- numeric(0)
    }

    # Parse optional parameters
    params <- list()
    if (length(args) > 1) {
        # Check if parameters are in "Name" "Value" format or name=value format
        if (length(args) >= 3 && !grepl("=", args[2])) {
            # Handle "Name" "Value" pairs
            if ((length(args) - 1) %% 2 != 0) {
                stop("Each parameter must have a corresponding value")
            }
            
            for (i in seq(2, length(args), by = 2)) {
                param_name <- args[i]
                param_value <- args[i + 1]
                
                # Convert parameter names to match R function expectations
                param_mapping <- list(
                    "ObsTest" = "obstest",
                    "PlotType" = "plottype",
                    "PlotCal" = "plotcal",
                    "Title" = "tit",
                    "ObsLabel" = "label",
                    "ObsClass" = "classes",
                    "BlurIndex" = "blur",
                    "Color" = "color"
                )
                
                if (param_name %in% names(param_mapping)) {
                    r_param_name <- param_mapping[[param_name]]
                } else {
                    r_param_name <- tolower(param_name)
                }
                
                # Convert param_value to appropriate type
                if (grepl("^[0-9]+(\\.[0-9]+)?$", param_value)) {
                    param_value <- as.numeric(param_value)
                } else if (param_value == "TRUE" || param_value == "FALSE") {
                    param_value <- as.logical(param_value)
                } else if (grepl("^\\[.*\\]$|^c\\(.*\\)$", param_value)) {
                    # Handle array parameters 
                    param_value <- eval(parse(text = param_value))
                }
                
                params[[r_param_name]] <- param_value
            }
        } else {
            # Handle name=value pairs
            for (i in 2:length(args)) {
                parts <- strsplit(args[i], "=")[[1]]
                if (length(parts) != 2) {
                    stop(sprintf("Parameter format error: %s (should be Name=Value)", args[i]))
                }
                
                param_name <- parts[1]
                param_value <- parts[2]
                
                # Convert param_value to appropriate type
                if (grepl("^[0-9]+(\\.[0-9]+)?$", param_value)) {
                    param_value <- as.numeric(param_value)
                } else if (param_value == "TRUE" || param_value == "FALSE") {
                    param_value <- as.logical(param_value)
                } else if (grepl("^\\[.*\\]$|^c\\(.*\\)$", param_value)) {
                    # Handle array parameters - simplified version
                    param_value <- eval(parse(text = param_value))
                }
                
                params[[param_name]] <- param_value
            }
        }
    }

    # Handle default values for parameters
    if (is.null(params$label)) {
        n_cal <- nrow(model$scores)
        n_test <- ifelse(!is.null(params$obstest), nrow(params$obstest), 0)
        
        if (is.null(params$plotcal) || params$plotcal) {
            params$label <- as.character(c(1:n_cal, if (n_test > 0) 1:n_test else NULL))
        } else {
            params$label <- as.character(1:n_test)
        }
    }

    # Add dependencies
    source("../R/okabeIto.R")
    source("../R/textScatter.R")
    source("../R/plotScatter.R")
    source("../R/plotVec.R")
    source("../R/scores.R")

    # Execute scores function
    fig_handles <- do.call(scores, c(list(model = model), params))

    # Save figures
    for (i in 1:length(fig_handles)) {
        if (dev.cur() > 1) dev.off()  # Close any open graphics devices
        ggsave(
            filename = sprintf("scores_%d_r.png", i),
            plot = fig_handles[[i]],
            width = 8,
            height = 6,
            dpi = 300
        )
        cat(sprintf("Saved plot to scores_%d_r.png\n", i))
    }

    cat("All plots saved successfully.\n")
}

scores_run()