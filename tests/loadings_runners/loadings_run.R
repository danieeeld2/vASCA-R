loadings_run <- function(...) {
    #' Run the loadings function and save the plots as images
    #'
    #' @description 
    #' This script runs the loadings function and saves the plot as an image.
    #' It takes command-line arguments to specify the input model and optional parameters.
    #'
    #' @usage 
    #' Rscript loadings_run.R <model.json> ["Name" "Value"] ["Name" "Value"] ...
    #'   - <model.json>: Path to a JSON file containing the model structure.
    #'   - ["Name" "Value"]: Optional parameters for loadings function.
    #'
    #' @details
    #' When providing a JSON file:
    #'   - The JSON should contain a structure with fields:
    #'       - lvs: array of latent variables
    #'       - loads: loadings matrix (variables x components)
    #'       - scores: scores matrix (optional)
    #'
    #' Optional Parameters:
    #'   - "PlotType": "Scatter" (default) or "Bars"
    #'   - "Title": Title for the plots. Empty by default
    #'   - "VarsLabel": Names of the variables (indices are used by default)
    #'   - "VarsClass": Groups for different visualization (a single group by default)
    #'   - "BlurIndex": Avoid blur when adding labels (1 by default)
    #'   - "Color": Color palette ("hsv", "parula", "okabeIto" or custom)
    #'
    #' @examples
    #' # Run from command line:
    #' # Rscript loadings_run.R model.json "PlotType" "Bars"

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
    stop("Usage: Rscript loadings_run.R <model.json> [\"Name\" \"Value\"] [\"Name\" \"Value\"] ...")
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
    required_fields <- c("lvs", "loads")
    for (field in required_fields) {
    if (!(field %in% names(model))) {
        stop(sprintf("Missing required field: %s", field))
    }
    }

    if (!("scores" %in% names(model))) {
    model$scores <- matrix(nrow = 0, ncol = 0)
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
            "PlotType" = "plottype",
            "Title" = "tit",
            "VarsLabel" = "label",
            "VarsClass" = "classes",
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

    # Add dependencies
    source("../R/okabeIto.R")
    source("../R/textScatter.R")
    source("../R/plotScatter.R")
    source("../R/plotVec.R")
    source("../R/loadings.R")

    fig_handles <- do.call(loadings, c(list(model = model), params))

    # Save figures
    for (i in 1:length(fig_handles)) {
        if (dev.cur() > 1) dev.off()  # Close any open graphics devices
        ggsave(
            filename = sprintf("loadings_%d_r.png", i),
            plot = fig_handles[[i]],
            width = 8,
            height = 6,
            dpi = 300
        )
        cat(sprintf("Saved plot to loadings_%d_r.png\n", i))
    }

    cat("All plots saved successfully.\n")
}

loadings_run()