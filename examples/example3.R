# Install and load necessary libraries
if (!require(dplyr)) {
  install.packages("dplyr", repos = "https://cran.r-project.org", dependencies = TRUE)
}

library(dplyr)

# Set seed for reproducibility
set.seed(123)

# --- Create X_Data matrix from GSE3212_series_matrix.txt ---

# Read gene expression data
exp_data <- read.table("./datasets/GSE3212_series_matrix.txt", header=TRUE, sep="\t", comment.char="!", quote="\"")

# Extract X_Data matrix
sample_cols <- grep("GSM", names(exp_data))
X_Data <- exp_data[, sample_cols]

# Transpose X_Data so genes are columns and samples are rows
X_Data <- t(X_Data)

# Assign row names to X_Data (sample names)
rownames(X_Data) <- colnames(exp_data)[sample_cols]

# Assign column names to X_Data (gene IDs)
colnames(X_Data) <- exp_data$ID_REF

# Convert X_Data to data.frame
X_Data <- as.data.frame(X_Data)

# Check X_Data dimensions
cat("Dimensions of X_Data matrix:", dim(X_Data), "\n")
cat("First 3 rows and columns of X_Data:\n")
print(X_Data[1:3, 1:3])

# --- Extract sample information from file ---

# Read lines from the file
lines <- readLines("./datasets/GSE3212_series_matrix.txt")

# Find lines with sample information
description_line <- grep("^!Sample_description", lines, value = TRUE)[1]
age_line <- grep("^!Sample_characteristics_ch1.*Age:", lines, value = TRUE)[1]
ethnicity_line <- grep("^!Sample_characteristics_ch1.*Ethnicity:", lines, value = TRUE)[1]

# Split descriptions by tab
sample_descriptions <- strsplit(description_line, "\t")[[1]]
age_info <- strsplit(age_line, "\t")[[1]]
ethnicity_info <- strsplit(ethnicity_line, "\t")[[1]]

# Remove the first element (headers)
sample_descriptions <- sample_descriptions[-1]
age_info <- age_info[-1]
ethnicity_info <- ethnicity_info[-1]

# Extract age values 
ages <- age_info %>%
  gsub("Age: ", "", .) %>%
  gsub('"', '', .) %>%
  as.numeric()

# Extract ethnicity values (remove "Ethnicity: " prefix)
ethnicities <- gsub("Ethnicity: ", "", ethnicity_info)

# Create a data frame with sample information
sample_info <- data.frame(
  Sample = rownames(X_Data),
  Description = sample_descriptions,
  Age = ages,
  Ethnicity = ethnicities
)

# Create 'Smoker' column as character strings
sample_info <- sample_info %>%
  mutate(Smoker = ifelse(grepl("non-smoker", Description, ignore.case = TRUE), "Non-Smoker", 
                         ifelse(grepl("smoker", Description, ignore.case = TRUE), "Smoker", NA)))

# Standardize ethnicity values (make them consistent)
sample_info$Ethnicity <- tolower(sample_info$Ethnicity)
sample_info$Ethnicity <- tools::toTitleCase(sample_info$Ethnicity)

# Create 'Age_Group' column based on age
sample_info <- sample_info %>%
  mutate(Age_Group = ifelse(Age > 40, "Over 40", "Under or Equal to 40"))

# Display sample information
cat("\nSample information:\n")
print(sample_info)

# Add scripts from the R folder to the environment
files <- list.files(path = "./R", pattern = "\\.R$", full.names = TRUE)

invisible(
  lapply(files, function(f) {
    suppressMessages(
      suppressWarnings(
        source(f, echo = FALSE, print.eval = FALSE)
      )
    )
  })
)

# Generate output folder with a different name
script_name <- tools::file_path_sans_ext(basename(sys.frames()[[1]]$ofile %||% "COPD_tabaco_analysis"))
output_dir <- file.path("examples", paste0(script_name, "_results"))
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Verify X_Data structure before calling parglmVS
cat("Verifying structure of X_Data before parglmVS:\n")
cat("Dimensions:", dim(X_Data), "\n")
cat("First columns:", head(colnames(X_Data)), "\n")

# Ensure X_Data is a numeric matrix
X_Data <- as.matrix(X_Data)

# Verify structure after conversion (optional)
cat("Structure of X_Data after as.matrix:\n")
str(X_Data)

F <- sample_info[, (ncol(sample_info) - 2):ncol(sample_info)]

# Verify structure of F
cat("Structure of F (design matrix):\n")
cat("Dimensions:", dim(F), "\n")
cat("First few rows of F:\n")
print(head(F))

# Perform analysis with the reduced design matrix
res_vs <- parglmVS(X_Data, F, model = "interaction", permutations = 1000)

# Extract and display results (TVS)
TVS <- res_vs[[1]]
parglmoVS <- res_vs[[2]]
cat("\nTVS Results:\n")
print(TVS)

# Save TVS as CSV
write.csv(TVS, file = file.path(output_dir, "TVS.csv"), row.names = FALSE)

# VASCA
vascao <- vasca(parglmoVS, siglev = 0.05)

# Save vascao as JSON
jsonlite::write_json(vascao, path = file.path(output_dir, "vascao.json"), pretty = TRUE, auto_unbox = TRUE)

# Print the total number of factors in vascao
cat(sprintf("Total number of factors in VASCA: %d\n", vascao$nFactors))

# Iterate over all factors
for (i in seq_len(vascao$nFactors)) {
    factor <- vascao$factors[[i]]
    cat(sprintf("Factor %d: stasig = %s\n", i, factor$stasig))

    if (factor$stasig) {
        cat(sprintf("Factor %d (Significant) \n", i))

        tempModel <- list()
        tempModel$loads <- as.matrix(factor$loadsSorted)
        tempModel$scores <- as.matrix(factor$scoresV)
        tempModel$lvs <- seq_len(ncol(tempModel$loads))
        tempModel$var <- sum(apply(factor$scoresV, 2, var))

        # Loadings plot
        figLoadings <- loadings(tempModel,
                                tit = sprintf("Factor %d Loadings ", i),
                                plottype = "Bars")

        if (is.list(figLoadings)) {
            for (j in seq_along(figLoadings)) {
                png(filename = file.path(output_dir, sprintf("loadings_factor_%d_plot_%d.png", i, j)))
                print(figLoadings[[j]])
                dev.off()
                cat(sprintf("Loadings plot %d for Factor %d saved.\n", j, i))
            }
        } else {
            png(filename = file.path(output_dir, sprintf("loadings_factor_%d.png", i)))
            print(figLoadings)
            dev.off()
            cat(sprintf("Loadings plot for Factor %d saved.\n", i))
        }

        # Scores plot
        figScores <- scores(tempModel,
                            tit = sprintf("Factor %d Scores ", i),
                            plottype = "Bars",
                            classes = vascao$design[, i])

        if (is.list(figScores)) {
            for (j in seq_along(figScores)) {
                png(filename = file.path(output_dir, sprintf("scores_factor_%d_plot_%d.png", i, j)))
                print(figScores[[j]])
                dev.off()
                cat(sprintf("Scores plot %d for Factor %d saved.\n", j, i))
            }
        } else {
            png(filename = file.path(output_dir, sprintf("scores_factor_%d.png", i)))
            print(figScores)
            dev.off()
            cat(sprintf("Scores plot for Factor %d saved.\n", i))
        }
    } else {
        cat(sprintf("Skipping Factor %d (Not significant)\n", i))
    }
}

cat("Plot generation process completed.\n")