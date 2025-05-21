# Install and load required libraries
if (!require(tidyr)) {
  install.packages("tidyr", repos = "https://cran.r-project.org", dependencies = TRUE)
}
if (!require(dplyr)) {
  install.packages("dplyr", repos = "https://cran.r-project.org", dependencies = TRUE)
}
if (!require(tibble)) {
  install.packages("tibble", repos = "https://cran.r-project.org", dependencies = TRUE)
}

library(tidyr)
library(dplyr)
library(tibble)

set.seed(123)

# --- Load and process matrix X ---
auc_data <- read.delim("./datasets/v10.D3.area_under_conc_curve.txt", sep = "\t")
auc_data_unique <- auc_data %>%
  group_by(ccl_name, cpd_name) %>%
  summarise(area_under_curve = mean(area_under_curve, na.rm = TRUE), .groups = 'drop')

X <- pivot_wider(
  auc_data_unique,
  names_from = cpd_name,
  values_from = area_under_curve,
  id_cols = ccl_name
)

X_matrix <- as.matrix(X[, -1])
rownames(X_matrix) <- X$ccl_name

cat("Dimensions of matrix X:", dim(X_matrix), "\n")
cat("First 3 rows and columns of X:\n")
print(X_matrix[1:3, 1:3])

# --- Load and process matrix F ---
cell_info <- read.delim("./datasets/v10.M2.cell_line_info.txt", sep = "\t")

# Define embryonic origin groupings
embryonic_origin_groups <- list(
  Epithelial = c("ENDOMETRIUM", "LARGE_INTESTINE", "LIVER", "LUNG", "OVARY",
                 "PANCREAS", "PROSTATE", "SKIN", "STOMACH", "URINARY_TRACT",
                 "UPPER_AERODIGESTIVE_TRACT", "OESOPHAGUS", "PLEURA"),
  Mesenchymal = c("BONE", "SOFT_TISSUE"),
  Hematopoietic = c("HEMATOPOIETIC_AND_LYMPHOID_TISSUE"),
  Neuroectodermal = c("CENTRAL_NERVOUS_SYSTEM")
)

# Map tissues to embryonic origin
origin_dict <- unlist(lapply(names(embryonic_origin_groups), function(k) {
  setNames(rep(k, length(embryonic_origin_groups[[k]])), embryonic_origin_groups[[k]])
}))

cell_info_processed <- cell_info %>%
  select(ccl_name, ccle_primary_site, growth_mode) %>%
  mutate(ccle_primary_site_embryonic = origin_dict[ccle_primary_site]) %>%
  filter(ccl_name %in% rownames(X_matrix))

cat("Tissue distribution by embryonic origin:\n")
print(table(cell_info_processed$ccle_primary_site_embryonic, useNA = "always"))

cell_info_classified <- cell_info_processed %>%
  filter(!is.na(ccle_primary_site_embryonic))

F_df <- cell_info_classified %>%
  column_to_rownames("ccl_name")

common_cells <- intersect(rownames(F_df), rownames(X_matrix))
X_matrix_filtered <- X_matrix[common_cells, , drop = FALSE]
F_df_filtered <- F_df[common_cells, , drop = FALSE]

ord <- order(rownames(X_matrix_filtered))
X_matrix_filtered <- X_matrix_filtered[ord, , drop = FALSE]
F_df_filtered <- F_df_filtered[ord, , drop = FALSE]

cat("\nAfter filtering for known embryonic origin:\n")
cat("Dimensions of filtered X matrix:", dim(X_matrix_filtered), "\n")
cat("First few rows of filtered X matrix:\n")
print(X[1:5, 1:5])

# Define the design matrix (uncoded, will be coded inside parglmVS)
F_design_matrix_embrionario <- data.frame(
  ccle_primary_site_embryonic = factor(F_df_filtered$ccle_primary_site_embryonic)
)

cat("\nDimensions of the design matrix:", dim(F_design_matrix_embrionario), "\n")
cat("\nPreview of the design matrix:\n")
print(head(F_design_matrix_embrionario))

# Load R scripts from ./R
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

# Create output directory
script_name <- tools::file_path_sans_ext(basename(sys.frames()[[1]]$ofile %||% "cancer_drug_embryonic"))
output_dir <- file.path("examples", paste0(script_name, "_results"))
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Run analysis
res_vs <- parglmVS(X_matrix_filtered, F_design_matrix_embrionario, model = "linear", permutations = 2000)

TVS <- res_vs[[1]]
parglmoVS <- res_vs[[2]]
cat("\nTVS Results:\n")
print(TVS)

write.csv(TVS, file = file.path(output_dir, "TVS.csv"), row.names = FALSE)

vascao <- vasca(parglmoVS, siglev = 0.05)
jsonlite::write_json(vascao, path = file.path(output_dir, "vascao.json"), pretty = TRUE, auto_unbox = TRUE)

cat(sprintf("Total number of factors in VASCA: %d\n", vascao$nFactors))

for (i in seq_len(vascao$nFactors)) {
  factor <- vascao$factors[[i]]
  cat(sprintf("Factor %d: stasig = %s\n", i, factor$stasig))

  if (factor$stasig) {
    cat(sprintf("Factor %d (Significant)\n", i))

    tempModel <- list()
    tempModel$loads <- as.matrix(factor$loadsSorted)
    tempModel$scores <- as.matrix(factor$scoresV)
    tempModel$lvs <- seq_len(ncol(tempModel$loads))
    tempModel$var <- sum(apply(factor$scoresV, 2, var))

    figLoadings <- loadings(tempModel,
                            tit = sprintf("Factor %d Loadings", i),
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

    figScores <- scores(tempModel,
                        tit = sprintf("Factor %d Scores", i),
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