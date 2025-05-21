# Read the data
datos <- read.csv("./datasets/gene_expression.csv") 
X <- as.matrix(datos[, c("Gene.One", "Gene.Two")])
F <- as.matrix(as.factor(datos$Cancer.Present))

# Display dimensions and first ten rows of X and F
cat("Dimensions of X:", dim(X), "\n")
cat("First 10 rows of X:\n")
print(head(X, 10))

cat("Dimensions of F:", dim(F), "\n")
cat("First 10 rows of F:\n")
print(head(F, 10))

# Add all R scripts in the R directory to the search path
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

# Determine script name to use in output path
script_name <- tools::file_path_sans_ext(basename(sys.frames()[[1]]$ofile %||% "gene_expression"))
output_dir <- file.path("examples", paste0(script_name, "_results"))
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Set the seed for reproducibility
set.seed(123)

# Perform the analysis
res_vs <- parglmVS(X, F, model = "linear", permutations = 2000) # Linear model because there is only one factor
TVS <- res_vs[[1]]
parglmoVS <- res_vs[[2]]
print(TVS)

# Save TVS as CSV
write.csv(TVS, file = file.path(output_dir, "TVS.csv"), row.names = FALSE)

# Call vasca
vascao <- vasca(parglmoVS, siglev = 0.05)

# Save vascao as JSON
jsonlite::write_json(vascao, path = file.path(output_dir, "vascao.json"), pretty = TRUE, auto_unbox = TRUE)

cat(sprintf("Total number of factors in vascao: %d\n", vascao$nFactors))

# Iterate over all factors
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

        # Loadings plot
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

        # Scores plot
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
