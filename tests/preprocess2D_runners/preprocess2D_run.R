args <- commandArgs(trailingOnly=TRUE)
# Take first argument (It should be the dataset)
dataset <- args[1]  

# Load the preprocess2D function
source("../R/preprocess2D.R")   

# Read data from the CSV file
X <- as.matrix(read.csv(dataset, header=FALSE)) 

# Run the preprocess2D function
result <- preprocess2D(X)

# Save the results to CSV files
write.csv(result$xcs, "preprocess2D_r.csv", row.names=FALSE)
write.csv(result$average, "average_r.csv", row.names=FALSE)
write.csv(result$scale, "scale_r.csv", row.names=FALSE)