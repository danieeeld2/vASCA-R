library(ggplot2)

# Read the data
data <- read.csv("parglmVS_benchmark/benchmark_results.csv")

# Convert time to numeric if not already
data$Time <- as.numeric(data$Time.s.)

# Define a white background theme with grid lines
white_theme <- theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    axis.line = element_line(color = "black"),
    text = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

# Create a combined plot for all models
p1 <- ggplot(data, aes(x=Permutations, y=Time, color=Language, shape=Model)) +
  geom_point(size=3) +
  geom_line(aes(linetype=Model)) +
  labs(title="Execution Time Comparison between R and Octave",
       x="Number of permutations",
       y="Execution time (seconds)") +
  scale_color_brewer(palette="Set1") +
  scale_x_log10(breaks = unique(data$Permutations), 
                labels = scales::comma(unique(data$Permutations))) +
  white_theme

# Save the combined plot
ggsave("parglmVS_benchmark/benchmark_comparison_all.png", p1, width=12, height=8, bg="white")

# Create separate plots for each model
for (model_name in unique(data$Model)) {
  subset_data <- data[data$Model == model_name,]
  p2 <- ggplot(subset_data, aes(x=Permutations, y=Time, color=Language)) +
    geom_point(size=3) +
    geom_line(linewidth=1) +
    labs(title=paste("Model:", model_name),
         x="Number of permutations",
         y="Execution time (seconds)") +
    scale_color_brewer(palette="Set1") +
    scale_x_log10(breaks = unique(subset_data$Permutations), 
                  labels = scales::comma(unique(subset_data$Permutations))) +
    white_theme
  
  ggsave(paste0("parglmVS_benchmark/benchmark_", model_name, ".png"), p2, width=10, height=6, bg="white")
}

# Create bar chart to compare models grouped by language
p3 <- ggplot(data, aes(x=Model, y=Time, fill=Language)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~Permutations, scales="free_y") +
  labs(title="Model comparison by number of permutations",
       x="Model",
       y="Execution time (seconds)") +
  scale_fill_brewer(palette="Set1") +
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    legend.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "lightgrey"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

ggsave("parglmVS_benchmark/benchmark_models_by_permutations.png", p3, width=14, height=10, bg="white")

# Create a log-scale plot to better visualize performance across all permutation ranges
p4 <- ggplot(data, aes(x=Permutations, y=Time, color=Language, shape=Model)) +
  geom_point(size=3) +
  geom_line(aes(linetype=Model)) +
  labs(title="Execution Time Comparison (Log-Log Scale)",
       x="Number of permutations (log scale)",
       y="Execution time (seconds, log scale)") +
  scale_color_brewer(palette="Set1") +
  scale_x_log10(breaks = unique(data$Permutations), 
                labels = scales::comma(unique(data$Permutations))) +
  scale_y_log10() +
  white_theme

ggsave("parglmVS_benchmark/benchmark_comparison_logscale.png", p4, width=12, height=8, bg="white")
