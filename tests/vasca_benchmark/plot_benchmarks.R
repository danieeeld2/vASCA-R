library(ggplot2)

# Read the data
data <- read.csv("vasca_benchmark/benchmark_results.csv")

# Convert time to numeric if not already
data$Time <- as.numeric(data$Time.s.)

# Convert SigLevel to factor (discrete) for proper plotting
data$SigLevel <- as.factor(data$SigLevel)

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

# 1. Combined plot for all datasets and significance levels
p1 <- ggplot(data, aes(x=Dataset, y=Time, fill=Language)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(~SigLevel, labeller = labeller(SigLevel = function(x) paste("Significance Level:", x))) +
  labs(title="Vasca Function: Execution Time Comparison between R and Octave",
       x="Dataset",
       y="Execution time (seconds)") +
  scale_fill_brewer(palette="Set1") +
  white_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("vasca_benchmark/vasca_comparison_all.png", p1, width=12, height=8, bg="white")

# 2. Plot comparing performance by dataset
p2 <- ggplot(data, aes(x=Language, y=Time, fill=SigLevel)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(~Dataset, scales="free_y", ncol=2) +
  labs(title="Vasca Function: Performance by Dataset",
       x="Implementation Language",
       y="Execution time (seconds)") +
  scale_fill_brewer(palette="Set2", name="Significance\nLevel") +
  white_theme

ggsave("vasca_benchmark/vasca_by_dataset.png", p2, width=10, height=8, bg="white")

# 3. Plot comparing languages by significance level
p3 <- ggplot(data, aes(x=Dataset, y=Time, color=Language, group=Language)) +
  geom_point(size=3) +
  geom_line(linewidth=1) +
  facet_wrap(~SigLevel, labeller = labeller(SigLevel = function(x) paste("Significance Level:", x))) +
  labs(title="Vasca Function: Language Performance Across Datasets",
       x="Dataset",
       y="Execution time (seconds)") +
  scale_color_brewer(palette="Set1") +
  white_theme

ggsave("vasca_benchmark/vasca_language_comparison.png", p3, width=12, height=6, bg="white")

# 4. Heatmap style visualization
p4 <- ggplot(data, aes(x=Dataset, y=Language, fill=Time)) +
  geom_tile() +
  facet_wrap(~SigLevel, labeller = labeller(SigLevel = function(x) paste("Significance Level:", x))) +
  labs(title="Vasca Function: Execution Time Heatmap",
       x="Dataset",
       y="Language") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name="Time (s)") +
  white_theme +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("vasca_benchmark/vasca_heatmap.png", p4, width=10, height=6, bg="white")
