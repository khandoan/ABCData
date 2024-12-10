# Load data
data <- read.csv("DATA/frutasysemillas.csv")

# Transform 'Nseeds': Replace values <= 2 with 0
data$Nseeds2 <- data$Nseeds <= 2

# Remove rows with missing Nseeds
data_clean <- na.omit(data[, c("Treatment", "Block", "Nseeds")])

# Perform ANOVA
anova_result <- aov(Nseeds ~ Treatment + Block, data = data_clean)
summary(anova_result)

# Tukey HSD test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# Agricolae HSD test
library(agricolae)
agricolae_result <- HSD.test(anova_result, "Treatment", group = TRUE)
print(agricolae_result$groups)

# Shapiro-Wilk Test (for normality)
library(dplyr)
group_counts_nseeds <- data_clean %>%
  group_by(Treatment) %>%
  summarise(unique_count = n_distinct(Nseeds))

# Filter treatments with more than one unique value in 'Nseeds'
data_filtered_nseeds <- data_clean %>%
  filter(Treatment %in% group_counts_nseeds$Treatment[group_counts_nseeds$unique_count > 1])

shapiro_result_nseeds <- data_filtered_nseeds %>%
  group_by(Treatment) %>%
  summarise(p_value = shapiro.test(Nseeds)$p.value)

print(shapiro_result_nseeds)
ggplot(data_filtered_nseeds, aes(x = Nseeds)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  facet_wrap(~ Treatment) +
  theme_minimal() +
  labs(
    title = "Histogram of Number of Seeds (Modified)",
    x = "Number of Seeds (Nseeds)",
    y = "Frequency"
  )

# QQ Plot by Treatment
ggplot(data_filtered_nseeds, aes(sample = Nseeds)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Treatment) +
  theme_minimal() +
  labs(
    title = "QQ Plot of Number of Seeds by Treatment (Modified)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )
# Kruskal-Wallis Test
kruskal_result <- kruskal.test(Nseeds ~ Treatment, data = data_clean)
print(kruskal_result)

# Create summary data for plotting
summary_data <- data_clean %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Nseeds, na.rm = TRUE),
    se = sd(Nseeds, na.rm = TRUE) / sqrt(n())
  )

# Merge Tukey groups with summary data
tukey_groups <- data.frame(
  Treatment = rownames(agricolae_result$groups),
  Group = agricolae_result$groups$groups
)

summary_data$Treatment <- factor(summary_data$Treatment, levels = c("B", "C", "D", "E", "A"))

# Define custom colors for the bars
custom_colors <- c(
  "B" = "#1f77b4",  # Blue
  "C" = "#ff7f0e",  # Orange
  "D" = "#2ca02c",  # Green
  "E" = "#d62728",  # Red
  "A" = "#9467bd"   # Purple
)
summary_data <- left_join(summary_data, tukey_groups, by = "Treatment")

# Define bar plot aesthetics
custom_colors <- c(
  "B" = "#1f77b4",
  "C" = "#ff7f0e",
  "D" = "#2ca02c",
  "E" = "#d62728",
  "A" = "#9467bd"
)
x_axis_labels <- c(
  "(-) Control (B)",
  "CuGA (C)",
  "ZnRep (D)",
  "CapRep (E)",
  "+ Control (A)"
)

# Bar plot
ggplot(summary_data, aes(x = Treatment, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_text(aes(label = Group, y = mean + se + 0.1), vjust = 0) +
  labs(
    title = "Mean Number of Seeds by Treatment",
    x = "Treatment",
    y = "Mean Number of Seeds",
    fill = "Treatments"
  ) +
  theme_minimal() +
  scale_fill_manual(values = custom_colors) +
  scale_x_discrete(labels = x_axis_labels) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.position = "right")



#----------------
data <- na.omit(data)
data <- data %>%
  mutate(SeedPresence = ifelse(Nseeds > 2, "TRUE", "FALSE"))  # Categorize as Seeded or Seedless

# Step 3: Check and force Treatment levels
data$Treatment <- factor(data$Treatment, 
                         levels = c("B", "C", "D", "E", "A"))  # Order: C-, CuGA, ZnRep, CapRep, C+

# Debugging Step: Print treatment levels
print("Treatment levels after ordering:")
print(levels(data$Treatment))

# Step 4: Calculate Percentages by Treatment and SeedPresence
percent_data <- data %>%
  group_by(Treatment, SeedPresence) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Treatment) %>%
  mutate(Percent = (Count / sum(Count)) * 100)

# Debugging Step: Print percent_data
print("Summarized percentage data:")
print(percent_data)

# Step 5: Force the graph to output
ggplot(percent_data, aes(x = Treatment, y = Percent, fill = SeedPresence)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = sprintf("%.1f%%", Percent)),
            position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(title = "Percentage of Seed Presence by Treatment",
       x = "Treatment",
       y = "Percentage",
       fill = "Seed Presence") +
  theme_minimal() +
  scale_x_discrete(labels = c("C-", "CuGA", "ZnRep", "CapRep", "C+"))  # Correct display labels

