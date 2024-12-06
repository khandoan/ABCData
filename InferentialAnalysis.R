#install.packages("dplyr")
#install.packages("knitr")
#install.packages("ggplot2")
#install.packages("agricolae")
library(dplyr)
library(knitr)
library(ggplot2)
library(agricolae)



#----ANOVA----------------
data <- data.frame(frutasysemillas)

# Remove rows with missing Nseeds
data_clean <- na.omit(data[, c("Treatment", "Block", "Nseeds")])

# Perform ANOVA
anova_result <- aov(Nseeds ~ Treatment + Block, data = data_clean)
summary(anova_result)


#---SHAPIRO TEST--------------
group_counts_nseeds <- data_clean %>%
  group_by(Treatment) %>%
  summarise(unique_count = n_distinct(Nseeds))

# Filter out treatments with only one unique value in 'Nseeds'
data_filtered_nseeds <- data_clean %>%
  filter(Treatment %in% group_counts_nseeds$Treatment[group_counts_nseeds$unique_count > 1])

# Perform the Shapiro-Wilk test on filtered data
shapiro_result_nseeds <- data_filtered_nseeds %>%
  group_by(Treatment) %>%
  summarise(p_value = shapiro.test(Nseeds)$p.value)

# Print the results
print(shapiro_result_nseeds)
# Plot the histogram and QQ plot again
ggplot(data_filtered_nseeds, aes(x = Nseeds)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  facet_wrap(~ Treatment) +
  theme_minimal() +
  labs(title = "Histogram of Number of Seeds",
       x = "Number of Seeds (Nseeds)",
       y = "Frequency")

ggplot(data_filtered_nseeds, aes(sample = Nseeds)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Treatment) +
  theme_minimal() +
  labs(title = "QQ Plot by Treatment")
#This plots show whether this analysis follows Normality.
#The graphs show non-normal distribution

#---Agricolae Test (Tukey HSD)--------
tukey_result <- HSD.test(anova_result, "Treatment", group = TRUE)
tukey_groups <- data.frame(
  Treatment = rownames(tukey_result$groups),
  Group = tukey_result$groups$groups
)

# Calculate means and standard errors for each treatment
summary_data <- filtered_data %>%
  group_by(Treatment) %>%
  summarise(
    mean = mean(Nseeds, na.rm = TRUE),
    se = sd(Nseeds, na.rm = TRUE) / sqrt(n())
  )
  #The standard error measures the precision of the sample mean as an estimate of the population mean.
  #Without dividing by the sqrt the result would simply be the standard deviation, 
  #which measures the spread of individual data points, not the precision of the group mean.


# Merge Tukey groups with summary data
summary_data <- left_join(summary_data, tukey_groups, by = "Treatment")

# Create the bar plot with error bars and Tukey groups
ggplot(summary_data, aes(x = Treatment, y = mean, fill = Treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  geom_text(aes(label = Group, y = mean + se + 0.1), vjust = 0) +
  labs(title = "Mean Number of Seeds by Treatment",
       x = "Treatment",
       y = "Mean Number of Seeds") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

