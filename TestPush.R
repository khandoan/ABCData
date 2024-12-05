
# A.Descriptive Statistics

# Load necessary libraries
library(ggplot2)

# Load the data (replace 'path/to/frutasysemillas.csv' with the correct path or GitHub URL)
data <- data.frame (frutasysemillas)

# Descriptive Statistics for Nseeds
nseeds_mean <- mean(data$Nseeds, na.rm = TRUE)
nseeds_se <- sd(data$Nseeds, na.rm = TRUE) / sqrt(sum(!is.na(data$Nseeds)))

# Descriptive Statistics for FrDiam
frdiam_mean <- mean(data$FrDiam, na.rm = TRUE)
frdiam_se <- sd(data$FrDiam, na.rm = TRUE) / sqrt(sum(!is.na(data$FrDiam)))

# Print results
cat("Nseeds: Mean =", nseeds_mean, ", SE =", nseeds_se, "\n")
cat("FrDiam: Mean =", frdiam_mean, ", SE =", frdiam_se, "\n")

# Frequency Distribution and Density Curve for Nseeds
ggplot(data, aes(x = Nseeds)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Frequency Distribution and Density Curve: Nseeds") +
  xlab("Nseeds") +
  ylab("Density") +
  theme_minimal()

# Frequency Distribution and Density Curve for FrDiam
ggplot(data, aes(x = FrDiam)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Frequency Distribution and Density Curve: FrDiam") +
  xlab("FrDiam") +
  ylab("Density") +
  theme_minimal()

# C. Correlation and Regression Analysis 

# Load necessary libraries
library(ggplot2)

# Load the data (adjust URL/path as needed)
data <- data.frame(frutasysemillas)  # Replace with actual file location

# View the structure of the data
str(data)

# Correlation Analysis between Nseeds and FrDiam
correlation <- cor(data$FrDiam, data$Nseeds, use = "complete.obs", method = "pearson")
cat("Correlation between Fruit Diameter and Seed Count:", correlation, "\n")

# Scatterplot with Linear Fit
ggplot(data, aes(x = FrDiam, y = Nseeds)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Scatterplot with Linear Fit: Seed Count vs Fruit Diameter",
       x = "Fruit Diameter",
       y = "Seed Count") +
  theme_minimal()

# Linear Regression Model
lm_model <- lm(Nseeds ~ FrDiam, data = data)
summary(lm_model)

# Plot diagnostic plots for regression model
par(mfrow = c(2, 2))  # Set plotting area
plot(lm_model)


#E.Visualization Techniques

# Load necessary libraries
library(ggplot2)

# Load the data (adjust URL/path as needed)
data <- data.frame(frutasysemillas)  # Replace with your file location

# Preview the data structure
head(data)

# Box Plot
ggplot(data, aes(x = Treatment, y = Nseeds, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Box Plot of Seed Counts by Treatment",
       x = "Treatment",
       y = "Seed Count") +
  theme_minimal()

# Calculate means and confidence intervals
summary_data <- data %>%
  group_by(Treatment) %>%
  summarise(Mean = mean(Nseeds, na.rm = TRUE),
            SE = sd(Nseeds, na.rm = TRUE) / sqrt(n()),
            Lower = Mean - qt(1 - 0.05 / 2, n() - 1) * SE,
            Upper = Mean + qt(1 - 0.05 / 2, n() - 1) * SE)

# Bar Chart with Error Bars
ggplot(summary_data, aes(x = Treatment, y = Mean, fill = Treatment)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  labs(title = "Mean Seed Count per Treatment with Confidence Intervals",
       x = "Treatment",
       y = "Mean Seed Count") +
  theme_minimal()

# Scatterplot
ggplot(data, aes(x = FrDiam, y = Nseeds)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatterplot of Seed Count vs Fruit Diameter",
       x = "Fruit Diameter",
       y = "Seed Count") +
  theme_minimal()

# Violin Plot
install.packages("dplyr")
ggplot(data, aes(x = Treatment, y = Nseeds, fill = Treatment)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", outlier.color = "red") +
  labs(title = "Violin Plot of Seed Counts by Treatment",
       x = "Treatment",
       y = "Seed Count") +
  theme_minimal()
