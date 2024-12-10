
# Load necessary library
#install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Load the dataset
data <- data.frame(frutasysemillas)

# Frequency Distribution for Number of Seeds (Nseeds)
ggplot(data, aes(x = Nseeds)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Frequency Distribution of Number of Seeds",
       x = "Number of Seeds",
       y = "Frequency") +
  theme_minimal()

# Density Curve for Number of Seeds (Nseeds)
ggplot(data, aes(x = Nseeds)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  labs(title = "Density Curve of Number of Seeds",
       x = "Number of Seeds",
       y = "Density") +
  theme_minimal()

# Frequency Distribution for Fruit Diameter (FrDiam)
ggplot(data, aes(x = FrDiam)) +
  geom_histogram(binwidth = 2, fill = "green", color = "black") +
  labs(title = "Frequency Distribution of Fruit Diameter",
       x = "Fruit Diameter (mm)",
       y = "Frequency") +
  theme_minimal()

# Density Curve for Fruit Diameter (FrDiam)
ggplot(data, aes(x = FrDiam)) +
  geom_density(fill = "lightgreen", alpha = 0.7) +
  labs(title = "Density Curve of Fruit Diameter",
       x = "Fruit Diameter (mm)",
       y = "Density") +
  theme_minimal()
# Frequency Distribution for Number of Seeds (Nseeds) by Treatment
ggplot(data, aes(x = Nseeds, fill = Treatment)) +
  geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
  facet_wrap(~Treatment) +
  labs(title = "Frequency Distribution of Number of Seeds by Treatment",
       x = "Number of Seeds",
       y = "Frequency") +
  theme_minimal()

# Density Curve for Number of Seeds (Nseeds) by Treatment
ggplot(data, aes(x = Nseeds, color = Treatment, fill = Treatment)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Treatment) +
  labs(title = "Density Curve of Number of Seeds by Treatment",
       x = "Number of Seeds",
       y = "Density") +
  theme_minimal()

# Frequency Distribution for Fruit Diameter (FrDiam) by Treatment
ggplot(data, aes(x = FrDiam, fill = Treatment)) +
  geom_histogram(binwidth = 2, color = "black", alpha = 0.7) +
  facet_wrap(~Treatment) +
  labs(title = "Frequency Distribution of Fruit Diameter by Treatment",
       x = "Fruit Diameter (mm)",
       y = "Frequency") +
  theme_minimal()

# Density Curve for Fruit Diameter (FrDiam) by Treatment
ggplot(data, aes(x = FrDiam, color = Treatment, fill = Treatment)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~Treatment) +
  labs(title = "Density Curve of Fruit Diameter by Treatment",
       x = "Fruit Diameter (mm)",
       y = "Density") +
  theme_minimal()

#chi-squared test to compare seeded versus seedless fruit 
# Step 1: Create a new column for Seeded and Seedless categorization
data <- data %>%
  mutate(Seeded = ifelse(Nseeds > 0, "Seeded", "Seedless"))  # Categorize fruits

# Step 2: Create a contingency table for Seeded vs Seedless fruits by Treatment
contingency_table <- table(data$Seeded, data$Treatment)

# Step 3: Calculate percentages
percentage_table <- prop.table(contingency_table, margin = 2) * 100  # Percentage by Treatment

# Step 4: Run the Chi-Squared Test
chi_squared_result <- chisq.test(contingency_table)

# Step 5: Extract Chi-squared statistic and p-value
x2_value <- chi_squared_result$statistic
p_value <- chi_squared_result$p.value

# Step 6: Print results
cat("Contingency Table:\n")
print(contingency_table)
cat("\nPercentage Table (Seeded vs Seedless by Treatment):\n")
print(round(percentage_table, 2))  # Rounded for clarity
cat("\nChi-squared Statistic (X^2):", x2_value, "\n")
cat("P-value:", p_value, "\n")

#Figure 5 Plot
# Load required libraries
library(ggplot2)
library(dplyr)

# Step 1: Exclude NA values in key columns
data <- data %>%
  filter(!is.na(Nseeds), !is.na(FrDiam), !is.na(Treatment))  # Remove rows with NA values

# Step 2: Categorize Seed Presence
data <- data %>%
  mutate(SeedPresence = ifelse(Nseeds > 0, "TRUE", "FALSE"))  # Categorize as Seeded or Seedless

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

