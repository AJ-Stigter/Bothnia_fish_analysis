# Load necessary packages
library(dplyr)
library(here)
library(ggplot2)

# Set the working directory
setwd(here("Data"))
# Read merged dataframe
df <- read.csv("merged_fish_data.csv", stringsAsFactors = FALSE,dec=",")


# 1 Overview of dataset ----
str(df)   # Shows column names, types, and first few values
dim(df)   # Number of rows and columns
head(df)  # First few rows
tail(df)  # Last few rows


# 2 Identify missing values ----
colSums(is.na(df))  # Counts missing values per column
# Missing values in vikt (weight) 8383, in Beräknad_vikt1 98, in stationsNr1 617 and in Information 3650.
summary(df)         # Summary statistics of numeric variables


# 3 Explore fish counts and weights ----
mean(df$Antal1, na.rm = TRUE)  # Average number of fish caught
# mean 19.12524
median(df$Antal1, na.rm = TRUE)
# median 5
range(df$Antal1, na.rm = TRUE)
# range 1 - 2009
df %>%              # Number of fish per species
  group_by(Artbestämning) %>%
  summarise(Total_Count = sum(Antal1, na.rm = TRUE)) %>%
  arrange(desc(Total_Count)) %>%
  print(n = Inf)
# Abborre most abundant 138311, Mört 110787, Storspigg 51440
# Torsk (Cod) only 2
mean(df$Vikt, na.rm = TRUE)  # Average weight of caught fish
# mean 1.120844
median(df$Vikt, na.rm = TRUE)
# median 0.276
range(df$Vikt, na.rm = TRUE)
# range 0.001 - 96.264

mean(df$Beräknad_vikt1, na.rm = TRUE)  # Average calculated weight of caught fish
# mean 4920.204
median(df$Beräknad_vikt1, na.rm = TRUE)
# median 0.317
range(df$Beräknad_vikt1, na.rm = TRUE)
# range 0 - 45256948


# 3.1 Explore weights ----
# Find the row with the extreme weight
extreme_weight_fish <- df %>%
  filter(Beräknad_vikt1 == 45256948)
# View the result
print(extreme_weight_fish)
# Look for more high calculated weights
high_weight_fish <- df[!is.na(df$Beräknad_vikt1) & df$Beräknad_vikt1 > quantile(df$Beräknad_vikt1, 0.99, na.rm = TRUE), ]
# View the outliers
View(high_weight_fish)
# Look at correlation between weight and calculated weight
# Remove rows with missing weights
cleaned_weight_data <- df[!is.na(df$Vikt) & !is.na(df$Beräknad_vikt1), ]
# Calculate the correlation
correlation <- cor(cleaned_weight_data$Vikt, cleaned_weight_data$Beräknad_vikt1, use = "complete.obs")
# Print the result
cat("Correlation between weight and calculated weight:", correlation, "\n")
# Pearson correlation between weight and calculated weight: 0.01153255 

# Plot to vizualize the relationship
plot(cleaned_weight_data$Vikt, cleaned_weight_data$Beräknad_vikt1,
     xlab = "Actual Weight (Vikt)", 
     ylab = "Calculated Weight (Beräknad_vikt1)", 
     main = "Correlation Between Actual and Calculated Weight",
     pch = 16, col = "blue")
abline(lm(Beräknad_vikt1 ~ Vikt, data = cleaned_weight_data), col = "red")
# Spearman correlation if relationship is non-linear
correlation_spearman <- cor(cleaned_weight_data$Vikt, cleaned_weight_data$Beräknad_vikt1, method = "spearman")
cat("Spearman correlation:", correlation_spearman, "\n")
# Spearman correlation = 0.9506941
# Possible explanation: Spearman looks at rank, not exact values. Outliers are therefore less likely to break linear correlation

# Exclude extreme weights and NA's to look at correlation again
threshold_weight <- df[!is.na(df$Beräknad_vikt1) & df$Beräknad_vikt1 < 150, ]
# Calculate correlation
correlation_threshold_pearson <- cor(threshold_weight$Vikt, threshold_weight$Beräknad_vikt1, use = "complete.obs")
# Print result
cat("Pearson correlation (thresholded data):", correlation_threshold_pearson, "\n")
# Pearson correlation (thresholded data): 0.9939339 
# Plot the results
# Plot the correlation between weight and calculated weight
x11()
plot(threshold_weight$Vikt, threshold_weight$Beräknad_vikt1,
     main = "Correlation between Weight and Calculated Weight",
     xlab = "Weight (Vikt)",
     ylab = "Calculated Weight (Beräknad_vikt1)",
     pch = 19, col = "blue")
# Add a regression line
abline(lm(Beräknad_vikt1 ~ Vikt, data = threshold_weight), col = "red", lwd = 2)
# Add a legend
legend("topleft", legend = paste("Pearson Correlation: ", round(correlation_threshold_pearson, 4)),
       col = "red", lwd = 2)
# lm for weight 
 my_lm <- lm(threshold_weight$Beräknad_vikt1 ~ threshold_weight$Vikt)
 summary(my_lm)

# Save dataframe
write.csv(threshold_weight, "fish_weight_threshold.csv")
 
 
# 4 Explore specific species and locations ----
# Select a species (e.g., Abborre)
species_aborre <- "Abborre"        # Filter for specific species
df_aborre <- df %>% filter(Artbestämning == species_aborre)

df_aborre %>%                     # Count fish by location
  group_by(StationsNr1) %>%
  summarise(Total_Fish = sum(Antal1, na.rm = TRUE)) %>%
  arrange(desc(Total_Fish))

ggplot(df_aborre, aes(x = StationsNr1, y = Antal1)) +        # Vizualizing species catches per location
  geom_boxplot(fill = "lightblue") +
  labs(title = paste("Fish count distribution for Aborre", species_aborre), x = "Location", y = "Number of Fish") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 5 Visualize data ----
# Fish count distribution
hist(df$Antal1, breaks = 100, main = "Distribution of Fish Caught", xlab = "Number of Fish", col = "skyblue")
# Fish species frequencies
library(ggplot2)
ggplot(df, aes(x = Artbestämning)) +
  geom_bar(fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Fish Species Counts", x = "Species", y = "Count")
# Total number of fish caught per species
ggplot(df, aes(x = Artbestämning, y = Antal1)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Number of Fish Caught per Species", x = "Species", y = "Total Count")
# Fish count over time
df_time <- df %>%
  group_by(Fiskedatum1 = as.Date(Fiskedatum1)) %>%
  summarise(Total_Fish = sum(Antal1, na.rm = TRUE))
df$Fiskedatum1 <- as.Date(df$Fiskedatum1, format = "%Y-%m-%d")  # Adjust date format
ggplot(df, aes(x = Fiskedatum1, y = Antal1)) +
  geom_point(color = "blue", size = 1) +
  labs(title = "Fish catch over time", x = "Date", y = "Number of Fish") +
  theme_minimal()
# Seasonal patterns
df_season <- df %>%
  mutate(Month = format(Fiskedatum1, "%m")) %>%
  group_by(Month) %>%
  summarise(Avg_Catch = mean(Antal1, na.rm = TRUE))
ggplot(df_season, aes(x = Month, y = Avg_Catch)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Average fish catch per month", x = "Month", y = "Average Catch") +
  theme_minimal()


