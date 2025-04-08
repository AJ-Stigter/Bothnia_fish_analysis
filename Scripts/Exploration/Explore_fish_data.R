# Load necessary packages
library(dplyr)
library(here)
library(ggplot2)

# Set the working directory
setwd(here("Data"))
# Read merged dataframe
df <- read.csv("fish_stationdata.csv", stringsAsFactors = FALSE,dec=",")


# 1 Overview of dataset ----
str(df)   # Shows column names, types, and first few values
dim(df)   # Number of rows and columns
head(df)  # First few rows
tail(df)  # Last few rows


# 2 Identify missing values ----
colSums(is.na(df))  # Counts missing values per column
# Missing values in vikt (weight) 7001, in Beräknad_vikt1 348, and in Information 21569
summary(df)         # Summary statistics of numeric variables


# 3 Explore fish counts and weights ----
mean(df$Antal1, na.rm = TRUE)  # Average number of fish caught
# mean 14.7377
median(df$Antal1, na.rm = TRUE)
# median 6
range(df$Antal1, na.rm = TRUE)
# range 1 - 640
df %>%              # Number of fish per species
  group_by(Artbestämning) %>%
  summarise(Total_Count = sum(Antal1, na.rm = TRUE)) %>%
  arrange(desc(Total_Count)) %>%
  print(n = Inf)
# Abborre most abundant 104540, Storspigg 62845, Mört 60554
# Torsk (Cod) only 1
mean(df$Vikt, na.rm = TRUE)  # Average weight of caught fish
# mean 0.758571
median(df$Vikt, na.rm = TRUE)
# median 0.251
range(df$Vikt, na.rm = TRUE)
# range 0.001 - 335.198

mean(df$Beräknad_vikt1, na.rm = TRUE)  # Average estimated weight of caught fish
# mean 0.7201339
median(df$Beräknad_vikt1, na.rm = TRUE)
# median 0.256
range(df$Beräknad_vikt1, na.rm = TRUE)
# range 0.000 - 16.458


# 3.1 Explore weights ----
# Find the row with the high weight
high_weight_fish <- df %>%
  filter(Vikt == 335.198)
# View the result
print(high_weight_fish)
# Look for more high weights
high_measured_weights <- df[!is.na(df$Vikt) & df$Vikt > quantile(df$Vikt, 0.99, na.rm = TRUE), ]
# View the outliers
View(high_measured_weights)
# 3 high unrealistic measured weights
# Look at correlation between weight and calculated weight
# Remove rows with missing weights
cleaned_weight_data <- df[!is.na(df$Vikt) & !is.na(df$Beräknad_vikt1), ]
# Calculate the correlation
correlation <- cor(cleaned_weight_data$Vikt, cleaned_weight_data$Beräknad_vikt1, use = "complete.obs")
# Print the result
cat("Correlation between weight and calculated weight:", correlation, "\n")
# Pearson correlation between weight and calculated weight: 0.2965686

# Plot to vizualize the relationship
x11()
plot(cleaned_weight_data$Vikt, cleaned_weight_data$Beräknad_vikt1,
     xlab = "Measured Weight (Vikt)", 
     ylab = "Estimated Weight (Beräknad_vikt1)", 
     main = "Correlation between measured and estimated weight",
     pch = 16, col = "blue")
abline(lm(Beräknad_vikt1 ~ Vikt, data = cleaned_weight_data), col = "red")
# Spearman correlation if relationship is non-linear
correlation_spearman <- cor(cleaned_weight_data$Vikt, cleaned_weight_data$Beräknad_vikt1, method = "spearman")
cat("Spearman correlation:", correlation_spearman, "\n")
# Spearman correlation = 0.9970253
# Possible explanation: Spearman looks at rank, not exact values. Outliers are therefore less likely to break linear correlation

# Exclude extreme weights and NA's to look at correlation again
threshold_weight <- df[!is.na(df$Vikt) & df$Vikt < 20, ]
# Calculate correlation
correlation_threshold_pearson <- cor(threshold_weight$Vikt, threshold_weight$Beräknad_vikt1, use = "complete.obs")
# Print result
cat("Pearson correlation (thresholded data):", correlation_threshold_pearson, "\n")
# Pearson correlation (thresholded data): 0.9954138 
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

# Clean weight
 cleaned_weight <- df[!is.na(df$Beräknad_vikt1) &  # Remove NA in Beräknad_vikt1
                        # df$Beräknad_vikt1 > 0 &  # Remove 0 in Beräknad_vikt1
                        !(is.na(df$Vikt) & df$Beräknad_vikt1 == 0) &  # Remove NA in Vikt *only if* Beräknad_vikt1 is 0
                        (is.na(df$Vikt) | df$Vikt < 20), ]  # Keep NA in Vikt if Beräknad_vikt1 is > 0, remove if ≥ 20

 
# 4 Explore specific species and locations ----
# Select a species (e.g., Abborre)
species_aborre <- "Abborre"        # Filter for specific species
df_aborre <- df %>% filter(Artbestämning == species_aborre)

df_aborre %>%                     # Count fish by location
  group_by(StationsNr1) %>%
  summarise(Total_Fish = sum(Antal1, na.rm = TRUE)) %>%
  arrange(desc(Total_Fish))

x11()
ggplot(df_aborre, aes(x = StationsNr1, y = Antal1)) +        # Vizualizing species catches per location
  geom_boxplot(fill = "lightblue") +
  labs(title = paste("Fish count distribution for Aborre", species_aborre), x = "Location", y = "Number of Fish") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 5 Visualize data ----
# Fish count distribution
x11()
hist(df$Antal1, breaks = 100, main = "Distribution of Fish Caught", xlab = "Number of Fish", col = "skyblue")
# Fish species frequencies
library(ggplot2)
x11()
ggplot(df, aes(x = Artbestämning)) +
  geom_bar(fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Fish Species Counts", x = "Species", y = "Count")
# Total number of fish caught per species
x11()
ggplot(df, aes(x = Artbestämning, y = Antal1)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Total Number of Fish Caught per Species", x = "Species", y = "Total Count")
# Fish count over time
df_time <- df %>%
  group_by(Fiskedatum1 = as.Date(Fiskedatum1)) %>%
  summarise(Total_Fish = sum(Antal1, na.rm = TRUE))
df$Fiskedatum1 <- as.Date(df$Fiskedatum1, format = "%Y-%m-%d")  # Adjust date format
x11()
ggplot(df, aes(x = Fiskedatum1)) +
  geom_histogram(binwidth = 365, fill = "blue", color = "black") +  # Group by 60-day intervals
  labs(title = "Fish Catch Over Time", x = "Date", y = "Frequency") +
  theme_minimal()
# Seasonal patterns
df_season <- df %>%
  mutate(Month = format(Fiskedatum1, "%m")) %>%
  group_by(Month) %>%
  summarise(Avg_Catch = mean(Antal1, na.rm = TRUE))
x11()
ggplot(df_season, aes(x = Month, y = Avg_Catch)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Average fish catch per month", x = "Month", y = "Average Catch") +
  theme_minimal()


# 6 Make map to check locations ----
library(sf)  # For spatial data handling
# library(ggthemes)  # Optional: for better map themes
library(rnaturalearth)
library(rnaturalearthdata)

sweden_map <- ne_countries(scale = "medium", country = c("Sweden", "Finland"), returnclass = "sf")
fish_data_sf <- st_as_sf(cleaned_weight, coords = c("Long_grader", "Lat_grader"), crs = 4326)

# Plot all stations
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "darkgrey", color = "lightblue") +  
  geom_point(data = cleaned_weight,  # Plot all stations
             aes(x = Long_grader, y = Lat_grader, color = factor(Fångstområde1)), 
             size = 3, alpha = 0.7) +  
  # geom_text(data = cleaned_weight, 
  #          aes(x = Long_grader, y = Lat_grader, label = Fångstområde1), 
  #         color = "black", size = 1, vjust = -1) +  # Adjust size and position
  labs(title = "Fish Sampling Stations",
       x = "Longitude", y = "Latitude",
       color = "Cluster") +  # Legend label
  theme_minimal() +  
  coord_sf(xlim = c(17, 25), ylim = c(60, 66))  # Adjust these limits to zoom in


# Check year distribution
library(lubridate)
cleaned_weight$Fiskedatum1 <- as.Date(cleaned_weight$Fiskedatum1, format = "%Y-%m-%d")  # Adjust the format if necessary
cleaned_weight$Year <- format(cleaned_weight$Fiskedatum1, "%Y")  # Extract year from Date column

# Count the number of unique years per cluster
year_count <- cleaned_weight %>%
  group_by(Fångstområde1) %>%
  summarise(Unique_Years = n_distinct(Year))

# Check whether the data in "Långvindsfjärden" and "Långvindsfjärden reservat" is duplicated or unique
langvind_data <- df[df$Fångstområde1 %in% c("Långvindsfjärden", "Långvindsfjärden reservat"), ]
duplicates <- langvind_data[duplicated(langvind_data) | duplicated(langvind_data, fromLast = TRUE), ]
print(duplicates)
# No duplicates -> Merge "Långvindsfjärden" and "Långvindsfjärden reservat"
cleaned_weight$Fångstområde1[cleaned_weight$Fångstområde1 == "Långvindsfjärden reservat"] <- "Långvindsfjärden"

# 7 Save dataframe ----
write.csv(cleaned_weight, "fish_cleaned_weight.csv", row.names = FALSE)
