# Load necessary package
library(dplyr)

# Set the working directory (update with your actual folder path)
setwd("C:/Users/adria/OneDrive - Radboud Universiteit/Documenten/RU/Master year 2/Internship_UMF/Data")

# Read and merge all files
df <- read.csv("C:/Users/adria/OneDrive - Radboud Universiteit/Documenten/RU/Master year 2/Internship_UMF/Data/merged_fish_data.csv", stringsAsFactors = FALSE)

# 1 Overview of dataset
str(df)   # Shows column names, types, and first few values
dim(df)   # Number of rows and columns
head(df)  # First few rows
tail(df)  # Last few rows

# 2 Identify missing values
colSums(is.na(df))  # Counts missing values per column
# Missing values mostly in vikt (weight) 1518, also in stationsNr1 617 and Information 3650.

summary(df)         # Summary statistics of numeric variables
# Vikt (weight) is in characters, should be numeric
df$Vikt <- as.numeric(gsub(",", ".", df$Vikt))  # Convert weight to numeric

# 3 Explore fish counts and weights
mean(df$Antal1, na.rm = TRUE)  # Average number of fish caught
# mean 19.12524
median(df$Antal1, na.rm = TRUE)
# median 5
range(df$Antal1, na.rm = TRUE)
# range 1 - 2009
df %>%              # Number of fish per species
  group_by(Artbestämning) %>%
  summarise(Total_Count = sum(Antal1, na.rm = TRUE)) %>%
  arrange(desc(Total_Count))
# Abborre most abundant 138311, Mört 110787, Storspigg 51440

# 4 Visualize data
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
df$Fiskedatum1 <- as.Date(df$Fiskedatum1, format = "%Y-%m-%d")  # Adjust date format
ggplot(df, aes(x = Fiskedatum1, y = Antal1)) +
  geom_point(color = "blue", size = 1) +
  labs(title = "Fish Catch Over Time", x = "Date", y = "Number of Fish") +
  theme_minimal()
