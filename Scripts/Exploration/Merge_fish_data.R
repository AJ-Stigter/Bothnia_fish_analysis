# Load necessary package
library(dplyr)

# Set the working directory (update with your actual folder path)
setwd("C:/Users/adria/OneDrive - Radboud Universiteit/Documenten/RU/Master year 2/Internship_UMF/Data/Raw_data")

# Get a list of all CSV files in the folder
files <- list.files(pattern = "*.csv")

# Read and merge all files
data_list <- lapply(files, read.csv, stringsAsFactors = FALSE)
Merged_fish_data <- bind_rows(data_list)

# Save the merged dataset
write.csv(Merged_fish_data, "C:/Users/adria/OneDrive - Radboud Universiteit/Documenten/RU/Master year 2/Internship_UMF/Data/Merged_fish_data.csv", row.names = FALSE)

# Print summary of merged data
print(dim(merged_data))  # Check number of rows and columns
head(merged_data)        # Preview first few rows

