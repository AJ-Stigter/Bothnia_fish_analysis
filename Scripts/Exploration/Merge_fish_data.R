# Load necessary package
library(dplyr)
library(here)

# Set the working directory
setwd(here("Data/Raw_data/Stations"))

# Get a list of all CSV files in the folder
files <- list.files(pattern = "*.csv")

# Read and merge all files
data_list <- lapply(files, read.csv, stringsAsFactors = FALSE)
Merged_fish_data <- bind_rows(data_list)

#Empty cells to NA
Merged_fish_data[Merged_fish_data == ""] <- NA

# Set the working directory
setwd(here("Data"))

# Save merged dataframe
write.csv(Merged_fish_data, "fish_stationdata.csv", row.names = FALSE)

