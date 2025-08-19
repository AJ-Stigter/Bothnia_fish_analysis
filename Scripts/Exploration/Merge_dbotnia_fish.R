# 0 Load necessary packages ----
library(here)
# install.packages ("readxl")
library(readxl)
library(dplyr)
# install.packages("lubridate")
library(lubridate)
library(ggplot2)

# Set the working directory
setwd(here("Data/Raw_data"))

# Read chla_data and coordinates
dbo_data <- read.csv("dbothnia_20241230.csv", sep = ";",na.strings = "", stringsAsFactors = FALSE,dec=",",header=T)
# coord_data <- read_excel("Coordinates_env_data.xlsx", sheet= "Blad1")
# env_data_with_coords <- merge(env_data, coord_data, by.x = "Station", by.y = "Namn")

# 1 Clean data ----
head(dbo_data$SampleDate)
# Separate date and time
dbo_data$SampleDate <- ymd_hms(dbo_data$SampleDate)       # Convert to date-time format
dbo_data$Date <- as.Date(dbo_data$SampleDate)             # Create separate columns for date and time
dbo_data$Time <- format(dbo_data$Sampledate, format = "%H:%M:%S")

# Remove NA's in date and column time & only keep relevant stations
dbo_data <- dbo_data %>%
  select(-Time) %>%                                # Remove 'Time' column
  filter(!is.na(Date)) %>%                         # Remove rows with NA in 'Date'
  filter(Station %in% c("RA1", "A13", "B7", "GA1", "C14"))  # Keep only selected stations

# Clean based on quality data -> only keep QID 1 (Checked and OK data) and QID 11 (Manually interpolated value)
# Named vector of parameters and their corresponding q-code columns
param_qcode_pairs <- c(
  "pH" = "q_pH",
  "Total.N..μM." = "q_Total.N",
  "NO3..μM." = "q_NO3",
  "NO2..μM." = "q_NO2",
  "NH4..μM." = "q_NH4",
  "Total.P..μM." = "q_Total.P",
  "PO4.P..μM." = "q_PO4.P",
  "SiO4..μM." = "q_SiO4",
  "DOC..μM." = "q_DOC"
)
# Loop through each pair and set the parameter to NA if q-code is not 1 or 11
for (param in names(param_qcode_pairs)) {
  q_col <- param_qcode_pairs[[param]]
  dbo_data[[param]] <- ifelse(dbo_data[[q_col]] %in% c(1, 11), dbo_data[[param]], NA)
}


# Create dataframe with mean per date/station
# Custom mean function that returns NA if all values are NA
safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(mean(x, na.rm = TRUE))
  }
}
# Vector of parameters to average
params_to_average <- c(
  "Seccidepth..m.",
  "pH",
  "Total.N..μM.",
  "NO3..μM.",
  "NO2..μM.",
  "NH4..μM.",
  "Total.P..μM.",
  "PO4.P..μM.",
  "SiO4..μM.",
  "DOC..μM."
)
# Group by Station and Date, then take mean of selected parameters
dbo_data_mean <- dbo_data %>%
  group_by(Station, Date) %>%
  summarise(across(all_of(params_to_average), safe_mean), .groups = "drop")

# Save dataframe
setwd(here("Data"))
# write.csv(dbo_data_mean, "dbo_data_mean.csv", row.names = FALSE)

 
# 2 Start over ----
setwd(here("Data"))
# Load fish data
fish_data <- read.csv("fish_cleaned_weight.csv")
# Load env_data
dbo_data_mean <- read.csv("dbo_data_mean.csv")

# Rename columns to match in both datasets
colnames(fish_data)[colnames(fish_data) == "Fiskedatum1"] <- "Date"
colnames(fish_data)[colnames(fish_data) == "Lat_grader"] <- "Lat"
colnames(fish_data)[colnames(fish_data) == "Long_grader"] <- "Long"

# Round coordinates to 4 decimals to avoid tiny mismatches, but stay accurate (~11m)
fish_data$Lat <- round(fish_data$Lat, 4)
fish_data$Long <- round(fish_data$Long, 4)

# Convert fish data stations to spatial data
fish_stations_sf <- fish_data %>%
  filter(!is.na(Lat), !is.na(Long), !is.na(Fångstområde1)) %>%
  group_by(Fångstområde1) %>%
  summarise(
    Lat = mean(Lat),
    Long = mean(Long),
    .groups = "drop"
  ) %>%
  arrange(desc(Lat)) %>%  # Sort from north (high lat) to south (low lat)
  mutate(
    Station = as.character(row_number()),  # Now 1 is most north, 6 most south
    Source = "Fish"
  ) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)


# 3 Merge env and fish data ----
library(dplyr)
library(lubridate)
library(fuzzyjoin)

# Define station pairs
paired_stations <- tibble::tibble(
  Station = c("1", "2", "3", "4", "5", "6"),
  Env_Station = c("RA1", "A13", "B7", "B7", "GA1", "C14")
)

# Add station number to fish stations
station_mapping <- fish_stations_sf %>%
  st_drop_geometry() %>%
  select(Fångstområde1, Station)
fish_data_with_station <- fish_data %>%
  left_join(station_mapping, by = "Fångstområde1")

# Add matching info to fish data
fish_data_matched <- fish_data_with_station %>%
  filter(!is.na(Station)) %>%
  left_join(paired_stations, by = "Station")

# Convert date columns to Date class
fish_data_matched$Date <- as.Date(fish_data_matched$Date)
dbo_data_mean$Date <- as.Date(dbo_data_mean$Date)
# Rename date column
dbo_data_mean <- dbo_data_mean %>%
  rename(Date_env = Date)

# Join based on station match and date difference within 14 days
merged_dbo <- fuzzy_left_join(
  fish_data_matched,
  dbo_data_mean,
  by = c("Env_Station" = "Station", "Date" = "Date_env"),
  match_fun = list(`==`, function(x, y) abs(difftime(x, y, units = "days")) <= 14)
) %>%
  mutate(Date_Diff = abs(difftime(Date, Date_env, units = "days"))) %>%
  group_by(across(names(fish_data))) %>%  # Ensure each fish row gets best match
  slice_min(Date_Diff, with_ties = FALSE) %>%
  ungroup()

# Clean data
 merged_dbo_clean <- merged_dbo %>%
  filter(!is.na(Date_env)) %>%  # Remove rows where no match was found
  mutate(Date_Diff = round(as.numeric(difftime(Date, Date_env, units = "days")))) %>%  # Make sure Date_Diff is in full days
  select(-Station.y) %>%         # Remove duplicate columns
  rename(Station = Station.x)

# Check/set the working directory
setwd(here("Data"))

# Save merged dataframe
write.csv(merged_dbo_clean, "fish_dbo_data.csv", row.names = FALSE)





# Possible extra step ----
# Join based on station match and date difference within 30 days
merged_data2 <- fuzzy_left_join(
  fish_data_matched,
  env_data_filtered,
  by = c("Env_Station" = "Env_Station", "Date" = "Date_env"),
  match_fun = list(`==`, function(x, y) abs(difftime(x, y, units = "days")) <= 30)
) %>%
  mutate(Date_Diff = abs(difftime(Date, Date_env, units = "days"))) %>%
  group_by(across(names(fish_data))) %>%  # Ensure each fish row gets best match
  slice_min(Date_Diff, with_ties = FALSE) %>%
  ungroup()

# Clean data
merged_data_clean2 <- merged_data2 %>%
  filter(!is.na(Temperature)) %>%  # Remove rows where no match was found
  mutate(Date_Diff = round(as.numeric(difftime(Date, Date_env, units = "days")))) %>%  # Make sure Date_Diff is in full days
  select(-Env_Station.y) %>%         # Remove duplicate columns
  rename(Env_Station = Env_Station.x)

# Check/set the working directory
setwd(here("Data"))

# Save merged dataframe
write.csv(merged_data_clean2, "fish_env_data2.csv", row.names = FALSE)


