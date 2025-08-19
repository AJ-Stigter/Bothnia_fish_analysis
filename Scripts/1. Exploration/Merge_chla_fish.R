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
chla_data <- read.csv("chla_data.csv", sep = ";",na.strings = "", stringsAsFactors = FALSE,dec=",")
# coord_data <- read_excel("Coordinates_env_data.xlsx", sheet= "Blad1")
# env_data_with_coords <- merge(env_data, coord_data, by.x = "Station", by.y = "Namn")

# 1 Clean data ----
head(chla_data$Sampledate)
# Separate date and time
chla_data$Sampledate <- ymd_hms(chla_data$Sampledate)       # Convert to date-time format
chla_data$Date <- as.Date(chla_data$Sampledate)             # Create separate columns for date and time
chla_data$Time <- format(chla_data$Sampledate, format = "%H:%M:%S")

# Clean temp and salinity based on quality data -> only keep QID 1 (Checked and OK data) and QID 11 (Manually interpolated value)

# Create dataframe with mean and sd for chla per date/station
daily_chla <- chla_data %>%
  group_by(Date, Station) %>%  # Group by both date and station
  summarise(
    Mean_chla = mean(Chl.a..mg.m..3, na.rm = TRUE),
    # SD_chla = sd(chla_data$Chl.a..mg.m..3, na.rm = TRUE),
    Depth = mean(Depth),# Keep location details per station
  ) %>% ungroup()  # Remove grouping after summarizing

# Save dataframe
setwd(here("Data"))
# write.csv(daily_chla, "daily_chla.csv", row.names = FALSE)

 
# 2 Start over ----
setwd(here("Data"))
# Load fish data
fish_data <- read.csv("fish_cleaned_weight.csv")
# Load env_data
 daily_chla <- read.csv("daily_chla.csv")

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
daily_chla$Date <- as.Date(daily_chla$Date)
# Rename date column
daily_chla <- daily_chla %>%
  rename(Date_env = Date)

# Join based on station match and date difference within 14 days
merged_chla <- fuzzy_left_join(
  fish_data_matched,
  daily_chla,
  by = c("Env_Station" = "Station", "Date" = "Date_env"),
  match_fun = list(`==`, function(x, y) abs(difftime(x, y, units = "days")) <= 14)
) %>%
  mutate(Date_Diff = abs(difftime(Date, Date_env, units = "days"))) %>%
  group_by(across(names(fish_data))) %>%  # Ensure each fish row gets best match
  slice_min(Date_Diff, with_ties = FALSE) %>%
  ungroup()

# Clean data
merged_chla_clean <- merged_chla %>%
  filter(!is.na(Mean_chla)) %>%  # Remove rows where no match was found
  mutate(Date_Diff = round(as.numeric(difftime(Date, Date_env, units = "days")))) %>%  # Make sure Date_Diff is in full days
  select(-Station.y) %>%         # Remove duplicate columns
  rename(Station = Station.x)

# Check/set the working directory
setwd(here("Data"))

# Save merged dataframe
write.csv(merged_chla_clean, "fish_chla_data.csv", row.names = FALSE)





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


