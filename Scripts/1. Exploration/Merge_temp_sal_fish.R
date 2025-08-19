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

# Read Env_data and coordinates
env_data <- read.csv("Env_data.csv", sep = ";",na.strings = "", stringsAsFactors = FALSE,dec=",")
coord_data <- read_excel("Coordinates_env_data.xlsx", sheet= "Blad1")

str(env_data)

head(env_data)
head(coord_data)

env_data_with_coords <- merge(env_data, coord_data, 
                              by.x = "Station", 
                              by.y = "Namn")
head(env_data_with_coords)


# 1 Clean data ----
# Separate date and time
env_data_with_coords$SampleDate <- ymd_hms(env_data_with_coords$SampleDate)       # Convert to date-time format
env_data_with_coords$Date <- as.Date(env_data_with_coords$SampleDate)             # Create separate columns for date and time
env_data_with_coords$Time <- format(env_data_with_coords$SampleDate, format = "%H:%M:%S")

# Clean temp and salinity based on quality data -> only keep QID 1 (Checked and OK data) and QID 11 (Manually interpolated value)
env_data_cleaned <- env_data_with_coords[
  env_data_with_coords$q_Temperature %in% c(1, 11) &
    env_data_with_coords$q_Salinity %in% c(1, 11), ]
summary(env_data_cleaned)

# Create dataframe with mean and sd for temp and salinity per date
daily_temp_salinity <- env_data_cleaned %>%
  group_by(Date, Station) %>%  # Group by both date and station
  summarise(
    Mean_Temperature_ºC = mean(Temperature..ºC., na.rm = TRUE),
    SD_Temperature_ºC = sd(Temperature..ºC., na.rm = TRUE),
    Mean_Salinity_psu = mean(Salinity..psu., na.rm = TRUE),
    SD_Salinity_psu = sd(Salinity..psu., na.rm = TRUE),
    Lat = first(Lat),  # Keep location details per station
    Long = first(Long),
    LatDeg = first(LatDeg),
    LongDeg = first(LongDeg),
    LongMin = first(LongMin),
    Djup = first(Djup),
    Station_depth = first(Station.depth..m.)
  ) %>% ungroup()  # Remove grouping after summarizing

# Save dataframe
setwd(here("Data"))
 write.csv(daily_temp_salinity, "daily_temp_salinity.csv", row.names = FALSE)

 
# 2 Start over ----
setwd(here("Data"))
# Load fish data
fish_data <- read.csv("fish_cleaned_weight.csv")
# Load env_data
daily_temp_salinity <- read.csv("daily_temp_salinity.csv")

# Rename columns to match in both datasets
colnames(fish_data)[colnames(fish_data) == "Fiskedatum1"] <- "Date"
colnames(fish_data)[colnames(fish_data) == "Lat_grader"] <- "Lat"
colnames(fish_data)[colnames(fish_data) == "Long_grader"] <- "Long"

# Round coordinates to 4 decimals to avoid tiny mismatches, but stay accurate (~11m)
fish_data$Lat <- round(fish_data$Lat, 4)
fish_data$Long <- round(fish_data$Long, 4)
daily_temp_salinity$Lat <- round(daily_temp_salinity$Lat, 4)
daily_temp_salinity$Long <- round(daily_temp_salinity$Long, 4)


# 3 Make map to check locations of the stations ----
# Plot on map
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Get base map
sweden_map <- ne_countries(scale = "medium", country = c("Sweden", "Finland"), returnclass = "sf")

# Convert environmental data stations to spatial data
env_stations_sf <- daily_temp_salinity %>%
  distinct(Station, Lat, Long) %>%  # Make sure each station appears once
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  mutate(Source = "Environmental")
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
# Combine both into one dataset
all_stations_sf <- bind_rows(env_stations_sf, fish_stations_sf)

# Plot on a map
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "gray95", color = "gray50") +
  geom_sf(data = all_stations_sf, aes(color = Source), size = 3, alpha = 0.8) +
  labs(title = "Map of Station Locations",
       subtitle = "Environmental vs. Fish Data Stations",
       x = "Longitude", y = "Latitude", color = "Station Type") +
  scale_color_manual(values = c("Environmental" = "darkgreen", "Fish" = "blue")) +
  theme_minimal()

# Count the number of unique years per env station
library(lubridate)
daily_temp_salinity$Date <- as.Date(daily_temp_salinity$Date)  # Adjust the format if necessary
daily_temp_salinity$Year <- format(daily_temp_salinity$Date, "%Y")  # Extract year from Date column
year_count_env <- daily_temp_salinity %>%
  group_by(Station) %>%
  summarise(Unique_Years = n_distinct(Year))


# Make new map with longterm data env stations
# Only keep environmental stations with 20+ years of data
selected_env_stations <- all_stations_sf %>%
  filter(Source == "Environmental" & Station %in% c("RA1", "RA2", "A13", "A5", "B3", "B7", "GA1", "C3", "C14"))
# Get all fish stations
fish_stations <- all_stations_sf %>% filter(Source == "Fish")
# Combine filtered environmental and all fish stations
stations_to_plot <- bind_rows(selected_env_stations, fish_stations)
#Plot env stations
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "gray95", color = "gray50") +
  geom_sf(data = selected_env_stations, aes(color = Source), size = 3, alpha = 0.8) +
  geom_sf_text(data = selected_env_stations, aes(label = Station), size = 3, check_overlap = TRUE) +  
  labs(title = "Map of Station Locations",
       subtitle = "Selected Environmental Stations",
       x = "Longitude", y = "Latitude", color = "Station Type") +
  scale_color_manual(values = c("Environmental" = "darkgreen", "Fish" = "blue")) +
  theme_minimal()
#Plot env and fish stations
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "gray95", color = "gray50") +
  geom_sf(data = stations_to_plot, aes(color = Source), size = 3, alpha = 0.8) +
  geom_sf_text(data = stations_to_plot, aes(label = Station), size = 3, check_overlap = TRUE) +  
  labs(title = "Map of Station Locations",
       subtitle = "Selected Environmental Stations & All Fish Stations",
       x = "Longitude", y = "Latitude", color = "Station Type") +
  scale_color_manual(values = c("Environmental" = "darkgreen", "Fish" = "blue")) +
  theme_minimal()
#Plot matching env and fish stations
matching_env_stations <- all_stations_sf %>%   # Only keep environmental stations close to fish stations
  filter(Source == "Environmental" & Station %in% c("RA1", "RA2", "A13", "B3", "B7", "GA1", "C14"))
matching_stations <- bind_rows(matching_env_stations, fish_stations) #Combine stations
#Plot
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "gray95", color = "gray50") +
  geom_sf(data = matching_stations, aes(color = Source), size = 3, alpha = 0.8) +
  geom_sf_text(data = matching_stations, aes(label = Station), size = 3, check_overlap = TRUE) +  
  labs(title = "Map of Station Locations",
       subtitle = "Matching Environmental & Fish Stations",
       x = "Longitude", y = "Latitude", color = "Station Type") +
  scale_color_manual(values = c("Environmental" = "darkgreen", "Fish" = "blue")) +
  theme_minimal()

# Pair stations
station_pairs <- tibble::tibble(
  fish_station = c("1", "1", "2", "3", "3", "4", "4", "5", "6"),
  env_station = c("RA1", "RA2", "A13", "B3", "B7", "B3", "B7", "GA1", "C14")
)
# Calculate distances between stations
station_pairs_geom <- station_pairs %>%
  left_join(all_stations_sf %>% filter(Source == "Fish") %>% 
              select(fish_station = Station, Fångstområde1, fish_geom = geometry),
            by = "fish_station") %>%
  left_join(all_stations_sf %>% filter(Source == "Environmental") %>% 
              select(env_station = Station, env_geom = geometry),
            by = "env_station") %>%
  mutate(
    distance_km = as.numeric(st_distance(fish_geom, env_geom, by_element = TRUE)) / 1000
  )

# Check/set the working directory
setwd(here("Data"))
# Save merged dataframe
write.csv(station_pairs_geom, "Preliminary_results/station_pairs_distances.csv", row.names = FALSE)

# 4 Merge env and fish data ----
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

### Filter and prepare environmental data
env_data_filtered <- daily_temp_salinity %>%
  filter(!is.na(Station)) %>%
  select(Station, Date, Lat, Long, Mean_Temperature_ºC, Mean_Salinity_psu) %>%
  rename(
    Env_Station = Station,
    Date_env = Date,
    Lat_env = Lat,
    Long_env = Long,
    Temperature = Mean_Temperature_ºC,
    Salinity = Mean_Salinity_psu
  )

# Join based on station match and date difference within 14 days
merged_data <- fuzzy_left_join(
  fish_data_matched,
  env_data_filtered,
  by = c("Env_Station" = "Env_Station", "Date" = "Date_env"),
  match_fun = list(`==`, function(x, y) abs(difftime(x, y, units = "days")) <= 14)
) %>%
  mutate(Date_Diff = abs(difftime(Date, Date_env, units = "days"))) %>%
  group_by(across(names(fish_data))) %>%  # Ensure each fish row gets best match
  slice_min(Date_Diff, with_ties = FALSE) %>%
  ungroup()

# Clean data
merged_data_clean <- merged_data %>%
  filter(!is.na(Temperature)) %>%  # Remove rows where no match was found
  mutate(Date_Diff = round(as.numeric(difftime(Date, Date_env, units = "days")))) %>%  # Make sure Date_Diff is in full days
  select(-Env_Station.y) %>%         # Remove duplicate columns
  rename(Env_Station = Env_Station.x)

# Check/set the working directory
setwd(here("Data"))

# Save merged dataframe
write.csv(merged_data_clean, "fish_env_data.csv", row.names = FALSE)



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


