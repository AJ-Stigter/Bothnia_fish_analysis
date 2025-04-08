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

# Create dataframe with mean and sd for temp and salinity per dateY
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

########## Skip this part, doesn't work yet
# Merge fish and env data
merged_fish_temp_salinity <- merge(fish_data, daily_temp_salinity, 
                     by = c("Date", "Lat", "Long"), 
                     all = FALSE)

merged_fish_temp_salinity <- merge(fish_data, daily_temp_salinity, 
                                   by = c("Lat", "Long"), 
                                   all = FALSE)
# No correlation between locations -> not able to merge dataframes
### Ask Leon ###
fish_LatLong <- paste(str_sub(fish_weight_threshold$Lat,1,5),
                      str_sub(fish_weight_threshold$Long,1,5),
                      sep=" ")
daily_LatLong <- paste(str_sub(daily_temp_salinity$Lat,1,5),
                       str_sub(daily_temp_salinity$Long,1,5),
                       sep=" ")
which(fish_LatLong %in% daily_LatLong)

str_sub(fish_weight_threshold$Lat,1,5)
#############

# Group by station and number of samples per station
bwah <- fish_weight_threshold %>%
  group_by(StationsNr1,Date)%>%
  summarise(count=n(),
            .groups = 'drop') %>%
  group_by(StationsNr1) %>%
  summarise(count=n())

print(bwah[order(bwah$count,decreasing = T),],n=100)

# Group by station and number of years sampled
bwah <- fish_weight_threshold %>%
  group_by(StationsNr1,year(Date))%>%
  summarise(count=n(),
            .groups = 'drop') %>%
  group_by(StationsNr1) %>%
  summarise(count=n())
# Stations with most years of data: 11 (21y) and 12-16 (20y).

######## Skip for now
# Keep only 6 stations with most years of data
stations_to_keep <- c("11", "12", "13", "14", "15", "16")   # Define stations to keep
station_cleaned_data <- fish_weight_threshold %>% filter(StationsNr1 %in% stations_to_keep)
#######


# 3 Merging dataset without perfect matches ----
# install.packages("fuzzyjon")
library(fuzzyjoin)

# Convert Date columns to Date format (if not already)
fish_data <- fish_data %>%
  mutate(Date = as.Date(Date))
daily_temp_salinity <- daily_temp_salinity %>%
  mutate(Date = as.Date(Date))

# Perform fuzzy join to match by Lat, Long, and allow for a X-day date difference
merged_data <- fish_data %>%
  fuzzy_left_join(daily_temp_salinity, 
                  by = c("Lat", "Long", "Date" = "Date"), 
                  match_fun = list(`==`, 
                                   ~ abs(.x - .y) <= 3, # Lat/Long tolerance
                                   function(x, y) abs(difftime(x, y, units = "days")) <= 100)) %>%  # Date tolerance
  ungroup() %>%  # Remove unnecessary grouping if present
  filter(!is.na(Mean_Temperature_ºC))  # Keep only rows where temp data exists

# Plot on map
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Convert merged_data to spatial data for plotting
merged_data_sf <- st_as_sf(merged_data, 
                           coords = c("Long.x", "Lat.x"),  # Longitude and Latitude for original location
                           crs = 4326)  # Set CRS to WGS 84 (standard for geographic coordinates)
# For fuzzy matched locations
merged_data_sf_fuzzy <- st_as_sf(merged_data, 
                                 coords = c("Long.y", "Lat.y"),  # Longitude and Latitude for fuzzy matched location
                                 crs = 4326)  # Set CRS to WGS 84
# Get base map
sweden_map <- ne_countries(scale = "medium", country = c("Sweden", "Finland"), returnclass = "sf")

x11()
ggplot() +
  # Plot the base map for Sweden
  geom_sf(data = sweden_map, fill = "lightgrey", color = "black") +
  
  # Plot the original location points in blue
  geom_sf(data = merged_data_sf, aes(color = "Original Location"), size = 3) +
  
  # Plot the fuzzy matched location points in red
  geom_sf(data = merged_data_sf_fuzzy, aes(color = "Fuzzy Matched Location"), size = 3) +
  
  labs(title = "Original vs. Fuzzy Matched Locations",
       subtitle = "Blue = Original Location, Red = Fuzzy Matched Location",
       x = "Longitude", y = "Latitude") +
  scale_color_manual(values = c("Original Location" = "blue", "Fuzzy Matched Location" = "red")) +
  theme_minimal()


# 4 Make map to check locations of the stations ----
# Convert environmental data stations to spatial data
env_stations_sf <- daily_temp_salinity %>%
  distinct(Station, Lat, Long) %>%  # Make sure each station appears once
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  mutate(Source = "Environmental")
# Convert fish data stations to spatial data
fish_stations_sf <- fish_data %>%
  distinct(Fångstområde1, Lat, Long) %>%  # Make sure each station appears once
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>%
  mutate(Source = "Fish")
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


# Try new merge
merged_data5 <- fuzzy_left_join(
  fish_data %>% filter(!is.na(Lat), !is.na(Long), !is.na(Date)),
  daily_temp_salinity %>% filter(!is.na(Lat), !is.na(Long), !is.na(Date)),
  by = c("Date", "Lat", "Long"),
  match_fun = list(
    function(x, y) abs(difftime(x, y, units = "days")) <= 21,
    ~ abs(.x - .y) <= 0.1,
    ~ abs(.x - .y) <= 0.1
  )
) %>% filter(!is.na(Mean_Temperature_ºC))
# Multiple env data records are matched to 1 fish data point

# Plot
# Convert the fish locations (from fish_data) to sf
fish_points <- st_as_sf(merged_data5, coords = c("Long.x", "Lat.x"), crs = 4326)
# Convert the matched environmental locations to sf
env_points <- st_as_sf(merged_data5, coords = c("Long.y", "Lat.y"), crs = 4326)
# Plot both sets of points on the Sweden map
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "lightgrey", color = "black") +
  geom_sf(data = fish_points, aes(color = "Fish Station"), size = 2, alpha = 0.7) +
  geom_sf(data = env_points, aes(color = "Matched Environment"), size = 2, alpha = 0.7) +
  labs(
    title = "Fish Sampling vs. Matched Environmental Stations",
    subtitle = "Blue = Fish Station | Red = Matched Environment Station",
    x = "Longitude", y = "Latitude"
  ) +
  scale_color_manual(values = c("Fish Station" = "blue", "Matched Environment" = "red")) +
  theme_minimal()


# New code to prevent multiple matches but only closest point match
# Add a unique ID to fish data before join
fish_data <- fish_data %>%
  filter(!is.na(Lat), !is.na(Long), !is.na(Date)) %>%
  mutate(Fish_RowID = row_number())
# Fuzzy join
merged_data6 <- fuzzy_left_join(
  fish_data,
  daily_temp_salinity %>% filter(!is.na(Lat), !is.na(Long), !is.na(Date)),
  by = c("Date", "Lat", "Long"),
  match_fun = list(
    function(x, y) abs(difftime(x, y, units = "days")) <= 21,
    ~ abs(.x - .y) <= 0.1,
    ~ abs(.x - .y) <= 0.1
  )
) %>%
  filter(!is.na(Mean_Temperature_ºC)) %>%
  mutate(
    Date_Diff = abs(as.numeric(difftime(Date.x, Date.y, units = "days"))),
    Spatial_Dist = geosphere::distHaversine(matrix(c(Long.x, Lat.x), ncol = 2),
                                            matrix(c(Long.y, Lat.y), ncol = 2)),
    Combined_Score = Date_Diff + Spatial_Dist / 1000  # days + km
  ) %>%
  group_by(Fish_RowID) %>%
  slice_min(order_by = Combined_Score, with_ties = FALSE) %>%
  ungroup()

# Count the number of unique years per station
year_count <- merged_data6 %>%
  group_by(Fångstområde1) %>%
  summarise(Unique_Years = n_distinct(Year))


# Count the number of unique years per env station
library(lubridate)
daily_temp_salinity$Fiskedatum1 <- as.Date(daily_temp_salinity$Date, format = "%Y-%m-%d")  # Adjust the format if necessary
daily_temp_salinity$Year <- format(daily_temp_salinity$Date, "%Y")  # Extract year from Date column
year_count_env <- daily_temp_salinity %>%
  group_by(Station) %>%
  summarise(Unique_Years = n_distinct(Year))


# *SKIP* 4 Map the locations of the stations ----

# Install and load packages
# install.packages("sf")
# install.packages("rnaturalearth")
# install.packages("rnaturaleartdata")
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Convert to spatial data
stations_sf <- st_as_sf(station_cleaned_data, coords = c("Long", "Lat"), crs = 4326)

# Get base map
sweden_map <- ne_countries(scale = "medium", country = c("Sweden", "Finland"), returnclass = "sf")

# Plot the map
X11()
ggplot() +
  geom_sf(data = sweden_map, fill = "darkgrey", color = "lightblue") +  # Add Sweden base map
  geom_point(data = station_cleaned_data, 
             aes(x = Long, y = Lat, color = StationsNr1), 
             size = 3) +  # Plot station points
  geom_text(data = station_cleaned_data, 
            aes(x = Long, y = Lat, label = StationsNr1), 
            vjust = -1, hjust = 0.5, size = 3, color = "black") +  # Add station number labels
  labs(title = "Fish Sampling Stations in the Gulf of Bothnia", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# 9 locations visible in map, even though there are only 6 stations.
# Find unique coordinates per station
unique_coords <- station_cleaned_data %>%
  group_by(StationsNr1) %>%
  distinct(Lat, Long) %>%
  summarize(n_coords = n(), .groups = "drop")
print(unique_coords)

# Plot locations per station on map
# Filter only Station 11
station_11_data <- station_cleaned_data %>%
  filter(StationsNr1 == 11)

X11()
ggplot() +
  geom_sf(data = sweden_map, fill = "darkgrey", color = "lightblue") +  # Add Sweden base map
  geom_point(data = station_11_data, 
             aes(x = Long, y = Lat), 
             color = "red", size = 3) +  # Plot Station 11 points in red
  geom_text(data = station_11_data, 
            aes(x = Long, y = Lat, label = StationsNr1), 
            vjust = -1, hjust = 0.5, size = 3, color = "black") +  # Add station number labels
  labs(title = "Sampling Locations for Station 11 in the Gulf of Bothnia", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# Filter only Station 12
station_12_data <- station_cleaned_data %>%
  filter(StationsNr1 == 12)

X11()
ggplot() +
  geom_sf(data = sweden_map, fill = "darkgrey", color = "lightblue") +  # Add Sweden base map
  geom_point(data = station_12_data, 
             aes(x = Long, y = Lat), 
             color = "red", size = 3) +  # Plot Station 12 points in red
  geom_text(data = station_12_data, 
            aes(x = Long, y = Lat, label = StationsNr1), 
            vjust = -1, hjust = 0.5, size = 3, color = "black") +  # Add station number labels
  labs(title = "Sampling Locations for Station 12 in the Gulf of Bothnia", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# Stations have multiple different coordinates, so need to check that before continuing.


# *SKIP* 5 Practice make dataset (without accurate data!!) ----

# Convert Date columns to Date format (if not already)
fish_data <- fish_data %>%
  mutate(Date = as.Date(Date))

daily_temp_salinity <- daily_temp_salinity %>%
  mutate(Date = as.Date(Date))

# Create Year-Month columns for both datasets
fish_data <- fish_data %>%
  mutate(Year_Month = paste(year(Date), sprintf("%02d", month(Date)), sep = "-"))

daily_temp_salinity <- daily_temp_salinity %>%
  mutate(Year_Month = paste(year(Date), sprintf("%02d", month(Date)), sep = "-"))

# Match by Year-Month
matched_data <- inner_join(fish_data, daily_temp_salinity, by = "Year_Month", relationship = "many-to-many")

# Convert fish and environmental data to spatial points
fish_sf <- st_as_sf(matched_data, coords = c("Long.x", "Lat.x"), crs = 4326)
env_sf <- st_as_sf(matched_data, coords = c("Long.y", "Lat.y"), crs = 4326)

# Perform a spatial join to match the closest points
final_matched_data <- st_join(fish_sf, env_sf, join = st_nearest_feature)


###############


# Match by exact Date, allowing many-to-many
matched_data <- inner_join(
  fish_weight_threshold, 
  daily_temp_salinity, 
  by = "Date",                           # Match on exact date
  relationship = "many-to-many"          # Allow many-to-many matches
)

# Convert fish and environmental data to spatial points
fish_sf <- st_as_sf(matched_data, coords = c("Long.x", "Lat.x"), crs = 4326)
env_sf <- st_as_sf(matched_data, coords = c("Long.y", "Lat.y"), crs = 4326)

# Perform a spatial join to match the closest points
final_matched_data <- st_join(fish_sf, env_sf, join = st_nearest_feature)

final_matched_data <- final_matched_data %>%
  select(-ends_with(".y")) %>% 
  rename_with(~ gsub("\\.x$", "", .))  # Remove '.x' suffix from fish data

head(final_matched_data)
# Remove excess columns
final_matched_data <- final_matched_data %>% select(-X.y)
str(final_matched_data)
# Remove geometry
final_matched_data <- st_drop_geometry(final_matched_data)

# Save test dataframe
write.csv(final_matched_data, "test_fish_env_merge.csv", row.names = FALSE)




