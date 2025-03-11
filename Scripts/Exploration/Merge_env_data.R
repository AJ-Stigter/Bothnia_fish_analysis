# 0 Load necessary packages ----
library(here)
# install.packages ("readxl")
library(readxl)
library(dplyr)
# install.packages("lubridate")
library(lubridate)

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
  group_by(Date) %>%
  summarise(
    Mean_Temperature_ºC = mean(Temperature..ºC., na.rm = TRUE),
    SD_Temperature_ºC = sd(Temperature..ºC., na.rm = TRUE),
    Mean_Salinity_psu = mean(Salinity..psu., na.rm = TRUE),
    SD_Salinity_psu = sd(Salinity..psu., na.rm = TRUE),
    # Keep all the location and station info
    Lat = first(Lat),
    Long = first(Long),
    LatDeg = first(LatDeg),
    LongDeg = first(LongDeg),
    LongMin = first(LongMin),
    Station = first(Station),
    Djup = first(Djup),
    Station_depth = first(Station.depth..m.)
  )

# Save dataframe
setwd(here("Data"))
write.csv(daily_temp_salinity, "daily_temp_salinity.csv")

# Load fish data
fish_weight_threshold <- read.csv("fish_weight_threshold.csv")

# Rename columns to match in both datasets
colnames(fish_weight_threshold)[colnames(fish_weight_threshold) == "Fiskedatum1"] <- "Date"
colnames(fish_weight_threshold)[colnames(fish_weight_threshold) == "Lat_grader"] <- "Lat"
colnames(fish_weight_threshold)[colnames(fish_weight_threshold) == "Long_grader"] <- "Long"

# Round coordinates to 3 decimals to avoid tiny mismatches
fish_weight_threshold$Lat <- round(fish_weight_threshold$Lat, 3)
fish_weight_threshold$Long <- round(fish_weight_threshold$Long, 3)
daily_temp_salinity$Lat <- round(daily_temp_salinity$Lat, 3)
daily_temp_salinity$Long <- round(daily_temp_salinity$Long, 3)

# Merge fish and env data
merged_fish_temp_salinity <- merge(fish_weight_threshold, daily_temp_salinity, 
                     by = c("Date", "Lat", "Long"), 
                     all = FALSE)

merged_fish_temp_salinity <- merge(fish_weight_threshold, daily_temp_salinity, 
                                   by = c("Lat", "Long"), 
                                   all = FALSE)

fish_LatLong <- paste(str_sub(fish_weight_threshold$Lat,1,5),
                      str_sub(fish_weight_threshold$Long,1,5),
                      sep=" ")
daily_LatLong <- paste(str_sub(daily_temp_salinity$Lat,1,5),
                       str_sub(daily_temp_salinity$Long,1,5),
                       sep=" ")
which(fish_LatLong %in% daily_LatLong)

str_sub(fish_weight_threshold$Lat,1,5)

bwah <- fish_weight_threshold %>%
  group_by(StationsNr1,Date)%>%
  summarise(count=n(),
            .groups = 'drop') %>%
  group_by(StationsNr1) %>%
  summarise(count=n())

print(bwah[order(bwah$count,decreasing = T),],n=100)

bwah <- fish_weight_threshold %>%
  group_by(StationsNr1,year(Date))%>%
  summarise(count=n(),
            .groups = 'drop') %>%
  group_by(StationsNr1) %>%
  summarise(count=n())

