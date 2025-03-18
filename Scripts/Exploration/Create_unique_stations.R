# Load necessary packages
library(dplyr)
library(here)
library(ggplot2)

# Set the working directory
setwd(here("Data"))
# Read merged dataframe
fish_data <- read.csv("fish_weight_threshold.csv")

# 1 Create unique station identifiers ----
fish_data$Unique_Station <- paste(fish_data$Lokal, fish_data$Fångstområde, fish_data$StationsNr1, sep = "_")

# Create unique station numbers based on unique coordinates
unique_stations <- fish_data %>%
  select(Lat_grader, Long_grader) %>%
  distinct() %>%
  arrange(Lat_grader, Long_grader) %>%
  mutate(Station_ID = paste0("Station_", row_number()))  # Assign unique station numbers

# Merge back into original dataset
fish_data <- left_join(fish_data, unique_stations, by = c("Lat_grader", "Long_grader"))


# 2 Create unique stations based on location ----
# install.packages("dbscan")
library(dbscan)

# Prepare coordinate matrix
coords <- fish_data[, c("Long_grader", "Lat_grader")]

# Run DBSCAN (eps = max distance in decimal degrees ~500m)
clusters <- dbscan(coords, eps = 0.005, minPts = 1)$cluster

# Assign cluster numbers
fish_data$Station_Cluster <- clusters
# 899 clusters left

# Reduce number of clusters, clusters need to have at least 10 years of data
fish_data$Fiskedatum1 <- as.Date(fish_data$Fiskedatum1, format = "%Y-%m-%d")  # Adjust the format if necessary
fish_data$Year <- format(fish_data$Fiskedatum1, "%Y")  # Extract year from Date column

# Count the number of unique years per cluster
year_count <- fish_data %>%
  group_by(Station_Cluster) %>%
  summarise(Unique_Years = n_distinct(Year))
# Filter clusters with at least 10 unique years of data
valid_clusters <- year_count %>%
  filter(Unique_Years >= 10)
# Only 2 clusters left



# 3 Create new clusters based on larger location ranges ----
# Run DBSCAN (eps = max distance in decimal degrees ~1000m, minimal 10 datapoints)
clusters2 <- dbscan(coords, eps = 0.01, minPts = 10)$cluster

# Assign cluster numbers
fish_data$Station_Cluster2 <- clusters2
# 219 clusters left
# Assign cluster numbers, replacing 0 (noise points) with NA
fish_data$Station_Cluster2 <- ifelse(clusters2 == 0, NA, clusters2)
# Check the result
table(is.na(fish_data$Station_Cluster2))  # To check how many NA values (noise points) you have
# 941 NA's, 19839 datapoints left

# Count the number of unique years per cluster
year_count2 <- fish_data %>%
  group_by(Station_Cluster2) %>%
  summarise(Unique_Years2 = n_distinct(Year))
# Filter clusters with at least 10 unique years of data
valid_clusters2 <- year_count2 %>%
  filter(Unique_Years2 >= 10)
# Only 3 left


# 4 Create new clusters based on even larger location ranges ----
# Run DBSCAN (eps = max distance in decimal degrees ~10.000m, minimal 10 datapoints)
clusters3 <- dbscan(coords, eps = 0.1, minPts = 10)$cluster

# Assign cluster numbers
fish_data$Station_Cluster3 <- clusters3
# 29 clusters left
# Assign cluster numbers, replacing 0 (noise points) with NA
# fish_data$Station_Cluster3 <- ifelse(clusters3 == 0, NA, clusters3)
# Check the result
# table(is.na(fish_data$Station_Cluster3))  # To check how many NA values (noise points) you have
# 12 NA's, 20768 datapoints left

# Count the number of unique years per cluster
year_count3 <- fish_data %>%
  group_by(Station_Cluster3) %>%
  summarise(Unique_Years3 = n_distinct(Year))
# Filter clusters with at least 10 unique years of data
valid_clusters3 <- year_count3 %>%
  filter(Unique_Years3 >= 10)
# Only 3 left




# Join the filtered clusters back into the original data
fish_data_filtered <- fish_data %>%
  filter(Station_Cluster %in% valid_clusters$Station_Cluster)


# 5 Make map to check locations ----
library(sf)  # For spatial data handling
# library(ggthemes)  # Optional: for better map themes
library(rnaturalearth)
library(rnaturalearthdata)

sweden_map <- ne_countries(scale = "medium", country = c("Sweden", "Finland"), returnclass = "sf")
fish_data_sf <- st_as_sf(fish_data, coords = c("Long_grader", "Lat_grader"), crs = 4326)

# Plot certain station clusters
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "darkgrey", color = "lightblue") +  
  # Map colors to the clusters
  geom_point(data = fish_data[fish_data$Station_Cluster3 %in% c(4, 2, 5), ],  # Plot only selected clusters
             aes(x = Long_grader, y = Lat_grader, color = factor(Station_Cluster3)),  # Color by cluster number
             size = 3) +  
  geom_text(data = fish_data[fish_data$Station_Cluster3 %in% c(4, 2, 5), ],  # Filter labels too
            aes(x = Long_grader, y = Lat_grader, label = Station_Cluster3), 
            color = "black", size = 3, vjust = -1) +    # Adjust size and position
  labs(title = "Fish Sampling Stations - Clusters 11 & 12",
       x = "Longitude", y = "Latitude") +
  scale_color_manual(values = c("4" = "red", "2" = "blue", "5" = "green" )) +  # Assign specific colors to clusters
  theme_minimal()+  
  coord_sf(xlim = c(17, 25), ylim = c(60, 66))  # Adjust these limits to zoom in

# PLot all station clusters
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "darkgrey", color = "lightblue") +  
  geom_point(data = fish_data,  # Plot all clusters
             aes(x = Long_grader, y = Lat_grader, color = factor(Station_Cluster3)), 
             size = 1, alpha = 0.7) +  
  # geom_text(data = fish_data, 
  #          aes(x = Long_grader, y = Lat_grader, label = Station_Cluster), 
  #         color = "black", size = 1, vjust = -1) +  # Adjust size and position
  labs(title = "Fish Sampling Stations - All Clusters",
       x = "Longitude", y = "Latitude",
       color = "Cluster") +  # Legend label
  theme_minimal() +  
  coord_sf(xlim = c(17, 25), ylim = c(60, 66))  # Adjust these limits to zoom in



# Join the filtered clusters back into the original data
fish_data_filtered <- fish_data %>%
  filter(Station_Cluster %in% valid_clusters$Station_Cluster)