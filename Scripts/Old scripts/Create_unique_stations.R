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
# fish_data_filtered <- fish_data %>%
#  filter(Station_Cluster %in% valid_clusters$Station_ClusterX)


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
  labs(title = "Fish Sampling Stations - Clusters 2, 4 & 5",
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


# 6 Make clusters based on latitude ----
# Define latitude clusters
fish_data$Latitude_Cluster <- cut(fish_data$Lat_grader, 
                                   breaks = c(60, 61, 62, 63, 64, 65, 66), 
                                   labels = c("60-61", "61-62", "62-63", "63-64", "64-65", "65-66"),
                                   include.lowest = TRUE)
# Count the number of unique years per cluster
year_count_lat <- fish_data %>%
  group_by(Latitude_Cluster) %>%
  summarise(Unique_Years_lat = n_distinct(Year))
# Filter clusters with at least 10 unique years of data
valid_lat_clusters <- year_count_lat %>%
  filter(Unique_Years_lat >= 10)
# Only 2 left -> 20 years of data

# Define larger latitude clusters
fish_data$Latitude_Cluster2 <- cut(fish_data$Lat_grader, 
                                  breaks = c(60, 62, 64, 66), 
                                  labels = c("60-62", "62-64", "64-66"),
                                  include.lowest = TRUE)
# Count the number of unique years per cluster
year_count_lat2 <- fish_data %>%
  group_by(Latitude_Cluster2) %>%
  summarise(Unique_Years_lat2 = n_distinct(Year))
# Filter clusters with at least 10 unique years of data
valid_lat_clusters2 <- year_count_lat2 %>%
  filter(Unique_Years_lat2 >= 10)
# 3 left -> 2 with 20+ years of data, 1 with 10 years of data



# 7 Make map to check locations ----
library(sf)  # For spatial data handling
# library(ggthemes)  # Optional: for better map themes
library(rnaturalearth)
library(rnaturalearthdata)

sweden_map <- ne_countries(scale = "medium", country = c("Sweden", "Finland"), returnclass = "sf")
fish_data_sf <- st_as_sf(fish_data, coords = c("Long_grader", "Lat_grader"), crs = 4326)

# Plot lat clusters
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "darkgrey", color = "lightblue") +  # Sweden basemap
  geom_point(data = fish_data, 
             aes(x = Long_grader, y = Lat_grader, color = as.factor(Latitude_Cluster)), 
             size = 3, alpha = 0.7) +  # Plot stations, colored by Latitude Cluster
  scale_color_manual(values = c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC963", "#FDE725FF", "#FCA50A"), 
                     name = "Latitude Cluster") +  # Custom colors
  coord_sf(xlim = c(17, 25), ylim = c(60, 66)) +  # Adjust these for zoom
  labs(title = "Fish Sampling Stations by Latitude Clusters",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# PLot 3 lat clusters
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "darkgrey", color = "lightblue") +  # Sweden basemap
  geom_point(data = fish_data, 
             aes(x = Long_grader, y = Lat_grader, color = as.factor(Latitude_Cluster2)), 
             size = 3, alpha = 0.7) +  # Plot stations, colored by Latitude Cluster
  scale_color_manual(values = c("#440154FF", "#21908CFF", "#FCA50A"), 
                     name = "Latitude Cluster 2") +  # Custom colors
  coord_sf(xlim = c(17, 25), ylim = c(60, 66)) +  # Adjust these for zoom
  labs(title = "Fish Sampling Stations by Latitude Clusters",
       x = "Longitude", y = "Latitude") +
  theme_minimal()


# 8 Create clusters based on Bothnian bay, the Quark and Bothnian sea ----
# Assigning regions
fish_data$Region_Cluster <- ifelse(fish_data$Lat_grader >= 64.2, "Bothnian Bay",
                                   ifelse(fish_data$Lat_grader >= 63.5 & fish_data$Lat_grader < 64.2, "The Quark",
                                          "Bothnian Sea"))  # Everything below 63.5 is Bothnian Sea

# Make map to check
x11()
ggplot() +
  geom_sf(data = sweden_map, fill = "darkgrey", color = "lightblue") +  # Base map
  geom_point(data = fish_data, 
             aes(x = Long_grader, y = Lat_grader, color = Region_Cluster), 
             size = 3, alpha = 0.7) +  # Plot stations, colored by region
  scale_color_manual(values = c("Bothnian Bay" = "#440154FF", 
                                "The Quark" = "#21908CFF", 
                                "Bothnian Sea" = "#FDE725FF"),
                     name = "Region") +  # Custom colors
  coord_sf(xlim = c(16, 25), ylim = c(60, 66)) +  # Zoomed to Gulf of Bothnia
  labs(title = "Fish Sampling Stations by Region",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# Check unique years
years_per_region <- fish_data %>%
  group_by(Region_Cluster) %>%
  summarise(Num_Unique_Years = n_distinct(Year))


# 9 Look at data distribution for each region ----
# Count data occurrences per year per region
yearly_counts <- fish_data %>%
  group_by(Region_Cluster, Year) %>%
  summarise(Count = n(), .groups = "drop")
print(yearly_counts)  # Shows how much data per year per region

# Count data occurrences per month per year per region
# Extract the month
fish_data$Month <- month(fish_data$Fiskedatum1)
monthly_counts <- fish_data %>%
  group_by(Region_Cluster, Year, Month) %>%
  summarise(Count = n(), .groups = "drop")
print(monthly_counts) # Shows how much data per month per year per region

# Plot results
# Yearly data distribution per region
x11()
ggplot(yearly_counts, aes(x = Year, y = Count)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ Region_Cluster) +  # All regions on the same y-scale
  labs(title = "Yearly Data Distribution per Region",
       x = "Year", y = "Count of Data Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing.y = unit(2, "lines"),  # More space between regions
        panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border around facets
        strip.text = element_text(face = "bold"))  # Make region labels stand out
# Monthly data distribution per region
x11()
ggplot(monthly_counts, aes(x = Month, y = Count)) +
  geom_col(fill = "darkorange") +
  facet_wrap(~ Region_Cluster, scales = "fixed") +  # Same scale for all regions
  scale_x_continuous(breaks = 1:12, labels = 1:12) +  # Ensures labels are 1 to 12
  labs(title = "Monthly Data Distribution per Region",
       x = "Month", y = "Count of Data Points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing.y = unit(2, "lines"),  # More space between regions
        panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border around facets
        strip.text = element_text(face = "bold"))  # Make region labels stand out


# 10 Look at biomass trends over time per region ----
# Mean calculated weight per year
biomass_summary <- fish_data %>%
  group_by(Year, Region_Cluster) %>%
  summarise(mean_biomass = mean(Beräknad_vikt1, na.rm = TRUE),
            sd_biomass = sd(Beräknad_vikt1, na.rm = TRUE),
            n_samples = n()) %>%
  ungroup()

# Make a plot
x11()
ggplot(biomass_summary, aes(x = Year, y = mean_biomass, group = Region_Cluster)) +  
  geom_line(color = "blue") +  # Connect points for each region
  geom_point(size = 2, color = "red") +  # Points for each year
  geom_errorbar(aes(ymin = pmax(mean_biomass - sd_biomass, 0),  
                    ymax = mean_biomass + sd_biomass), 
                width = 0.2, color = "black") +  
  labs(title = "Mean Biomass Trends per Region", 
       x = "Year", 
       y = "Mean Biomass (Beräknad_vikt1)") +
  facet_wrap(~Region_Cluster, scales = "fixed") +  # Ensure all regions have the same scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing.y = unit(2, "lines"),  # More space between regions
        panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border around facets
        strip.text = element_text(face = "bold"))  # Make region labels stand out

# Change plot to have same scale and remove std-error bar
x11()
ggplot(biomass_summary, aes(x = Year, y = mean_biomass, group = Region_Cluster)) +  
  geom_line(color = "blue") +  # Connect points for each region
  geom_point(size = 2, color = "red") +  # Points for each year
  labs(title = "Mean Biomass Trends per Region", 
       x = "Year", 
       y = "Mean Biomass (Beräknad_vikt1)") +
  facet_wrap(~Region_Cluster, scales = "fixed") +  # Same y-axis scale across all plots
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust x-axis text angle
        panel.spacing.y = unit(2, "lines"),  # More space between regions
        panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add border around facets
        strip.text = element_text(face = "bold"))  # Make region labels stand out

# Summarize total biomass per year and region
biomass_summary <- fish_data %>%
  group_by(Region_Cluster, Year) %>%
  summarize(total_biomass = sum(Beräknad_vikt1, na.rm = TRUE),  # Sum total biomass per year
            num_samples = n()) %>%  # Count number of samples per year
  mutate(mean_total_biomass = total_biomass / num_samples)  # Average biomass per sample

# Plot
x11()
ggplot(biomass_summary, aes(x = Year, y = mean_total_biomass, group = Region_Cluster)) +  
  geom_line(color = "blue") +  # Line for trend
  geom_point(size = 2, color = "red") +  # Data points
  labs(title = "Total Biomass Trends per Region (Averaged Over Samples)", 
       x = "Year", 
       y = "Average Total Biomass per Sample (kg)") +
  facet_wrap(~Region_Cluster, scales = "fixed") +  # Keep scales the same
  theme_minimal()
