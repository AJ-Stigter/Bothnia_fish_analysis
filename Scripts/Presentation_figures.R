# Make figures for presentation ----

# 0 Preparation ----
# install and load packages
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(ggspatial)

# 1 Make map of entire study area ----
# Define map limits
map_limits <- c(xmin = 11, xmax = 30, ymin = 54, ymax = 66)

# Extended limits for blue background (a bit bigger)
xlim_bg <- c(10, 31)
ylim_bg <- c(53, 67)

# Load countries you want to plot
countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin %in% c("Sweden", "Finland", "Denmark", "Norway", "Estonia", "Latvia", "Lithuania", "Poland", "Germany", "Russia", "Belarus"))

# Labels only for Sweden, Denmark, Finland
labels <- data.frame(
  name = c("Sweden", "Finland", "Baltic Sea", "Gulf of\nBothnia"),
  lon = c(15, 26, 19, 19.5),
  lat = c(60, 63, 56.5, 62),
  fontface = c("italic", "italic", "bold", "bold")
)

# Plot map
x11()
ggplot() +
  # Blue water background rectangle (exact to limits)
  geom_rect(aes(xmin = xlim_bg[1], xmax = xlim_bg[2], ymin = ylim_bg[1], ymax = ylim_bg[2]),
            fill = "lightblue", color = NA) +
  # Grey countries
  geom_sf(data = countries, fill = "grey80", color = "grey50") +
  # Labels for selected countries and seas
  geom_text(data = labels,
            aes(x = lon, y = lat, label = name, fontface = fontface),
            size = 4, color = "black") +
  # Zoom in on map limits
  coord_sf(xlim = c(map_limits["xmin"], map_limits["xmax"]),
           ylim = c(map_limits["ymin"], map_limits["ymax"])) +
  theme_minimal() +
  labs(
    title = "Gulf of Bothnia region",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )



# 2. Define map limits
map_limits <- c(xmin = 11, xmax = 30, ymin = 54, ymax = 66)
xlim_bg <- c(10, 31)
ylim_bg <- c(53, 67)

# 3. Load countries
countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(admin %in% c("Sweden", "Finland", "Denmark", "Norway", 
                      "Estonia", "Latvia", "Lithuania", "Poland", 
                      "Germany", "Russia", "Belarus"))

# 4. Labels
labels <- data.frame(
  name = c("Sweden", "Finland", "Baltic Sea", "Gulf of\nBothnia"),
  lon = c(15, 26, 19, 19.5),
  lat = c(60, 63, 56.5, 62),
  fontface = c("italic", "italic", "bold", "bold")
)

# 5. Plot
x11()
ggplot() +
  # Blue water background
  geom_rect(aes(xmin = xlim_bg[1], xmax = xlim_bg[2],
                ymin = ylim_bg[1], ymax = ylim_bg[2]),
            fill = "lightblue", color = NA) +
  # Countries
  geom_sf(data = countries, fill = "grey80", color = "grey50") +
  # Labels
  geom_text(data = labels,
            aes(x = lon, y = lat, label = name, fontface = fontface),
            size = 4, color = "black") +
  # North arrow (top right)
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                          pad_x = unit(0.7, "cm"),  # move left (increase to move further left)
                          pad_y = unit(0.3, "cm"))+
  # Scale bar (bottom right)
  annotation_scale(location = "br", width_hint = 0.2) +
  # Zoom
  coord_sf(xlim = c(map_limits["xmin"], map_limits["xmax"]),
           ylim = c(map_limits["ymin"], map_limits["ymax"])) +
  theme_minimal() +
  labs(
    title = "Gulf of Bothnia region",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  )


# 2 Make map of station locations ----
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
  filter(Source == "Environmental" & Station %in% c("RA1", "A13", "B7", "GA1", "C14"))
matching_stations <- bind_rows(matching_env_stations, fish_stations) #Combine stations



# Set map limits for the Gulf of Bothnia
xlim_bg <- c(16, 25.5)     # Adjust longitude range
ylim_bg <- c(60, 66)   # Adjust latitude range

# Separate datasets
fish_points <- matching_stations[matching_stations$Source == "Fish", ]
env_points  <- matching_stations[matching_stations$Source == "Environmental", ]

# Pair stations
station_pairs <- tibble::tibble(
  fish_station = c("1", "2", "3", "4", "5", "6"),
  env_station = c("RA1", "A13", "B7", "B7", "GA1", "C14")
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
# Create LINESTRING sf object connecting fish and env station geometries
station_lines <- station_pairs_geom %>%
  rowwise() %>%
  mutate(line = st_sfc(
    st_linestring(matrix(
      c(st_coordinates(fish_geom), st_coordinates(env_geom)),
      ncol = 2,
      byrow = TRUE
    )),
    crs = st_crs(fish_geom)
  )) %>%
  ungroup() %>%
  st_as_sf()

#Plot the final map
x11()
ggplot() +
  # Light blue background rectangle (sea)
  geom_rect(aes(xmin = xlim_bg[1], xmax = 26, ymin = 59, ymax = ylim_bg[2]),
            fill = "lightblue", color = NA) +
  # Countries layer
  geom_sf(data = sweden_map, fill = "gray95", color = "gray50") +
  # Station lines
  geom_sf(data = station_lines, aes(geometry = line), color = "red", size = 2) +
  # Station points with shape & transparency
  geom_sf(data = matching_stations, 
          aes(color = Source, shape = Source), 
          size = 3, alpha = 0.8) +
  # Station labels: above for Fish, below for Environmental
  geom_sf_text(data = fish_points,
               aes(label = Fångstområde1),
               nudge_y = 0.15, size = 3, check_overlap = TRUE) +
  geom_sf_text(data = env_points,
               aes(label = Station),
               nudge_y = -0.1, size = 3, check_overlap = TRUE) +
  # Titles and color legend
  labs(title = "Map of Station Locations",
       subtitle = "Matching Environmental & Fish Stations",
       x = "Longitude", y = "Latitude",
       color = "Station Type",
       shape = "Station Type") +
  # Custom colors and shapes
  scale_color_manual(values = c("Environmental" = "darkgreen", "Fish" = "blue")) +
  scale_shape_manual(values = c("Environmental" = 16, "Fish" = 17)) +  # Circle & triangle
  # Zoom to Gulf of Bothnia
  coord_sf(xlim = xlim_bg, ylim = ylim_bg) +
  # Theme adjustments
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5)
  )



# New map
library(ggrepel)
# Create a dummy column to label the line in the legend
station_lines$LineType <- "Matching stations"

x11()
ggplot() +
  # Sea background
  geom_rect(aes(xmin = xlim_bg[1], xmax = 26, ymin = 59, ymax = ylim_bg[2]),
            fill = "lightblue", color = NA) +
  # Base map
  geom_sf(data = sweden_map, fill = "gray95", color = "gray50") +
  # Matching station lines (with legend only for line type)
  geom_sf(data = station_lines, aes(geometry = line, linetype = "Matching stations"),
          inherit.aes = FALSE, color = "red", size = 1.2, show.legend = c(linetype = TRUE, color = FALSE, shape = FALSE)) +
  # Station points
  geom_sf(data = matching_stations, 
          aes(color = Source, shape = Source), 
          size = 3.5, alpha = 0.9) +
  # Simple bold labels with clear nudging
  geom_sf_text(data = fish_points,
               aes(label = Fångstområde1),
               nudge_y = 0.15, size = 3.5, fontface = "bold", color = "navyblue", check_overlap = TRUE) +
  geom_sf_text(data = env_points,
               aes(label = Station),
               nudge_y = -0.15, size = 3.5, fontface = "bold", color = "darkgreen", check_overlap = TRUE) +
  # Titles and axis
  labs(title = "Map of station locations",
       # subtitle = "Matched fish & environmental stations",
       x = "Longitude", y = "Latitude",
       color = "Station Type",
       shape = "Station Type",
       linetype = "") +  # Clean title for line legend
  # Manual colors and shapes
  scale_color_manual(values = c("Environmental" = "darkgreen", "Fish" = "navyblue")) +
  scale_shape_manual(values = c("Environmental" = 16, "Fish" = 17)) +
  scale_linetype_manual(values = c("Matching stations" = "dashed")) +
  # View window
  coord_sf(xlim = xlim_bg, ylim = ylim_bg) +
  # Legend on x-axis within the plot with white box
  theme_minimal() +
  theme(
    axis.title = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.position = c(0.99, 0.01),  # Bottom right inside plot
    legend.direction = "vertical",
    legend.justification = c(1, 0),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "white", color = NA),
    legend.box.background = element_rect(color = "gray50", fill = "white", linewidth = 0.4),
    legend.spacing.y = unit(0.1, "cm")  # Reduces vertical spacing
  )


# 3 Make boxplot for temperature and salinity range ----
# Load data
setwd(here("Data"))
envdata <- read.csv("fish_env_data.csv")

north_to_south_order <- c("Råneå", "Kinnbäcksfjärden", "Holmön", "Norrbyn", "Gaviksfjärden", "Långvindsfjärden")
envdata$Fångstområde1 <- factor(envdata$Fångstområde1, levels = north_to_south_order)

# Boxplot for temperature per station
x11()
ggplot(envdata, aes(x = Fångstområde1, y = Mean_Temperature_env)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Temperature range per station",
       x = "Station",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 9),  
    axis.text.y = element_text(face = "bold", size = 9) 
  )

# Boxplot for salinity per station
x11()
ggplot(envdata, aes(x = Fångstområde1, y = Salinity_env)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Salinity range per station",
       x = "Station",
       y = "Salinity") +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text.x = element_text(face = "bold", size = 9),  
    axis.text.y = element_text(face = "bold", size = 9) 
  )




# 4 Make nice plots for biomass trends GAM ----
# First make model in original script!

#Perch
# Add fitted values and standard errors to the original data
fish_abborre <- subset(fish_mean, Artbestämning == "Abborre")
fish_abborre$fit <- predict(model_biomass_time_s, type = "response")
fish_abborre$se <- predict(model_biomass_time_s, type = "response", se.fit = TRUE)$se.fit

# Plot using ggplot2
x11()
ggplot(fish_abborre, aes(x = Year, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Perch biomass over time",
    x = "Year",
    y = "Biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Herring
# Add fitted values and standard errors to the original data
fish_stromming <- subset(fish_mean, Artbestämning == "Strömming")
fish_stromming$fit <- predict(model_biomass_time_s, type = "response")
fish_stromming$se <- predict(model_biomass_time_s, type = "response", se.fit = TRUE)$se.fit

# Plot using ggplot2
x11()
ggplot(fish_stromming, aes(x = Year, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Herring biomass over time",
    x = "Year",
    y = "Biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Ruffe
# Add fitted values and standard errors to the original data
fish_gars <- subset(fish_mean, Artbestämning == "Gärs")
fish_gars$fit <- predict(model_biomass_time_s, type = "response")
fish_gars$se <- predict(model_biomass_time_s, type = "response", se.fit = TRUE)$se.fit

# Plot using ggplot2
ggplot(fish_gars, aes(x = Year, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Ruffe biomass over time",
    x = "Year",
    y = "Mean biomass (per effort)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )

# 5 Make nice plots for CPU trends GAM ----
# First make model in original script!

#Perch
# Add fitted values and standard errors to the original data
fish_abborre <- subset(fish_mean, Artbestämning == "Abborre")
fish_abborre$fit <- predict(model_cpu_time_s, type = "response")
fish_abborre$se <- predict(model_cpu_time_s, type = "response", se.fit = TRUE)$se.fit
# Plot using ggplot2
ggplot(fish_abborre, aes(x = Year, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Perch CPUE over time",
    x = "Year",
    y = "Mean CPUE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Herring
# Add fitted values and standard errors to the original data
fish_stromming <- subset(fish_mean, Artbestämning == "Strömming")
fish_stromming$fit <- predict(model_cpu_time_s, type = "response")
fish_stromming$se <- predict(model_cpu_time_s, type = "response", se.fit = TRUE)$se.fit

# Plot using ggplot2
ggplot(fish_stromming, aes(x = Year, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Herring CPUE over time",
    x = "Year",
    y = "Mean CPUE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Ruffe
# Add fitted values and standard errors to the original data
fish_gars <- subset(fish_mean, Artbestämning == "Gärs")
fish_gars$fit <- predict(model_cpu_time_s, type = "response")
fish_gars$se <- predict(model_cpu_time_s, type = "response", se.fit = TRUE)$se.fit
# Plot using ggplot
ggplot(fish_gars, aes(x = Year, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Ruffe CPU over time",
    x = "Year",
    y = "Mean CPU"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )


# 6 Make nice plots for BM/CPU trends GAM ----
# First make model in original script!

#Perch
# Add fitted values and standard errors to the original data
fish_abborre <- subset(fish_mean, Artbestämning == "Abborre")
fish_abborre$fit <- predict(model_cpu_time_s, type = "response")
fish_abborre$se <- predict(model_cpu_time_s, type = "response", se.fit = TRUE)$se.fit
# Plot using ggplot
ggplot(fish_abborre, aes(x = Year, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Perch biomass per CPUE over time",
    x = "Year",
    y = "Biomass per CPUE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Herring
# Add fitted values and standard errors to the original data
fish_stromming <- subset(fish_mean, Artbestämning == "Strömming")
fish_stromming$fit <- predict(model_cpu_time_s, type = "response")
fish_stromming$se <- predict(model_cpu_time_s, type = "response", se.fit = TRUE)$se.fit
# Plot using ggplot
ggplot(fish_stromming, aes(x = Year, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Herring biomass per CPU over time",
    x = "Year",
    y = "Biomass per CPU"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Ruffe
# Add fitted values and standard errors to the original data
fish_gars <- subset(fish_mean, Artbestämning == "Gärs")
fish_gars$fit <- predict(model_cpu_time_s, type = "response")
fish_gars$se <- predict(model_cpu_time_s, type = "response", se.fit = TRUE)$se.fit
# Plot using ggplot
ggplot(fish_gars, aes(x = Year, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Ruffe biomass per CPU over time",
    x = "Year",
    y = "Biomass per CPU"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )


# 7 Make nice plots for salinity/temp GAM ----
# First make model in original script!
#Perch sal
# Step 1: Subset original data for observed points
data_perchsal <- subset(common_tsfish, Artbestämning == "Abborre")
# Step 2: Create a new data frame for smooth predictions
sal_grid <- data.frame(
  Salinity_env = seq(min(data_perchsal$Salinity_env, na.rm = TRUE),   #change parameter
            max(data_perchsal$Salinity_env, na.rm = TRUE),
            length.out = 200)
)
# Step 3: Predict on the grid
sal_grid$fit <- predict(tsmodel_biomass_time_s, newdata = sal_grid, type = "response")
sal_grid$se <- predict(tsmodel_biomass_time_s, newdata = sal_grid, type = "response", se.fit = TRUE)$se.fit
# Step 4: Plot
x11()
ggplot() +
  # geom_point(data = data_perchsal, aes(x = Salinity_env, y = Beräknad_vikt1), alpha = 0.3, color = "black") +    #change x= to parameter
  geom_ribbon(data = sal_grid, aes(x = Salinity_env, ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(data = sal_grid, aes(x = Salinity_env, y = fit), color = "blue", size = 1.2) +
  labs(
    title = "Effect of salinity on perch biomass",  # change title for parameter
    x = "Salinity",
    y = "Biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Perch temp
# Step 1: Subset original data for observed points
data_perchtemp <- subset(common_tsfish, Artbestämning == "Abborre")
# Step 2: Create a new data frame for smooth predictions
temp_grid <- data.frame(
  Mean_Temperature_env = seq(min(data_perchtemp$Mean_Temperature_env, na.rm = TRUE),   #change parameter
                     max(data_perchtemp$Mean_Temperature_env, na.rm = TRUE),
                     length.out = 200)
)
# Step 3: Predict on the grid
temp_grid$fit <- predict(tsmodel_biomass_time_s, newdata = temp_grid, type = "response")
temp_grid$se <- predict(tsmodel_biomass_time_s, newdata = temp_grid, type = "response", se.fit = TRUE)$se.fit
# Step 4: Plot
x11()
ggplot() +
  # geom_point(data = data_perchtemp, aes(x = Mean_Temperature_env, y = Beräknad_vikt1), alpha = 0.3, color = "black") +  #change x= to parameter
  geom_ribbon(data = temp_grid, aes(x = Mean_Temperature_env, ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(data = temp_grid, aes(x = Mean_Temperature_env, y = fit), color = "blue", size = 1.2) +
  labs(
    title = "Effect of temperature on perch biomass",  # change title for parameter
    x = "Temperature (°C)",
    y = "Biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Herring sal
# Step 1: Subset original data for observed points
data_herringsal <- subset(common_tsfish, Artbestämning == "Strömming")
# Step 2: Create a new data frame for smooth predictions
sal_grid <- data.frame(
  Salinity_env = seq(min(data_herringsal$Salinity_env, na.rm = TRUE),   #change parameter
                     max(data_herringsal$Salinity_env, na.rm = TRUE),
                     length.out = 200)
)
# Step 3: Predict on the grid
sal_grid$fit <- predict(tsmodel_biomass_time_s, newdata = sal_grid, type = "response")
sal_grid$se <- predict(tsmodel_biomass_time_s, newdata = sal_grid, type = "response", se.fit = TRUE)$se.fit
# Step 4: Plot
x11()
ggplot() +
   geom_point(data = data_herringsal, aes(x = Salinity_env, y = Beräknad_vikt1), alpha = 0.3, color = "black") +    #change x= to parameter
  geom_ribbon(data = sal_grid, aes(x = Salinity_env, ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(data = sal_grid, aes(x = Salinity_env, y = fit), color = "blue", size = 1.2) +
  labs(
    title = "Effect of salinity on herring biomass",  # change title for parameter
    x = "Salinity",
    y = "Biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Herring temp
# Step 1: Subset original data for observed points
data_herringtemp <- subset(common_tsfish, Artbestämning == "Strömming")
# Step 2: Create a new data frame for smooth predictions
temp_grid <- data.frame(
  Mean_Temperature_env = seq(min(data_herringtemp$Mean_Temperature_env, na.rm = TRUE),   #change parameter
                             max(data_herringtemp$Mean_Temperature_env, na.rm = TRUE),
                             length.out = 200)
)
# Step 3: Predict on the grid
temp_grid$fit <- predict(tsmodel_biomass_time_s, newdata = temp_grid, type = "response")
temp_grid$se <- predict(tsmodel_biomass_time_s, newdata = temp_grid, type = "response", se.fit = TRUE)$se.fit
# Step 4: Plot
x11()
ggplot() +
  # geom_point(data = data_herringtemp, aes(x = Mean_Temperature_env, y = Beräknad_vikt1), alpha = 0.3, color = "black") +#change x= to parameter
  geom_ribbon(data = temp_grid, aes(x = Mean_Temperature_env, ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(data = temp_grid, aes(x = Mean_Temperature_env, y = fit), color = "blue", size = 1.2) +
  labs(
    title = "Effect of temperature on herring biomass",  # change title for parameter
    x = "Temperature (°C)",
    y = "Biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.7),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )



#Perch sal
# Predict values and standard errors
data_filtered <- subset(common_tsfish, Artbestämning == "Abborre" & !is.na(Salinity_env))
data_filtered$fit <- predict(tsmodel_biomass_time_s, newdata = data_filtered, type = "response")
data_filtered$se <- predict(tsmodel_biomass_time_s, newdata = data_filtered, type = "response", se.fit = TRUE)$se.fit

# Plot using ggplot
x11()
ggplot(data_filtered, aes(x = Salinity_env, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Effect of salinity on perch biomass",
    x = "Salinity",
    y = "Biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

#Herring sal
# Predict values and standard errors
data_herring <- subset(common_tsfish, Artbestämning == "Strömming" & !is.na(Salinity_env))
data_herring$fit <- predict(tsmodel_biomass_time_s, newdata = data_herring, type = "response")
data_herring$se <- predict(tsmodel_biomass_time_s, newdata = data_herring, type = "response", se.fit = TRUE)$se.fit
# Plot using ggplot
ggplot(data_herring, aes(x = Salinity_env, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Effect of salinity on herring biomass",
    x = "Mean salinity (PSU)",
    y = "Mean biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Perch temp
# Predict values and standard errors
data_perch <- subset(common_tsfish, Artbestämning == "Abborre")
data_perch$fit <- predict(tsmodel_biomass_time_s, newdata = data_perch, type = "response")
data_perch$se <- predict(tsmodel_biomass_time_s, newdata = data_perch, type = "response", se.fit = TRUE)$se.fit
# Plot using ggplot
x11()
ggplot(data_perch, aes(x = Mean_Temperature_env, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Effect of temperature on perch biomass",
    x = "Mean temperature (°C)",
    y = "Mean biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

# herring temp
# Predict values and standard errors
data_herring2 <- subset(common_tsfish, Artbestämning == "Abborre")
data_herring2$fit <- predict(tsmodel_biomass_time_s, newdata = data_herring2, type = "response")
data_herring2$se <- predict(tsmodel_biomass_time_s, newdata = data_herring2, type = "response", se.fit = TRUE)$se.fit
# Plot using ggplot
x11()
ggplot(data_herring2, aes(x = Mean_Temperature_env, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Effect of temperature on herring biomass",
    x = "Mean temperature (°C)",
    y = "Mean biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

# 8 Make nice plots for env GAM ----
# First make model in original script!

# Perch chla
# Predict values and standard errors
data_perch <- subset(common_chla_fish, Artbestämning == "Abborre")
data_perch$fit <- predict(chla_model_biomass_time_s, newdata = data_perch, type = "response")
data_perch$se <- predict(chla_model_biomass_time_s, newdata = data_perch, type = "response", se.fit = TRUE)$se.fit
# Plot using ggplot
x11()
ggplot(data_perch, aes(x = Mean_chla, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Effect of chlorophyll-a on perch biomass",
    x = "Mean chlorophyll-a (µg/L)",
    y = "Mean biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

# herring chla
# Predict values and standard errors
data_herring2 <- subset(common_chla_fish, Artbestämning == "Strömming")
data_herring2$fit <- predict(chla_model_biomass_time_s, newdata = data_herring2, type = "response")
data_herring2$se <- predict(chla_model_biomass_time_s, newdata = data_herring2, type = "response", se.fit = TRUE)$se.fit
# Plot using ggplot
x11()
ggplot(data_herring2, aes(x = Mean_chla, y = fit)) +
  geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Effect of chlorophyll-a on herring biomass",
    x = "Mean chlorophyll-a (µg/L)",
    y = "Mean biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Step 1: Subset original data for observed points
data_herring2 <- subset(common_chla_fish, Artbestämning == "Strömming")
# Step 2: Create a new data frame for smooth predictions
chla_grid <- data.frame(
  Mean_chla = seq(min(data_herring2$Mean_chla, na.rm = TRUE),
                  max(data_herring2$Mean_chla, na.rm = TRUE),
                  length.out = 200)
)
# Step 3: Predict on the grid
chla_grid$fit <- predict(chla_model_biomass_time_s, newdata = chla_grid, type = "response")
chla_grid$se <- predict(chla_model_biomass_time_s, newdata = chla_grid, type = "response", se.fit = TRUE)$se.fit
# Step 4: Plot
x11()
ggplot() +
  # geom_point(data = data_herring2, aes(x = Mean_chla, y = Beräknad_vikt1), alpha = 0.3, color = "black") +
  geom_ribbon(data = chla_grid, aes(x = Mean_chla, ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(data = chla_grid, aes(x = Mean_chla, y = fit), color = "blue", size = 1.2) +
  labs(
    title = "Effect of chlorophyll-a on herring biomass",
    x = "Mean chlorophyll-a (µg/L)",
    y = "Mean biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

# 9 Make nice plots for DBO GAM ----
# First make model in original script!

# Perch
# Step 1: Subset original data for observed points
data_perchdbo <- subset(common_dbo_fish, Artbestämning == "Abborre")
# Step 2: Create a new data frame for smooth predictions
dbo_grid <- data.frame(
  PO4 = seq(min(data_perchdbo$PO4, na.rm = TRUE),   #change parameter
                  max(data_perchdbo$PO4, na.rm = TRUE),
                  length.out = 200)
)
# Step 3: Predict on the grid
dbo_grid$fit <- predict(dbo_model_biomass_s, newdata = dbo_grid, type = "response")
dbo_grid$se <- predict(dbo_model_biomass_s, newdata = dbo_grid, type = "response", se.fit = TRUE)$se.fit
# Step 4: Plot
x11()
ggplot() +
   geom_point(data = data_perchdbo, aes(x = PO4, y = Beräknad_vikt1), alpha = 0.3, color = "black") +    #change x= to parameter
  geom_ribbon(data = dbo_grid, aes(x = PO4, ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(data = dbo_grid, aes(x = PO4, y = fit), color = "blue", size = 1.2) +
  labs(
    title = "Effect of PO4 on perch biomass",  # change title for parameter
    x = "PO4 (μmol/L)",
    y = "Biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

xxxxxxxxxxxx
# Group the data into bins and calculate mean
mean_line <- data_perchdbo %>%
  group_by(pH_bin = cut(pH, breaks = 20)) %>%
  summarise(
    pH_mid = mean(pH, na.rm = TRUE),
    mean_biomass = mean(Beräknad_vikt1, na.rm = TRUE)
  )
# Plot
ggplot(data_perchdbo, aes(x = pH, y = Beräknad_vikt1)) +
  geom_point(alpha = 0.3) +
  geom_line(data = mean_line, aes(x = pH_mid, y = mean_biomass), color = "darkgreen", size = 1.2) +
  labs(
    title = "Mean perch biomass across pH bins",
    x = "pH",
    y = "Biomass (kg)"
  ) +
  theme_minimal(base_size = 18)
# Plot with smoothed line through binned means
ggplot(data_perchdbo, aes(x = pH, y = Beräknad_vikt1)) +
  geom_point(alpha = 0.3) +
  geom_smooth(data = mean_line, aes(x = pH_mid, y = mean_biomass),
              method = "loess", color = "darkgreen", se = FALSE, size = 1.2) +
  labs(
    title = "Smoothed mean trend of perch biomass across pH",
    x = "pH",
    y = "Biomass (kg)"
  ) +
  theme_minimal(base_size = 18)
xxxxxxxxxxx


# Herring
# Step 1: Subset original data for observed points
data_herringdbo <- subset(common_dbo_fish, Artbestämning == "Strömming")
# Step 2: Create a new data frame for smooth predictions
dbo_grid2 <- data.frame(
  PO4 = seq(min(data_herringdbo$PO4, na.rm = TRUE),   #change parameter
           max(data_herringdbo$PO4, na.rm = TRUE),
           length.out = 200)
)
# Step 3: Predict on the grid
dbo_grid2$fit <- predict(dbo_model_biomass_s, newdata = dbo_grid2, type = "response")
dbo_grid2$se <- predict(dbo_model_biomass_s, newdata = dbo_grid2, type = "response", se.fit = TRUE)$se.fit
# Step 4: Plot
x11()
ggplot() +
  # geom_point(data = data_herringdbo, aes(x = PO4, y = Beräknad_vikt1), alpha = 0.3, color = "black") +    #change x= to parameter
  geom_ribbon(data = dbo_grid2, aes(x = PO4, ymin = fit - 2 * se, ymax = fit + 2 * se), fill = "lightblue", alpha = 0.4) +
  geom_line(data = dbo_grid2, aes(x = PO4, y = fit), color = "blue", size = 1.2) +
  labs(
    title = "Effect of PO4 on herring biomass",  # change title for parameter
    x = "PO4 (μmol/L)",
    y = "Biomass per effort (kg)"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank()
  )

# 10 Make barplot of species per station ----
library(dplyr)
library(ggplot2)

# Example: filter to top 5 species first
top5_species <- fishdata %>%
  group_by(Artbestämning) %>%
  summarise(total_fish = sum(Antal1, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_fish)) %>%
  slice_head(n = 5) %>%
  pull(Artbestämning)


# Calculate relative abundance per station
station_species_perc <- fishdata %>%
  filter(Artbestämning %in% top5_species) %>%
  group_by(Fångstområde1, Artbestämning) %>%
  summarise(total_abundance = sum(Antal1, na.rm = TRUE), .groups = "drop") %>%
  group_by(Fångstområde1) %>%
  mutate(perc = total_abundance / sum(total_abundance)) %>%
  ungroup()

north_to_south_order <- c("Råneå", "Kinnbäcksfjärden", "Holmön", "Norrbyn", "Gaviksfjärden", "Långvindsfjärden")
station_species_perc$Fångstområde1 <- factor(station_species_perc$Fångstområde1, levels = north_to_south_order)

station_species_perc$Artbestämning <- recode(
  station_species_perc$Artbestämning,
  "Abborre" = "Perch",
  "Gärs" = "Ruffe",
  "Mört" = "Roach",
  "Storspigg" = "Stickleback",
  "Strömming" = "Herring"
)

# Plot stacked barplot (percentages)
x11()
ggplot(station_species_perc, aes(x = Fångstområde1, y = perc, fill = Artbestämning)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Relative abundance of top 5 species per station",
    x = "Station",
    y = "Relative abundance (%)",
    fill = "Species"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
