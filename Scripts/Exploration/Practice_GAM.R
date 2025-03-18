# 0 Preparation ----

# Load necessary packages
library("here")
# install.packages("mgcv")
library("dplyr")
library("mgcv")

# Load data
setwd(here("Data"))
fishdata <- read.csv("test_fish_env_merge.csv")

# Clean data
fishdata_abborre <- fishdata %>%
  filter(Artbestämning == "Abborre")

# 1 Make first models ----
model_perchcount <- gam(Antal1 ~ s(Mean_Temperature_ºC, k=9,bs='tp') + 
                     s(Mean_Salinity_psu, k=9, bs='tp'),
                   data = fishdata_abborre, family = poisson(),method = "REML")


model_perchweight <- gam(Beräknad_vikt1+0.1 ~ s(Mean_Temperature_ºC, k=9,bs='tp') + 
                     s(Mean_Salinity_psu, k=9, bs='tp'),
                   data = fishdata_abborre, family = gaussian(),method = "REML")

gam.check(model_perchcount)
gam.check(model_perchweight)
Gamma()
summary(model_perch)
# 

x11()
plot(fishdata_abborre$Antal1 ~fishdata_abborre$Mean_Salinity_psu)



# 2 Make a 3D plot ------
# Load the necessary library
# install.packages("plotly")
library(plotly)

# Create a grid of salinity and temperature values
salinity_range <- seq(min(fishdata_abborre$Mean_Salinity_psu), max(fishdata_abborre$Mean_Salinity_psu), length.out = 30)
temp_range <- seq(min(fishdata_abborre$Mean_Temperature_ºC), max(fishdata_abborre$Mean_Temperature_ºC), length.out = 30)

# Create a data frame for predictions
pred_grid <- expand.grid(Mean_Salinity_psu = salinity_range, Mean_Temperature_ºC = temp_range)

# Predict perch weight for every combination of salinity and temperature
pred_grid$Predicted_Weight <- predict(model_perch, newdata = pred_grid, type = "response")

# Make a 3D surface plot
plot_ly(x = ~pred_grid$Mean_Salinity_psu, 
        y = ~pred_grid$Mean_Temperature_ºC, 
        z = ~pred_grid$Predicted_Weight, 
        type = "scatter3d", 
        mode = "markers", 
        marker = list(size = 3, color = ~pred_grid$Predicted_Weight, colorscale = "Viridis")) %>%
  layout(scene = list(xaxis = list(title = "Mean Salinity (psu)"),
                      yaxis = list(title = "Mean Temperature (°C)"),
                      zaxis = list(title = "Predicted Perch Weight (g)")))

# 3 Make new models -----

# First group by year and calculate mean
library(lubridate)
fishdata_abborre$Year <- year(fishdata_abborre$Date)

aborre_mean <- fishdata_abborre %>%
  group_by(Year) %>%
  summarise(
    mean_biomass = mean(Beräknad_vikt1, na.rm = TRUE),
    mean_count = mean(Antal1, na.rm = TRUE),
    mean_temperature = mean(Mean_Temperature_ºC, na.rm = TRUE),
    mean_salinity = mean(Mean_Salinity_psu, na.rm = TRUE)
  )

# Model biomass over time
model_perchweight_time <- gam(mean_biomass ~ s(Year, k=9, bs='tp') + s(mean_temperature, k=9,bs='tp') + 
                           s(mean_salinity, k=9, bs='tp'),
                         data = aborre_mean, family = gaussian(),method = "REML")

x11()
plot(aborre_mean$mean_biomass ~aborre_mean$Year)

gam.check(model_perchweight_time)
x11()
plot(model_perchweight_time, select = 1)
summary(model_perchweight_time)

# Model count over time
model_perchcount_time <- gam(mean_count ~ s(Year, k=9, bs='tp') + s(mean_temperature, k=9,bs='tp') + 
                                s(mean_salinity, k=9, bs='tp'),
                              data = aborre_mean, family = gaussian(),method = "REML")
x11()
plot(aborre_mean$mean_count ~aborre_mean$Year)

gam.check(model_perchcount_time)
x11()
plot(model_perchcount_time, select = 1)
summary(model_perchcount_time)


model2_perchweight_time <- gam(mean_biomass ~ s(Year, k=9, bs='tp') 
                              # + s(mean_temperature, k=9,bs='tp')  
                              # + s(mean_salinity, k=9, bs='tp')
                              ,
                              data = aborre_mean, family = gaussian(),method = "REML")
x11()
plot(model2_perchweight_time, select = 1)


