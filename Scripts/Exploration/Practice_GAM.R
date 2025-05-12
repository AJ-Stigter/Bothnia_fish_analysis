# 0 Preparation ----

# Load necessary packages
library("here")
# install.packages("mgcv")
library("dplyr")
library("mgcv")
library("ggplot2")

# Load data
setwd(here("Data"))
fishdata <- read.csv("fish_cleaned_weight.csv")
#

# Clean data
common_fish <- fishdata %>%
  filter(Artbestämning %in% c("Abborre", "Gärs", "Mört", "Storspigg", "Strömming"))


# First group by year and calculate mean
library(lubridate)
# common_fish$Year <- year(common_fish$Date)
common_fish$Year <- year(common_fish$Fiskedatum1)

fish_mean <- common_fish %>%
  group_by(Fångstområde1, Artbestämning, Year) %>%   # Group by station and year
  summarise(
    mean_biomass = mean(Beräknad_vikt1, na.rm = TRUE),
    mean_cpu = mean(Antal_per_anstr1, na.rm = TRUE),
    #mean_temperature = mean(Mean_Temperature_ºC, na.rm = TRUE),
    #mean_salinity = mean(Mean_Salinity_psu, na.rm = TRUE)
  )

# Check data distribution
years_per_species_station <- fish_mean %>%
  distinct(Artbestämning, Fångstområde1, Year) %>%  # Just get unique year entries
  group_by(Artbestämning, Fångstområde1) %>%
  summarise(Years_of_Data = n(), .groups = "drop")
#Plot
x11()
ggplot(years_per_species_station, aes(x = Fångstområde1, y = Artbestämning, fill = Years_of_Data)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Years of Data per Species per Station",
    x = "Station",
    y = "Species",
    fill = "Years"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Check year distribution
years_data <- fish_mean %>%
  distinct(Artbestämning, Fångstområde1, Year)
#Plot
x11()
ggplot(years_data, aes(x = Year, y = Fångstområde1)) +
  geom_point(size = 2, color = "darkblue") +
  facet_wrap(~ Artbestämning, ncol = 1, scales = "free_y") +
  labs(
    title = "Years with Data per Species per Station2",
    x = "Year",
    y = "Station"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))



# 1 Make first models -----
library("gratia")

#Change Fångstområde from character to factor
fish_mean$Fångstområde1 <- as.factor(fish_mean$Fångstområde1)

# 1.1 Model total Perch biomass over time ----
# GS smoother with same wiggliness
model_biomass_time_s <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs') 
                          # + s(mean_temperature, k=9,bs='tp') 
                          # + s(mean_salinity, k=9, bs='tp')
                          , data = subset(fish_mean, Artbestämning=="Abborre")
                          , family = gaussian(),method = "REML")
summary(model_biomass_time_s)
x11()
draw(model_biomass_time_s)
# Global term plus group specific smoother with same wiggliness
model_biomass_time_gs <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs')
                          + s(Year, k=9, bs='tp')
                          # + s(mean_temperature, k=9,bs='tp') 
                          # + s(mean_salinity, k=9, bs='tp')
                          , data = subset(fish_mean, Artbestämning=="Abborre")
                          , family = gaussian(),method = "REML")
summary(model_biomass_time_gs)
x11()
draw(model_biomass_time_gs)
# Group specific smoother of different wiggliness
model_biomass_time_i <- gam(mean_biomass ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                          # + s(mean_temperature, k=9,bs='tp') 
                          # + s(mean_salinity, k=9, bs='tp')
                          , data = subset(fish_mean, Artbestämning=="Abborre")
                          , family = gaussian(),method = "REML")
summary(model_biomass_time_i)
x11()
draw(model_biomass_time_i)
AIC(model_biomass_time_gs, model_biomass_time_i, model_biomass_time_s)

# 1.2 Model total three-spined stickleback biomass over time ----
# GS smoother with same wiggliness
model_biomass_time_s <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs') 
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Storspigg")
                            , family = gaussian(),method = "REML")
summary(model_biomass_time_s)
x11()
draw(model_biomass_time_s)
# Global term plus group specific smoother with same wiggliness
model_biomass_time_gs <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(fish_mean, Artbestämning=="Storspigg")
                             , family = gaussian(),method = "REML")
summary(model_biomass_time_gs)
x11()
draw(model_biomass_time_gs)
# Group specific smoother of different wiggliness
model_biomass_time_i <- gam(mean_biomass ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Storspigg")
                            , family = gaussian(),method = "REML")
summary(model_biomass_time_i)
x11()
draw(model_biomass_time_i)
AIC(model_biomass_time_gs, model_biomass_time_i, model_biomass_time_s)

# 1.3 Model total common roach biomass over time ----
# GS smoother with same wiggliness
model_biomass_time_s <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs') 
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Mört")
                            , family = gaussian(),method = "REML")
summary(model_biomass_time_s)
x11()
draw(model_biomass_time_s)
# Global term plus group specific smoother with same wiggliness
model_biomass_time_gs <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(fish_mean, Artbestämning=="Mört")
                             , family = gaussian(),method = "REML")
summary(model_biomass_time_gs)
x11()
draw(model_biomass_time_gs)
# Group specific smoother of different wiggliness
model_biomass_time_i <- gam(mean_biomass ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Mört")
                            , family = gaussian(),method = "REML")
summary(model_biomass_time_i)
x11()
draw(model_biomass_time_i)
AIC(model_biomass_time_gs, model_biomass_time_i, model_biomass_time_s)

# 1.4 Model total ruffe biomass over time ----
# GS smoother with same wiggliness
model_biomass_time_s <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs') 
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Gärs")
                            , family = gaussian(),method = "REML")
summary(model_biomass_time_s)
x11()
draw(model_biomass_time_s)
# Global term plus group specific smoother with same wiggliness
model_biomass_time_gs <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(fish_mean, Artbestämning=="Gärs")
                             , family = gaussian(),method = "REML")
summary(model_biomass_time_gs)
x11()
draw(model_biomass_time_gs)
# Group specific smoother of different wiggliness
model_biomass_time_i <- gam(mean_biomass ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Gärs")
                            , family = gaussian(),method = "REML")
summary(model_biomass_time_i)
x11()
draw(model_biomass_time_i)
AIC(model_biomass_time_gs, model_biomass_time_i, model_biomass_time_s)

# 1.5 Model total herring biomass over time ----
# GS smoother with same wiggliness
model_biomass_time_s <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs') 
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Strömming")
                            , family = gaussian(),method = "REML")
summary(model_biomass_time_s)
x11()
draw(model_biomass_time_s)
# Global term plus group specific smoother with same wiggliness
model_biomass_time_gs <- gam(mean_biomass ~ s(Year,Fångstområde1, k=9, bs='fs')
                             + s(Year, k=9, bs='tp')
                             # + s(mean_temperature, k=9,bs='tp') 
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(fish_mean, Artbestämning=="Strömming")
                             , family = gaussian(),method = "REML")
summary(model_biomass_time_gs)
x11()
draw(model_biomass_time_gs)
# Group specific smoother of different wiggliness
model_biomass_time_i <- gam(mean_biomass ~ s(Year, by=Fångstområde1, k=9, bs='fs')
                            # + s(mean_temperature, k=9,bs='tp') 
                            # + s(mean_salinity, k=9, bs='tp')
                            , data = subset(fish_mean, Artbestämning=="Strömming")
                            , family = gaussian(),method = "REML")
summary(model_biomass_time_i)
x11()
draw(model_biomass_time_i)
AIC(model_biomass_time_gs, model_biomass_time_i, model_biomass_time_s)



# 2 Analyze results ----
# Per station
sm <- gratia::smooth_estimates(model_biomass_time_i)
sm <- sm %>%
  add_constant(coef(model_biomass_time_i)["(Intercept)"]) %>% 
  add_confint(coverage=0.95) %>%
  transform_fun(inv_link(model_biomass_time_i))

GAM_timeseries <- ggplot(aes(x=Year,y=.estimate,group = Fångstområde1,colour = Fångstområde1),data=sm)+
  geom_line()+
  geom_ribbon(aes(ymin = .lower_ci,
                  ymax = .upper_ci,
                  fill = Fångstområde1), alpha = 0.2)+
  facet_wrap(~Fångstområde1) +
  geom_point(data = subset(fish_mean, Artbestämning == "Strömming"),     # Change Artbestämning to specific species used in model
             aes(x = Year, y = mean_biomass, colour = Fångstområde1),
             inherit.aes = FALSE)
  # + scale_y_continuous(limits = c(0, 4))  # <- Set min and max of y-axis if needed

x11()
GAM_timeseries

# Global trend











x11()
plot(fish_mean$mean_biomass ~fish_mean$Year)
x11()
gam.check(model_biomass_time)
x11()
plot(model_biomass_time, select = 1)



























# Model total CPU over time
model_perchcount_time <- gam(mean_cpu ~ s(Year, k=9, bs='tp') 
                             # + s(mean_temperature, k=9,bs='tp')  
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = subset(fish_mean, Artbestämning=="Abborre")
                             , family = gaussian(),method = "REML")
x11()
plot(aborre_total_mean$mean_count ~aborre_total_mean$Year)
x11()
gam.check(model_perchcount_time)
x11()
plot(model_perchcount_time, select = 1)
summary(model_perchcount_time)


# Model total count over time
model_perchcount_gav <- gam(mean_count ~ s(Year, k=9, bs='tp') 
                             # + s(mean_temperature, k=9,bs='tp')  
                             # + s(mean_salinity, k=9, bs='tp')
                             , data = station_gav, family = gaussian(),method = "REML")
x11()
plot(station_gav$mean_count ~station_gav$Year)
x11()
gam.check(model_perchcount_gav)
x11()
plot(model_perchcount_gav, select = 1)
summary(model_perchcount_gav)





model2_perchweight_time <- gam(mean_biomass ~ s(Year, k=9, bs='tp') 
                              # + s(mean_temperature, k=9,bs='tp')  
                              # + s(mean_salinity, k=9, bs='tp')
                              ,
                              data = aborre_mean, family = gaussian(),method = "REML")
x11()
plot(model2_perchweight_time, select = 1)




# 3 Make new models with env data ----
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


