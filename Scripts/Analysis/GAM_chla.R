# 0 Preparation ----

# Load necessary packages
library("here")
library("dplyr")
library("mgcv")
library("ggplot2")

# Load data
setwd(here("Data"))
fish_chla_data <- read.csv("fish_chla_data.csv")
#

# Clean data
common_chla_fish <- fish_chla_data %>%
  filter(Artbestämning %in% c("Abborre", "Gärs", "Mört", "Storspigg", "Strömming"))


# Change Fångstområde from character to factor
common_chla_fish$Fångstområde1 <- as.factor(common_chla_fish$Fångstområde1)


# 1 Model biomass -----
library("gratia")

# 1.1 Model chla / Perch biomass ----
# GS smoother with same wiggliness
chla_model_biomass_time_s <- gam(Beräknad_vikt1 ~  s(Mean_chla, k=9, bs='tp') 
                          , data = subset(common_chla_fish, Artbestämning=="Abborre") 
                          , family = gaussian(),method = "REML")
summary(chla_model_biomass_time_s)
x11()
draw(chla_model_biomass_time_s)

# Check relation
data <- subset(common_chla_fish, Artbestämning=="Abborre" & Fångstområde1 == "Holmön")
data_sum <- data %>% group_by(Mean_chla) %>% summarize(Mean_fish = mean(Beräknad_vikt1,na.rm=T))
plot(data$Mean_chla,data$Beräknad_vikt1)
plot(data_sum$Mean_chla,data_sum$Mean_fish)


#check data distribution
# Prepare data
data_chla <- subset(common_chla_fish, Artbestämning == "Strömming")
# Predict fitted values and standard errors
data_chla$fit <- predict(chla_model_biomass_time_s, newdata = data_chla, type = "response")
data_chla$se <- predict(chla_model_biomass_time_s, newdata = data_chla, type = "response", se.fit = TRUE)$se.fit

data_chla$fit_trunc <- pmax(data_chla$fit, 0)
data_chla$ymin <- pmax(data_chla$fit - 2 * data_chla$se, 0)
data_chla$ymax <- pmax(data_chla$fit + 2 * data_chla$se, 0)
# Plot
x11()
ggplot(data_chla, aes(x = Mean_chla)) +
  geom_point(aes(y = Beräknad_vikt1), color = "black", alpha = 0.3, size = 1.5) +   # Observed points
  geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.4) +    # 95% CI
  geom_line(aes(y = fit_trunc), color = "blue", size = 1.2) +                       # Fitted line
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



# 1.2 Model chla / Herring biomass ----
# GS smoother with same wiggliness
chla_model_biomass_time_s <- gam(Beräknad_vikt1 ~  s(Mean_chla, k=9,bs='tp') 
                                 , data = subset(common_chla_fish, Artbestämning=="Strömming")  
                                 , family = gaussian(),method = "REML")
summary(chla_model_biomass_time_s)
x11()
draw(chla_model_biomass_time_s)




# 1.3 Analyze results ----
# Per station
sm <- gratia::smooth_estimates(tsmodel_biomass_time_s)               # Change model if wanted
sm <- sm %>%
  add_constant(coef(tsmodel_biomass_time_s)["(Intercept)"]) %>%      # Change model if wanted
  add_confint(coverage=0.95) %>%
  transform_fun(inv_link(tsmodel_biomass_time_s))                    # Change model if wanted

GAM_timeseries <- ggplot(aes(x=mean_salinity,y=.estimate,group = Fångstområde1,colour = Fångstområde1),data=sm)+
  geom_line()+
  geom_ribbon(aes(ymin = .lower_ci,
                  ymax = .upper_ci,
                  fill = Fångstområde1), alpha = 0.2)+
  facet_wrap(~Fångstområde1) +
  geom_point(data = subset(common_chla_fish, Artbestämning == "Strömming"),  # Change Artbestämning to specific species used in model
             aes(x = Mean_chla, y = mean_biomass, colour = Fångstområde1),
             inherit.aes = FALSE) +
  labs(title = "Influence of Chla on herring biomass across stations",              # Change species name so title is correct
       x = "Year", y = "biomass")
  # + scale_y_continuous(limits = c(0, 100))  # <- Set min and max of y-axis if needed

x11()
GAM_timeseries
