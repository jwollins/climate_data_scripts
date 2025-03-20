## Shawbury weather data 




#______________________________________####
# packages ####

source(file = "weather_station/01_packages.R")

library(dplyr)
library(lubridate)






#______________________________________####
# data ####


experiment_dat <- read.delim("~/OneDrive - Harper Adams University/Data/climate_data/data/shawburydata_long_term.txt",
                         header = FALSE, 
                         skip = 7, 
                         sep = "",  # Auto-detect white spaces
                         na.strings = "---",
                         col.names = c("Year", "Month", "Tmax", "Tmin", "AirFrost", "Rain", "Sun"),
                         strip.white = TRUE)

experiment_dat <- filter(.data = experiment_dat, 
                         experiment_dat$Year == 2022 |
                           experiment_dat$Year == 2023 |
                           experiment_dat$Year == 2024)

glimpse(experiment_dat$Sun)

experiment_dat$Sun <- as.numeric(gsub("[#*]", "", experiment_dat$Sun))






## ~~ Historic data ####

dat_trends <- read.delim("~/OneDrive - Harper Adams University/Data/climate_data/data/shawburydata_long_term.txt",
                           header = FALSE, 
                           skip = 7, 
                           sep = "",  # Auto-detect white spaces
                           na.strings = "---",
                           col.names = c("Year", "Month", "Tmax", "Tmin", "AirFrost", "Rain", "Sun"),
                           strip.white = TRUE)

# Convert columns to numeric
dat_trends <- transform(dat_trends,
                          Tmax = as.numeric(Tmax),
                          Tmin = as.numeric(Tmin),
                          AirFrost = as.numeric(AirFrost),
                          Rain = as.numeric(Rain),
                          Sun = as.numeric(Sun))

head(dat_trends)










#______________________________________####
# calculations ####

glimpse(monthly_experiment)
glimpse(experiment_dat)
glimpse(dat_trends)


# Compute mean for each month across all years
dat_trends <- dat_trends %>%
  group_by(Month) %>%
  summarise(
    Tmax = mean(Tmax, na.rm = TRUE),
    Tmin = mean(Tmin, na.rm = TRUE),
    AirFrost = mean(AirFrost, na.rm = TRUE),
    Rain = mean(Rain, na.rm = TRUE),
    Sun = mean(Sun, na.rm = TRUE)
  )

# View the results
print(dat_trends)

dat_trends <- dat_trends %>%
  filter(!is.na(Month))

# Convert Year to character in monthly_experiment
experiment_dat <- experiment_dat %>%
  mutate(Year = as.character(Year), Source = "Experiment")

# Add a "Source" column and ensure Year is also character in dat_trends
dat_trends <- dat_trends %>%
  mutate(Year = "Historical", Source = "Historic Mean")  # Label for historic data

# Convert the Sun column in dat_trends to numeric
dat_trends$Sun <- as.numeric(dat_trends$Sun)

# Bind both datasets together
combined_data <- bind_rows(experiment_dat, dat_trends)

# Check the combined data
glimpse(combined_data)


combined_data$Tavg <- (combined_data$Tmax + combined_data$Tmin) / 2








#______________________________________####
# plots ####



## ~~ rain ####

a <-
ggplot(data = combined_data, 
       aes(x = factor(Month), y = Rain)) +
  
  # Bar plot for experimental data (different years)
  geom_bar(data = filter(combined_data, Source == "Experiment"), 
           stat = "identity", position = "dodge", alpha = 0.8, aes(fill = Year)) +
  
  # Line plot for historic mean data (dashed) with legend
  geom_line(data = filter(combined_data, Source == "Historic Mean"), 
            aes(color = "Historic Mean", group = 1), linetype = "solid", linewidth = 1.2) +
  
  # Manually set legend colors
  scale_color_manual(name = element_blank(), values = "black") +
  
  # Labels and theme
  labs(
    # title = "Monthly Rainfall Over 3 Years vs. Historic Mean",
      subtitle = "Precipitation (mm)",
       x = "Month",
       y = "Precipitation (mm)",
       fill = "Year") +
  
  scale_x_discrete(labels = month.abb) +  # Format month labels (Jan, Feb, ...)
  theme_bw() +
  theme(legend.position = "bottom")


a

ggsave(filename = "symlink_climate_data/plots/fig_rain_historic_mean.png", height = 3.5, width = 10)




## ~~ tmax ####

b <-
ggplot(data = combined_data, 
       aes(x = factor(Month), y = Tavg)) +
  
  # Bar plot for experimental data (different years)
  geom_bar(data = filter(combined_data, Source == "Experiment"), 
           stat = "identity", position = "dodge", alpha = 0.8, aes(fill = Year)) +
  
  # Line plot for historic mean data (dashed) with legend
  geom_line(data = filter(combined_data, Source == "Historic Mean"), 
            aes(color = "Historic Mean", group = 1), linetype = "solid", linewidth = 1.2) +
  
  # Manually set legend colors
  scale_color_manual(name = element_blank(), values = "black") +
  
  # Labels and theme
  labs(
    # title = "Monthly Rainfall Over 3 Years vs. Historic Mean",
    subtitle = "Mean Temperature (°C)",
    x = "Month",
    y = "Temperature (°C)",
    fill = "Year") +
  
  scale_x_discrete(labels = month.abb) +  # Format month labels (Jan, Feb, ...)
  theme_bw() +
  theme(legend.position = "bottom")

b

ggsave(filename = "symlink_climate_data/plots/fig_temp_historic_mean.png", 
       height = 3.5, 
       width = 10)







## ~~ sun ####

c <-
  ggplot(data = combined_data, 
         aes(x = factor(Month), y = Sun)) +
  
  # Bar plot for experimental data (different years)
  geom_bar(data = filter(combined_data, Source == "Experiment"), 
           stat = "identity", position = "dodge", alpha = 0.8, aes(fill = Year)) +
  
  # Line plot for historic mean data (dashed) with legend
  geom_line(data = filter(combined_data, Source == "Historic Mean"), 
            aes(color = "Historic Mean", group = 1), linetype = "solid", linewidth = 1.2) +
  
  # Manually set legend colors
  scale_color_manual(name = element_blank(), values = "black") +
  
  # Labels and theme
  labs(
    # title = "Monthly Rainfall Over 3 Years vs. Historic Mean",
    subtitle = "Hours of Sun (hrs)",
    x = "Month",
    y = "Hours of Sun (hrs)",
    fill = "Year") +
  
  scale_x_discrete(labels = month.abb) +  # Format month labels (Jan, Feb, ...)
  theme_bw() +
  theme(legend.position = "bottom")

c

ggsave(filename = "symlink_climate_data/plots/fig_sun_historic_mean.png", 
       height = 3.5, 
       width = 10)




# ~ joint plot ####

ggarrange(a,b,c, 
          ncol = 1, 
          nrow = 3, 
          labels = c("A","B","C"), 
          common.legend = TRUE, 
          legend = "bottom")


ggsave(filename = "symlink_climate_data/plots/fig_joint_climate_data_plots.png", 
       height = 7, 
       width = 9)












