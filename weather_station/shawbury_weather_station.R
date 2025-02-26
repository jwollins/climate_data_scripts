## Shawbury weather data 




#______________________________________####
# packages ####

source(file = "weather_station/01_packages.R")

library(dplyr)
library(lubridate)

#______________________________________####
# data ####


## ~~ Experiment data ####

dat <- read_excel(path = "~/OneDrive - Harper Adams University/Data/climate_data/data/shawbury_2021_01_01_to_2024_12_31.xlsx")

glimpse(dat)

dat$date <- as.Date(dat$date, format = "%Y-%m-%d")

dat$date <- as.POSIXct(dat$date)








# Extract year and month from the date column
monthly_experiment <- dat %>%
  mutate(Year = year(date), Month = month(date)) %>%  # Extract Year and Month
  group_by(Year, Month) %>%  # Group by both Year and Month
  summarise(
    Tmax_mean = sum(tmax, na.rm = TRUE),
    Tmin_mean = sum(tmin, na.rm = TRUE),
    Rain_mean = sum(prcp, na.rm = TRUE),
    Sun_mean = sum(tsun, na.rm = TRUE)  # Assuming tsun is in hours
  ) %>%
  ungroup()  # Ungroup after summarizing

# View summarized data
print(monthly_experiment)





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



# Compute mean for each month across all years
dat_trends <- dat_trends %>%
  group_by(Month) %>%
  summarise(
    Tmax_mean = mean(Tmax, na.rm = TRUE),
    Tmin_mean = mean(Tmin, na.rm = TRUE),
    AirFrost_mean = mean(AirFrost, na.rm = TRUE),
    Rain_mean = mean(Rain, na.rm = TRUE),
    Sun_mean = mean(Sun, na.rm = TRUE)
  )

# View the results
print(dat_trends)

dat_trends <- dat_trends %>%
  filter(!is.na(Month))







#______________________________________####
# plots ####

glimpse(monthly_experiment)
glimpse(dat_trends)





library(dplyr)

# Convert Year to character in monthly_experiment
monthly_experiment <- monthly_experiment %>%
  mutate(Year = as.character(Year), Source = "Experiment")

# Add a "Source" column and ensure Year is also character in dat_trends
dat_trends <- dat_trends %>%
  mutate(Year = "Historical", Source = "Historic Mean")  # Label for historic data

# Bind both datasets together
combined_data <- bind_rows(monthly_experiment, dat_trends)

# Check structure
glimpse(combined_data)




ggplot(data = combined_data, 
       aes(x = Month, 
           y = Rain_mean, 
           group = interaction(Year, Source), 
           color = Year)) +
  
  # Line plot for experimental data (different years)
  geom_line(data = filter(combined_data, Source == "Experiment"), 
            aes(), linewidth = 1) +
  
  # Line plot for historic mean data (dashed)
  geom_line(data = filter(combined_data, Source == "Historic Mean"), 
            linetype = "dashed", linewidth = 1.2) +
  
  # Labels and theme
  labs(title = "Monthly Rainfall Over 3 Years vs. Historic Mean",
       subtitle = "Comparing Experiment vs. Long-Term Trends",
       x = "Month",
       y = "Precipitation (mm)",
       color = "Data Source",
       linetype = "Year") +
  
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  # Format month labels (Jan, Feb, ...)
  theme_bw()



names(combined_data)

ggplot(data = combined_data, 
       aes(x = Month, 
           y = Tmin_mean, 
           group = interaction(Year, Source), 
           color = Year)) +
  
  # Line plot for experimental data (different years)
  geom_line(data = filter(combined_data, Source == "Experiment"), 
            aes(), linewidth = 1) +
  
  # Line plot for historic mean data (dashed)
  geom_line(data = filter(combined_data, Source == "Historic Mean"), 
            linetype = "dashed", linewidth = 1.2) +
  
  # Labels and theme
  labs(title = "Monthly Rainfall Over 3 Years vs. Historic Mean",
       subtitle = "Comparing Experiment vs. Long-Term Trends",
       x = "Month",
       y = "Precipitation (mm)",
       color = "Data Source",
       linetype = "Year") +
  
  scale_x_continuous(breaks = 1:12, labels = month.abb) +  # Format month labels (Jan, Feb, ...)
  theme_bw()









