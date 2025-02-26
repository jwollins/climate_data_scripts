### ERP June 2024 Deployment 
## Weather Data 
## Joe Collins 
## 2024-09-01
### 02 - load data and plot every variable


# setwd(dir = "~/Documents/ERP/scripts/erpsoiltools/weather_station/")


## 00 packages ####

source(file = "weather_station/01_packages.R")


## 01 Load Data ####

setwd(dir = "~/jcollins@earthroverprogram.org - Google Drive/Shared drives/DATA/FIELD_DEPLOYMENTS/2024-06-Joesfield/soil_data/weather_station/")

### 01.1 weather data ####
dat <- read.csv(file = "raw_data/ERP_1(A4100533)-Configuration 1-1723196443.681639.csv", skip = 2)
dat$Timestamps <- strptime(dat$Timestamps, format = "%m/%d/%Y %I:%M:%S %p")
dat$Timestamps <- as.POSIXct(dat$Timestamps)


### 01.2 data information ####
info <- read.csv(file = "raw_data/ERP_1(A4100533)-Metadata-1723196443.681639.csv")

info2 <- read.csv(file = "raw_data/ERP_1(A4100533)-Raw-Configuration 1-1723196443.681639.csv")


## raw data 
for (i in 2:ncol(dat)) {
  
  p <-  ggplot(data = dat, 
               mapping = aes(x = Timestamps,
                             y = dat[[i]])) + 
    geom_line(linewidth = 0.6) + 
    labs(title = colnames(dat[i]), 
         subtitle = "ERP June 2024 Deployment",
         y = colnames(dat[i])) + 
    theme_linedraw()
  
  ggsave(filename = paste0("plots/", colnames(dat[i]), ".png"), plot = p)
  
}




