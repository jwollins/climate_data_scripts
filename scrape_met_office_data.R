

## 00 SETUP ####
setwd(dir = "OneDrive - Harper Adams University/Data/dssat_files/")

## 01 PACKAGES ####
# Load necessary libraries
library(rvest)
library(dplyr)


## 02 SCRAPE ####
# Specify the URL
url <- "https://www.metoffice.gov.uk/research/climate/maps-and-data/location-specific-long-term-averages/gcqh76ug7"

# Read the webpage
webpage <- read_html(url)

# Extract the first table
table <- webpage %>%
  html_node("table") %>% # Select the first table
  html_table()

# Check the extracted table
print(table)

# Save the table as a CSV file
write.csv(table, "shawbury_metoffice_table.csv", row.names = FALSE)

write.csv(table, "~/Applications (Parallels)/{ccc9e4c7-e67b-4a2f-b0dd", row.names = FALSE)


# Notify the user
cat("The table has been saved as 'metoffice_table.csv'.\n")



##