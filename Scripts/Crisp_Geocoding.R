#The purpose of this script is to geocode the CRISP datamart locations and output a csv file for conversion to kml.
#The deliverable is a functional google earth map showing the sites of interest.
#Credit to Aleszu Bajak at https://www.storybench.org/geocode-csv-addresses-r/ for the skeleton code.

setwd("C:/Users/kstory/Documents/CRISP")

#google API key: AIzaSyDuyKPtuOUckdpkMV5qoU9FHUlF_J-ieRE
register_google(key = "AIzaSyDuyKPtuOUckdpkMV5qoU9FHUlF_J-ieRE", write = TRUE)

# Geocoding a csv column of "addresses" in R


#load ggmap
library(ggmap)
library(tidyverse)

# Select the file from the file chooser
fileToLoad <- file.choose(new = TRUE)

# Read in the CSV data and store it in a variable 
origAddress <- read.csv(fileToLoad, stringsAsFactors = FALSE)
origAddressExtract <- origAddress

origAddressExtract$Location.Address <- str_replace(origAddressExtract$Location.Address, "#", "")



# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)
# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(origAddressExtract))
        {
        tryCatch({
            # Print("Working...")
            result <- geocode(origAddressExtract$Location.Address[i], output = "latlona", source = "google")
              
            origAddressExtract$lon[i] <- as.numeric(result[1])
            origAddressExtract$lat[i] <- as.numeric(result[2])
            origAddressExtract$geoAddress[i] <- as.character(result[3])
                },
            error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
            )
                
        }
# Write a CSV file containing origAddress to the working directory
write.csv(origAddressExtract, "geocoded.csv", row.names=FALSE)