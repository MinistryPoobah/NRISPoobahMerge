
# __ This script was designed for the Environmental Protection Division Compliance Team
# __ with the purpose of cleaning, filtering, and mergeing the poobah and NRIS databases 
# __ for use in the Compliance Team ArcGIS Online Dashboard.

# The script can be found on GitHub at https://github.com/MinistryPoobah/NRISPoobahMerge.


# Author: Keith Story, M.Sc., R.P. Bio.
# Ministry of Environment and Climate Change Strategy
# Environmental Protection Division


# _______________________________________________________________________________________

# DEPENDENCIES

    library(tidyverse)
    library(data.table)
    library(readxl)
    library(googledrive)
    library(lubridate)
    
# _______________________________________________________________________________________
    
# AUTHORIZATION FOR GOOGLE CLOUD
    
    drive_auth(
      email = "keithtrizinstory.gov@gmail.com",
    )
    
    
# _______________________________________________________________________________________

# FILE IMPORTS
    
# Read in the csv files. Recognize #N/A and blanks as NA
    poobah <- read_xlsx(path = "C:/Users/kstory/Documents/GrandPoobah_R/Poobah/2020-2021 Poobah 2020-04-30.xlsx", sheet = "Assigned List", guess_max = 1048576,  skip = 3, na = c("", NA, "#N/A"))
    NRIS_inspections <- read_csv(file = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/NRIS/Inspections/NRIS.SearchResult.2020-05-08 14_05_32.csv", na = c("", NA,"#N/A"))
    NRIS_complaints <- read_csv(file = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/NRIS/Complaints/NRIS.SearchResult.2020-05-08 14_07_32.csv", na = c("", NA,"#N/A"))
    
# _______________________________________________________________________________________
    
# FUNCTIONS
    
    `%notin%` <- Negate(`%in%`) # Dummy function made to exlude fields during filtering.
    
# _______________________________________________________________________________________  
    
# POOBAH
    
    dashboard <- poobah %>%
      filter(`Workplan INS Qtr` == "Q1") %>% # Filter for quarter.
      mutate(Long = as.numeric(Long) *-1) %>% #the data in the poobah have the longitude as positive. It should be negative.
      mutate("Authorizations - Name" = paste(`Auth Num`, Authorization, sep = " - ")) # Create a unique site name that includes both the auth number and site name
    # filter(Assigned %notin% c("Abandoned", "Cancelled", "Reactive", "Defer", "Doesn't exist", "?"))

# _______________________________________________________________________________________
  
# NRIS INSPECTIONS AND POOBAH MERGE
    
    NRIS_inspections_filtered <- NRIS_inspections %>%
      filter(`EP System` == "AMS" | `EP System` == "Other") %>% # Filter for data from AMS and Other ("Other" includes UAs)
      rename("Auth Num" = "Authorization ID") %>% # Make field name match the poobah name so a merge can be easily made.
      filter(`Inspection Status` %notin% c("Deleted", "Closed", "Template")) %>% # Omit not useful NRIS data.
      # filter(`Inspection Date` > "2020-01-01") %>%
      select(`Auth Num`, `Inspection Status`, `Inspection Date`, "Latitude", "Longitude") # Trim down the data set now that it is filtered.

# Recode some weird values and mizspellz in the poobah Assigned field.
    dashboard$Assigned[is.na(dashboard$Assigned)] <- "Unassigned"
    dashboard$`Assigned`[which(dashboard$`Assigned` == "jeffery")] <- "Jeffery"
    dashboard$`Assigned`[which(dashboard$`Assigned` == "White")] <- "T. White"
    dashboard$Assigned[which(dashboard$Assigned == "Naseri")] <- "Nazeri"

# Merge the cleaned/filtered poobah data with the NRIS data. Merge on the Auth Num field, as this is a unique identifier. Remove duplicate rows.
    dashboard_merge <- merge(x=NRIS_inspections_filtered,y=dashboard,by="Auth Num",all.y =TRUE)
    dashboard_merge <- dashboard_merge[!duplicated(dashboard_merge),]
    
# Update the poobah "Last Inspected" column to reflect NRIS "Inspection Date"
    dashboard_merge$`Last Inspected` <- as.POSIXct(ifelse(!is.na(dashboard_merge$`Inspection Date`), dashboard_merge$`Inspection Date`, dashboard_merge$`Last Inspected`), origin = "1970-01-01 00:00:00")
    
# Update the poobah "Inspected this Fiscal?" column to "In Draft", "Complete", or "Not Started" based on NRIS data
    dashboard_merge$'Inspected This Fiscal?'[which(dashboard_merge$`Inspection Status` == "Incomplete")] <- "In Draft"
    dashboard_merge$'Inspected This Fiscal?'[which(dashboard_merge$`Inspection Status` == "Complete")] <- "Complete" 
    dashboard_merge$'Inspected This Fiscal?'[which(dashboard_merge$'Inspected This Fiscal?' == "No")] <- "Not Started"
    
# Recode the Rank to Priority for easy viewing on the map legend
    dashboard_merge$'Rank'[which(dashboard_merge$'Rank' == "1")] <- "Priority 1"
    dashboard_merge$'Rank'[which(dashboard_merge$'Rank' == "2")] <- "Priority 2"
    dashboard_merge$'Rank'[which(dashboard_merge$'Rank' == "3")] <- "Priority 3"
    dashboard_merge$'Rank'[which(is.na(dashboard_merge$'Rank'))] <- "Priority 4"
    

# Update Poobah lat/lon with the NRIS Latitude and Longitude fields. If NRIS users are updating the Lat/Lon, this will be reflected on the map.
    dashboard_merge$Lat[is.na(dashboard_merge$Lat)] <- dashboard_merge$Latitude[is.na(dashboard_merge$Lat)]
    dashboard_merge$Long[is.na(dashboard_merge$Long)] <- dashboard_merge$Longitude[is.na(dashboard_merge$Long)]
    
# Missing coordinates recoded to a site slightly off the coast of BC. This allows users to idetify sites that need an update to their coordinates in NRIS.
    dashboard_merge$Lat[which(is.na(dashboard_merge$Lat))] <- 56.1
    dashboard_merge$Lat[which(dashboard_merge$Lat == 0)] <- 56.1
    dashboard_merge$Long[which(is.na(dashboard_merge$Long))] <- -138
    dashboard_merge$Long[which(dashboard_merge$Long == 0)] <- -138
    
# Some entries are duplicates. Choose most recent of duplicated entries
    dashboard_merge <- setDT(dashboard_merge)[order(`Auth Num`, -as.POSIXct(`Last Inspected`, "%y/%m/%d"))][!duplicated(`Auth Num`)]
    
# Update the "last inspected" date in the poobah with any recent updates in NRIS.
    dashboard_merge$`Last Inspected` <- as.Date(dashboard_merge$`Last Inspected`)

# Additional filtering on the merged dataset.
    dashboard_merge <- dashboard_merge %>%
      select(-c(2:5, 26)) %>%
      filter(!is.na(`Auth Num`))
      # mutate("Completion Status" = case_when(
      #   dashboard_merge$`Inspected This Fiscal?` == "Not Started" ~ 0,
      #   dashboard_merge$`Inspected This Fiscal?` == "In Draft" ~ 0,
      #   TRUE ~ 1
      # )) # The mutate function here creates a column that has "1" for complete, and "0" for not complete. This is needed for % progress plots, which were removed.

# _______________________________________________________________________________________
    
# NRIS COMPLAINTS
    
    
# Basic filtering of data. Inspector name in weird format, so used a function to removed the "IDIR\" portion of name.
    NRIS_complaints_filtered <- NRIS_complaints %>%
      select(-c(2, 4:7, 9, 11, 12, 13, 18, 19, 23:30, 33:35, 38, 39)) %>%
      mutate(Inspector = str_replace(Inspector, "IDIR\\\\", ""))

# Missing coordinates recoded to a site slightly off the coast of BC. This allows users to idetify sites that need an update to their coordinates in NRIS.
    
    NRIS_complaints_filtered$Latitude[which(is.na(NRIS_complaints_filtered$Latitude))] <- 56.1
    NRIS_complaints_filtered$Latitude[which(NRIS_complaints_filtered$Latitude == 0)] <- 56.1
    NRIS_complaints_filtered$Longitude[which(is.na(NRIS_complaints_filtered$Longitude))] <- -138
    NRIS_complaints_filtered$Longitude[which(NRIS_complaints_filtered$Longitude == 0)] <- -138
    
# _______________________________________________________________________________________________________________________

# EXPORTING TO CSV AND GOOGLE DRIVE
    

    out_file <- paste(Sys.Date(), "2_DashboardData.csv", sep = "_")
    
    write_csv(dashboard_merge, out_file)
    write_csv(dashboard_merge, "Updated NRIS Inspection Data.csv")
    write_csv(NRIS_complaints_filtered, "Updated NRIS Complaints Data.csv")
    
# Use drive upDATE only when a files has been previously uploaded. This allows the file to use the same id, which is referenced by ArcGIS. Using Drive upLOAD will overwrite the id.
    (Inspections <- drive_update(file = as_id("1rcF6V7CbuIIaLsDkNCvlOcRF6O1IHoMD"), media = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/Updated NRIS Inspection Data.csv"))
    (Complaints <- drive_update(file = as_id("1kN78zG_yXGTpuoMgE35iOvSERQy986cK"), media = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/Updated NRIS Complaints Data.csv"))
    # (Complaints <- drive_upload(media = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/Updated NRIS Complaints Data.csv", path = "Compliance/", overwrite = TRUE))
    # (Inspections <- drive_upload(media = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/Updated NRIS Inspection Data.csv", path = "Compliance/", overwrite = TRUE))
    
