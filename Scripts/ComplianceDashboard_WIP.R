
# __ This script was designed for the Environmental Protection Division Compliance Team
# __ with the purpose of cleaning, filtering, and merging the poobah and NRIS databases 
# __ for use in the Compliance Team ArcGIS Online Dashboard.

# The script can be found on GitHub at https://github.com/MinistryPoobah/NRISPoobahMerge.


# Author: Keith Story, M.Sc., R.P. Bio.
# Ministry of Environment and Climate Change Strategy
# Environmental Protection Division

# ____________________________________________________________________________________________________  
# ____________________________________________________________________________________________________


#### _________________________________INITIALIZATION AND DATA IMPORT__________________________________

# DEPENDENCIES

  library(tidyverse)
  library(data.table)
  library(readxl)
  library(googledrive)
  library(lubridate)
  

# LAN path for saving outputs
  
  setwd("//Sfp.idir.bcgov/s140/s40086/WANSHARE/ROB/ARCS/ROB ARCS/Information Technology 6000-6999/6820-20 ArcGIS Dashboard/DashboardDataOutput") 

# _______________________________________________________________________________________
  
# FUNCTIONS
  
  `%notin%` <- Negate(`%in%`) # Dummy function made to exlude fields during filtering.
  
# _______________________________________________________________________________________

# AUTHORIZATION FOR GOOGLE CLOUD
  
  drive_auth(email = "epdcompliancedashboard@gmail.com")
  
# _______________________________________________________________________________________
  

# FILE LIST INFO
  
# Saves a file list of the paths in each data directory.
  poobah_file_list <- file.info(list.files("//Sfp.idir.bcgov/s140/s40086/WANSHARE/ROB/ARCS/ROB ARCS/Information Technology 6000-6999/6820-20 ArcGIS Dashboard/Poobah", full.names = TRUE))
  inspections_file_list <- file.info(list.files("//Sfp.idir.bcgov/s140/s40086/WANSHARE/ROB/ARCS/ROB ARCS/Information Technology 6000-6999/6820-20 ArcGIS Dashboard/NRIS/Inspections", full.names = TRUE))
  complaints_file_list <- file.info(list.files("//Sfp.idir.bcgov/s140/s40086/WANSHARE/ROB/ARCS/ROB ARCS/Information Technology 6000-6999/6820-20 ArcGIS Dashboard/NRIS/Complaints", full.names = TRUE))
  datamart_file_list <- file.info(list.files("//Sfp.idir.bcgov/s140/s40086/WANSHARE/ROB/ARCS/ROB ARCS/Information Technology 6000-6999/6820-01 Datamarts", pattern = "DataMart of AMS Regulated Parties", full.names = TRUE))
  
  # to-do: create a path list checker for the AMS datamart too.
  
# _______________________________________________________________________________________

# FILE IMPORTS

# Read in the most recently updated spreadsheet from the respective file lists. Recognize #N/A and blanks as "NA"
  poobah <- read_xlsx(path = rownames(poobah_file_list[which.max(poobah_file_list$mtime),]), sheet = "Assigned List", guess_max = 1048576,  skip = 3, na = c("", NA, "#N/A"))
  poobah_IR_complete <- read_xlsx(path = rownames(poobah_file_list[which.max(poobah_file_list$mtime),]), sheet = "IRs Complete", guess_max = 1048576,  skip = 5, na = c("", NA, "#N/A"))
  NRIS_inspections <- read_csv(file = rownames(inspections_file_list[which.max(inspections_file_list$mtime),]), na = c("", NA,"#N/A"))
  NRIS_complaints <- read_csv(file = rownames(complaints_file_list[which.max(complaints_file_list$mtime),]), na = c("", NA,"#N/A"))
  datamart <- read_xlsx(path = rownames(datamart_file_list[which.max(datamart_file_list$mtime),]), sheet = "ALL", na = c("", NA,"#N/A", "n/a"))
  name_key <- read_csv(file = "//Sfp.idir.bcgov/s140/s40086/WANSHARE/ROB/ARCS/ROB ARCS/Information Technology 6000-6999/6820-20 ArcGIS Dashboard/Name Key.csv")
  

# __________________________________________________________________________________________  
# __________________________________________________________________________________________ 

#### ____________________________________POOBAH UPDATING____________________________________ 

# The current "Assigned List" in the Poobah does not reflect the IRs that have been completed. To better reflect the planned team's
# workload, the "Assigned List" needs to be updated with the "IRs Complete".

# Mutate the "Inspector" field to reflect the actual name using the "names" sheet key. Check for any "NAs
  
  poobah_IR_complete_filt <- poobah_IR_complete %>%
    filter(`Inspection Date` > "2020-03-31") %>%
    select(`EP System Number`, `Regulated Party`, `Requirement Source`, `Inspection Status`, `Inspection Date`, `Inspection Signed By`) %>%
    mutate(`Inspection Signed By` = str_replace(`Inspection Signed By`, "IDIR\\\\","")) %>%
    rename("Auth Num" = "EP System Number")
  
  poobah_IR_complete_filt$`Inspection Signed By` <- name_key$`poobah name`[match(poobah_IR_complete_filt$`Inspection Signed By`, name_key$`nris name`)]
  
  poobah_IR_complete_filt <- poobah_IR_complete_filt[!duplicated(poobah_IR_complete_filt),]
  # poobah_auth_list <- unique(poobah$`Auth Num`)
    
  
# Add rows from IRs Total.

  poobah_complete_merged <- merge(poobah, poobah_IR_complete_filt, by = "Auth Num", all = TRUE)
  poobah_complete_merged <- poobah_complete_merged[!duplicated(poobah_complete_merged),]
  poobah_complete_merged$`Inspected This Fiscal?` <- ifelse(!is.na(poobah_complete_merged$`Inspection Status`), "Yes", poobah_complete_merged$`Inspected This Fiscal?`)
  poobah_complete_merged$`Authorization` <- ifelse(!is.na(poobah_complete_merged$`Regulated Party`), poobah_complete_merged$`Regulated Party`, poobah_complete_merged$`Authorization`)
  poobah_complete_merged$`Sector` <- ifelse(!is.na(poobah_complete_merged$`Requirement Source`), poobah_complete_merged$`Requirement Source`, poobah_complete_merged$`Sector`)
  poobah_complete_merged$`Assigned` <- ifelse(!is.na(poobah_complete_merged$`Inspection Signed By`), poobah_complete_merged$`Inspection Signed By`, poobah_complete_merged$`Assigned`)
  


  # write_csv(poobah_complete_merged, "test_out1.csv")
  

#### _________________________________CLEANING AND MERGING__________________________________  

# POOBAH

  dashboard <- poobah_complete_merged %>%
    # filter(`Workplan INS Qtr` == "Q1") %>% # Filter for quarter.
    mutate(Long = as.numeric(Long) *-1) %>% #the data in the poobah have the longitude as positive. It should be negative.
    mutate("Authorizations - Name" = paste(`Auth Num`, Authorization, sep = " - ")) # Create a unique site name that includes both the auth number and site name
  # filter(Assigned %notin% c("Abandoned", "Cancelled", "Reactive", "Defer", "Doesn't exist", "?"))

# _______________________________________________________________________________________

# NRIS INSPECTIONS AND POOBAH MERGE

  NRIS_inspections_filtered <- NRIS_inspections %>%
    filter(`EP System` != "CRISP", `Requirement Source` %notin% c("Greenhouse Gas Industrial Reporting and Control Act", "Integrated Pest Management Act")) %>% # Filter for data from AMS and Other ("Other" includes UAs)
    rename("Auth Num" = "Authorization ID") %>% # Make field name match the poobah name so a merge can be easily made.
    filter(`Inspection Status` %notin% c("Deleted", "Template")) %>% # Omit non-useful NRIS data.
    select(`Auth Num`, `Inspection Status`, `Inspector`, `Inspection Date`, "Latitude", "Longitude") # Trim down the data set now that it is filtered.


# Recode some weird values and mizspellz in the poobah Assigned field.
  dashboard$`Assigned`[is.na(dashboard$`Assigned`)] <- "Unassigned"
  dashboard$`Assigned`[which(dashboard$`Assigned` == "jeffery")] <- "Jeffery"
  dashboard$`Assigned`[which(dashboard$`Assigned` == "White")] <- "T. White"
  dashboard$`Assigned`[which(dashboard$`Assigned` == "Naseri")] <- "Nazeri"


# Merge the cleaned/filtered poobah data with the NRIS data. Merge on the Auth Num field, as this is a unique identifier. Remove duplicate rows.
  dashboard_merge <- merge(x = NRIS_inspections_filtered, y = dashboard, by ="Auth Num", all.y = TRUE)
  dashboard_merge <- dashboard_merge[!duplicated(dashboard_merge), ]

  
# Update the poobah "Last Inspected" column to reflect NRIS "Inspection Date"
  dashboard_merge$`Last Inspected` <- as.POSIXct(ifelse(!is.na(dashboard_merge$`Inspection Date.x`), dashboard_merge$`Inspection Date.x`, dashboard_merge$`Last Inspected`), origin = dashboard_merge$`Inspection Date.x`)
  # dashboard_merge$`Last Inspected` <- as.POSIXct(ifelse(!is.na(dashboard_merge$`Inspection Date`), dashboard_merge$`Inspection Date`, dashboard_merge$`Last Inspected`), origin = "1970-01-01 00:00:00")

  
# Update the poobah "Inspected this Fiscal?" column to "In Draft", "Complete", or "Not Started" based on NRIS data
# The following "for" loop checks through the poobah Assigned names. If the Assigned name matches the list of approved names, then the status can be updated.
# The purpose of this loop is to ensure that inspections completed by reactive aren't updated as "complete" when they actually haven't been inspected by planned.  
   for (i in 1:nrow(dashboard_merge)){
    if (dashboard_merge$Assigned %in% name_key$`poobah name`){
      dashboard_merge$'Inspected This Fiscal?'[which(dashboard_merge$`Inspection Status.x` == "Complete")] <- "Complete" 
      dashboard_merge$'Inspected This Fiscal?'[which(dashboard_merge$'Inspected This Fiscal?' == "Yes")] <- "Complete" 
      dashboard_merge$'Inspected This Fiscal?'[which(dashboard_merge$'Inspected This Fiscal?' == "No")] <- "Not Started"
      dashboard_merge$'Inspected This Fiscal?'[which(dashboard_merge$`Inspection Status.x` == "Incomplete")] <- "In Draft"
      }
    }
  

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
  dashboard_merge_filt <- dashboard_merge %>%
    select(-c(2:6, 27:51)) %>%
    filter(!is.na(`Auth Num`))
  
  dashboard_merge_filt$CPIX[which(dashboard_merge_filt$CPIX == 0)] <- NA


# _______________________________________________________________________________________

# NRIS COMPLAINTS


# Basic filtering of data. Inspector name in weird format, so used a function to removed the "IDIR\\" portion of name.
  NRIS_complaints_filtered <- NRIS_complaints %>%
    select(-c(2, 4:7, 9, 11, 12, 13, 18, 19, 23:30, 33:35, 38, 39)) %>%
    mutate(Inspector = str_replace(Inspector, "IDIR\\\\", ""))

# Missing coordinates recoded to a site slightly off the coast of BC. This allows users to idetify sites that need an update to their coordinates in NRIS.
  NRIS_complaints_filtered$Latitude[which(is.na(NRIS_complaints_filtered$Latitude))] <- 56.1
  NRIS_complaints_filtered$Latitude[which(NRIS_complaints_filtered$Latitude == 0)] <- 56.1
  NRIS_complaints_filtered$Longitude[which(is.na(NRIS_complaints_filtered$Longitude))] <- -138
  NRIS_complaints_filtered$Longitude[which(NRIS_complaints_filtered$Longitude == 0)] <- -138

# _______________________________________________________________________________________
  
# AMS Datamart Authorizations
  
  AMS_clean <- datamart %>%
    select(1:9,14,27,28) %>%
    mutate(Longitude = Longitude *-1) %>%
    filter(!is.na(`AMS System Number`))
  
  AMS_clean <- AMS_clean[!duplicated(AMS_clean),]
  AMS_clean$`Waste Type`[which(is.na(AMS_clean$`Waste Type`))] <- "Combination or Other"
  
  AMS_clean <- AMS_clean %>%
    mutate("Name" = paste(AMS_clean$'AMS System Number', `Regulated Party`, sep = " - "))
  
  AMS_clean$`Issue Date` <- as.Date(AMS_clean$`Issue Date`)
  AMS_clean$`Expiry Date` <- as.Date(AMS_clean$`Expiry Date`)
  
  AMS_clean$Latitude[which(is.na(AMS_clean$Latitude))] <- 56.1
  AMS_clean$Latitude[which(AMS_clean$Latitude == 0)] <- 56.1
  AMS_clean$Longitude[which(is.na(AMS_clean$Longitude))] <- -138
  AMS_clean$Longitude[which(AMS_clean$Longitude == 0)] <- -138

# _______________________________________________________________________________________
# _______________________________________________________________________________________


  
# ____________________________EXPORTING TO CSV AND GOOGLE DRIVE__________________________
  
  out_file <- paste("ProgressTracker/", Sys.Date(), "_DashboardData.csv", sep = "")
  
  write_csv(dashboard_merge_filt, out_file)
  write_csv(dashboard_merge_filt, "Updated NRIS Inspection Data.csv") # !! DO NOT CHANGE THE OUTFILE NAME
  write_csv(NRIS_complaints_filtered, "Updated NRIS Complaints Data.csv") # !! DO NOT CHANGE THE OUTFILE NAME
  write_csv(AMS_clean, "Updated_Authorizations.csv") # !! DO NOT CHANGE THE OUTFILE NAME

# Use drive upDATE only when a files has been previously uploaded. This allows the file to use the same id, which is referenced by ArcGIS. Using Drive upLOAD will overwrite the id.
  (Inspections <- drive_update(file = as_id("1qYZ1zvyOt2P6_eNwysNUEezcRgb5qqC0"), media = "Updated NRIS Inspection Data.csv"))
  (Complaints <- drive_update(file = as_id("1RixYE1ApAMvKk350pwx4A-MwboSs6MeX"), media = "Updated NRIS Complaints Data.csv"))
  (Authorizations <- drive_update(file = as_id("1iVRv5-eSrQreN9uocFnjTtgt3EfjAsy-"), media = "Updated_Authorizations.csv"))

# drive_upload is used for creation of the file on Drive and the creation of the ID. Do not use this unless you really know what you are doing! Overwriting the ID will cause serious problems in ArcGIS Online.
# (Complaints <- drive_upload(media = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/Updated NRIS Complaints Data.csv", path = "Compliance/", overwrite = TRUE))
# (Inspections <- drive_upload(media = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/Updated NRIS Inspection Data.csv", path = "Compliance/", overwrite = TRUE))
# (Authorizations <- drive_upload(media = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/Updated_Authorizations.csv", path = "Compliance/", overwrite = TRUE))


# _______________________________________________________________________________________
# _______________________________________________________________________________________
# ___________________________________END_________________________________________________
# _______________________________________________________________________________________












# df_check <- dashboard_merge %>%
#   filter(Assigned == "OK Blitz")
# 
# write_csv(df_check, "poobah_blitz_check.csv")
