# The purpose of this script is to provide tracking metrics for the compliance team over time.
# The script will read in all of the NRIS spreadsheet downloads saved on file and summarize the number of inspections completed in that time.

# Reading in data: iterate among files, summarize total number of "Yes", report.

library(tidyverse)
library(data.table)
library(readxl)
library(googledrive)
library(lubridate)
library(gtools)

drive_auth(
  email = "epdcompliancedashboard@gmail.com",
)

`%notin%` <- Negate(`%in%`)

path.set = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/DashboardDataOutput/"
setwd("C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/DashboardDataOutput/")

name_key <- read_csv(file = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/Name Key.csv")

# file.names <- dir(path.set, pattern =".csv")
# master.file <- list()
# 
# extract_summarize <- function(file.names) {
#   in.file <- read_csv(file = file.names, na = c("", NA,"#N/A"))
#   file.date <- str_sub(file.names, start = 1, end = 10)
#   
#   in.file$`Assigned`[is.na(in.file$`Assigned`)] <- "Unassigned"
#   in.file$`Assigned`[which(in.file$`Assigned` == "jeffery")] <- "Jeffery"
#   in.file$`Assigned`[which(in.file$`Assigned` == "White")] <- "T. White"
#   in.file$`Assigned`[which(in.file$`Assigned` == "Naseri")] <- "Nazeri"
#   in.file$`Assigned`[which(in.file$`Assigned` == "beck")] <- "Beckett"
#   
#   in.file <- in.file %>%
#     select(Assigned, `Inspected This Fiscal?`) %>%
#     filter(Assigned %in% unique(name_key$`poobah name`), Assigned != "Unassigned") %>%
#     filter(`Inspected This Fiscal?` == "Yes") 
#   
#   in.file <- as.tibble(with(in.file, table(Assigned, `Inspected This Fiscal?`)))
#   
#   in.file <- in.file %>%
#     select(-`Inspected This Fiscal?`) %>%
#     rename(!!file.date := n)
#   
#   bind_cols
# }
# 
# master.list <- lapply(file.names, extract_summarize)
#____________________________________________________________

multmerge = function(mypath){
  
  filenames = list.files(path = mypath, full.names = TRUE)
  
  datalist = lapply(filenames, function(x){
    in.file <- read_csv(file = x)
    # csv.names <- dir(path = x, pattern =".csv")
    file.date <- str_sub(x, start = -28, end = -19)
    
    in.file$`Assigned`[is.na(in.file$`Assigned`)] <- "Unassigned"
    in.file$`Assigned`[which(in.file$`Assigned` == "jeffery")] <- "Jeffery"
    in.file$`Assigned`[which(in.file$`Assigned` == "White")] <- "T. White"
    in.file$`Assigned`[which(in.file$`Assigned` == "Naseri")] <- "Nazeri"
    in.file$`Assigned`[which(in.file$`Assigned` == "beck")] <- "Beckett"
    in.file$`Inspected This Fiscal?`[which(in.file$`Inspected This Fiscal?` == "Yes")] <- "Complete"
    
  
    
    in.file <- in.file %>%
      select(Assigned, `Inspected This Fiscal?`) %>%
      filter(Assigned %in% unique(name_key$`poobah name`), Assigned != "Unassigned") %>%
      filter(`Inspected This Fiscal?` == "Complete")
    
    in.file <- as.data.frame(table(in.file$`Assigned`, in.file$`Inspected This Fiscal?`), col.names = names("Assigned", "Completion", "Freq"))
    
    # print(names(in.file))


    in.file <- in.file %>%
      select(-Var2) %>%
      mutate(Date = file.date)
     #rename(!!file.date := Freq) #%>%
      # rename(Assigned = Var1)
    
    # print(in.file)
    
    })
  
  # print(datalist)
  # Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Var1", all.x = TRUE), mymergeddata)
  
}

mymergeddata = multmerge("C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/DashboardDataOutput")

mymergeddata <- mymergeddata %>%
  bind_rows %>%
  rename(Assigned = Var1) %>%
  filter(as.Date(Date) > "2020-04-14")


spread_compliance_data <- mymergeddata %>%
  bind_rows %>%
  # mutate(ID = row_number()) %>%
  spread(Date, Freq) %>%
  rename(Assigned = Var1)


#__________________________________________________________
(p <- ggplot(data = mymergeddata, aes(as.Date(Date), Freq), color = Assigned) +
  geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ Assigned) +
    theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  )

# ___________________________________________________________________

(ComplianceTeamOverview <- ggplot(data = mymergeddata, aes(as.Date(Date), Freq), color = Assigned) +
    geom_point(color = "white") +
    geom_smooth(method = "loess", color = "olivedrab2") +
    theme_light() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("Month (2020)") +
    ylab("# of Inspections, Cumulative") +
    ggtitle("Compliance Team Overview") +
    theme_dark() +
    
    theme(plot.background = element_rect(fill = "grey17"),
          panel.background = element_rect(fill = "grey17"),
          plot.title = element_text(color = "white", size=18, hjust = 0.5, face="bold"),
          axis.title.x = element_text(color = "white", size = 15),
          axis.title.y = element_text(color = "white", size = 15)
          )
)

png("C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/ComplianceTeamOverview.png")
print(ComplianceTeamOverview)
dev.off()

# (Plots <- drive_upload(media = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/ComplianceTeamOverview.png", path = "Plots/", overwrite = TRUE))
(Plots <- drive_update(file = as_id("1B_cz-RFU9-0EzwFtMIg7Lox3J3QLK9Nb"), media = "C:/Users/kstory/Documents/GrandPoobah_R/Dashboard Data/ComplianceTeamOverview.png"))


