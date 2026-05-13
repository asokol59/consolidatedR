pacman::p_load("tidyverse", "janitor", "lubridate","stringr","forcats", "openxlsx", "tidyr","kableExtra", "scales", "knitr")

datLink_previousAY <- "C:/Users/asokol/OneDrive - University of South Carolina/StudentLife/Data/Master Lists/SL Final Attendance AY2425 20260218.xlsx"
datLink_currentAY <- "C:/Users/asokol/OneDrive - University of South Carolina/StudentLife/Data/Weekly Numbers Pull/Clean Data/Spring 2026 to Apr 12 20260415.xlsx"


#Read in Current Year files 
CurrAY_Student <-Clean_Event %>% 
  filter(Affiliation !="Faculty/Staff",
        `Start Date` >= floor_date(Sys.Date(), "year") &
        `Start Date` <  floor_date(Sys.Date(), "week", week_start = 1)- days(1))

# Read in previous year

dat_PreviousAY <- read.xlsx(datLink_previousAY, detectDates=T, sep.names= " ") %>% 
  filter(Affiliation !="Faculty/Staff") %>% 
  filter(`Start Date` >= floor_date(Sys.Date(), "year") - years(1) &
         `Start Date` <  floor_date(Sys.Date(), "week", week_start = 1) - years(1)- days(1))

# Remove FSL Events since FSL is no longer apart of student affairs
rm_majorEventFSL <- dat_PreviousAY %>% 
  filter(Unit!= "FSL")

#### number of events for current and previous year ####

## number of events for previous semester with FSL Removed
LastYR_NumEvent <- rm_majorEventFSL %>% 
  group_by(Event) %>% 
  summarise(Count=n(), .groups="drop")

## number of event for current semester
CurrYr_Num_Event <- Weekly %>% 
  group_by(Event) %>% 
  summarise(Count=n(), .groups= "drop")

#### compute percentage ####

# Determine the number of row in each data Set
numRow_Curr <- nrow(Weekly)
numRow_Prev <- nrow(rm_majorEventFSL) #- 3228

# compute percent difference
round(((numRow_Curr-numRow_Prev)/numRow_Prev)*100,2)

