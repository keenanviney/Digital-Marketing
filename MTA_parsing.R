##-------------------------------------------------------------------
#  A script to parse and adjust MTA data
##-------------------------------------------------------------------

## Load Packages

library(tidyverse)


## Import Data

MTA_full_data <- read_csv("/Users/keenanv/Desktop/MTA_Parsing/MTA.csv")


## Extract out Creative IDs into a new column

MTA_parsed <- MTA_full_data %>% 
  mutate(C_Creative_ID = if_else(str_detect(`Creative Name`, "1x1|Nstar Redirect"), 
                                  str_extract(`Placement Name`, "\\d{1,2}[A-Z]{3,5}\\d{2,4}|[A-Z]{3,5}\\d{2,4}"),
                                  str_extract(`Creative Name`, "\\d{1,2}[A-Z]{3,5}\\d{2,4}|[A-Z]{3,5}\\d{2,4}")))


## Group each unique Creative ID together and summarize variables

MTA_Creative_ID <- MTA_parsed %>% 
  group_by(C_Creative_ID) %>% 
  summarize(Spending_Adjustment = sum(`Event Count`),
            Event_Count = sum(`Event Count`)/sum(`Matched Event Count`), 
            Matched_Event_Count = sum(`Matched Event Count`),
            Fractional_Activation_Count = sum(`Fractional Activation Count`),
            Adjusted_Incremental_Activation_Count = sum(`Incremental Activation Count`)/0.58,
            Unattributed_Activation_Count = sum(`Unattributed Activation Count`),
            Placement_Name_First_Postition = first(`Placement Name`),
            Creative_Name_First_Position = first(`Creative Name`),
            Funnel_First_Position = first(Funnel))


## Filter rows down to the Creative IDs of interest

MTA_Creative_ID <- MTA_Creative_ID %>% filter(C_Creative_ID %in% c(CID1, CID2))


## Write out the new csv

write_csv(MTA_Creative_ID, "/Users/keenanv/Desktop/DCO_DATA_with_Creative_ID.csv")