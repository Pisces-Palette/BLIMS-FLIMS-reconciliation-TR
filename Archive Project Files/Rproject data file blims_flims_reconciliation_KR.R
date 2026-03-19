library(readxl)
library(janitor)
library(here)
library(dplyr)
library(tidyverse)
library(data.table)
library(writexl)

#Pull in all data except lab file
lab_file <- "BLIMS - FLIMS Transfer (1).xlsx"

all_files <- list.files(pattern='*.xlsx', recursive = TRUE)
wdl_files <- setdiff(all_files, lab_file)
wdl.list <- lapply(wdl_files, read_excel)

#combine the datasets
wdl1 <- rbindlist(wdl.list, fill = TRUE)

wdl1 <- clean_names(wdl1)

#read in crosswalk from lab
lab_list <- read_excel("BLIMS - FLIMS Transfer (1).xlsx")

lab_list <- clean_names(lab_list)

names(wdl1)
names(lab_list)

#remove unnecessary columns from both dataframes

wdl2 <- select(wdl1, collection_date, data_owner, data_status, long_station_name, short_station_name, station_number, description, sample_code)

lab_list1 <- select(lab_list, station_name, error_occurred_skipped, collection_date, flims_submittal_id, flims_sample_i_ds, blims_submittal_id, blims_flims_complete, key,
                    awaiting_field_spreadsheet, blims_sample_i_ds, requires_lab_update)

#rename columns in lab's list to match what's in wdl, adjust those in wdl for ease of use

view(unique(lab_list1$station_name))
#lab's ID of the station is not the official short/long/number for the station submitted by field group, so will have to deal with this later to match up

lab_list2 <- lab_list1 %>%
  rename('station_lab' = 'station_name')
    
wdl3 <- wdl2 %>%
  rename(
    'data_status_wdl' = 'data_status',
    'long_station_wdl' = 'long_station_name',
    'short_station_wdl' = 'short_station_name',
    'station_num_wdl' = 'station_number',
    'wdl_sample_id' = 'sample_code')

#prepare to combine

str(wdl3)
str(lab_list2)

#wdl dataframe has data broken down to analyte level unlike lab dataframe, so need to select just row for each, remove by selecting the first one

wdl4 <- wdl3 %>%  distinct(collection_date, wdl_sample_id, data_owner,data_status_wdl, long_station_wdl,short_station_wdl,station_num_wdl,description)

# Check if there are any duplicates using just wdl_sample_id as unique identifier. shouldn't be any
wdl4 %>% count(wdl_sample_id) %>% filter(n > 1)
#two cases

#take a closer look
dups <- wdl4 %>%
  count(wdl_sample_id) %>%
  filter(n>1) %>%
  select(-n) %>%
  left_join(wdl4, by = join_by(wdl_sample_id))
#these are NA columns for the sample ID and can be removed

wdl5 <- wdl4 %>% 
  drop_na(wdl_sample_id)

  
lab_list2 <- mutate(lab_list2, 
                  error_occurred_skipped = as.character(error_occurred_skipped),
                  blims_flims_complete = as.character(blims_flims_complete),
                  key = as.character(key))

#because lab list is nested, fill out NA values with submittal-specific metadata
lab_list3 <- lab_list2 %>%
  fill(collection_date, 
       blims_submittal_id,
       flims_submittal_id,.direction = "down")

#remove rows without any sample IDs
lab_list4 <- filter(lab_list3, blims_sample_i_ds != "NA" | flims_sample_i_ds != "NA") 
#there are 286 rows where NA values in all of these columns, verify
sum(is.na(lab_list3$blims_sample_i_ds) & is.na(lab_list3$flims_sample_i_ds))
#286, correct

#look at which data have these missing
na_both_ids <- lab_list3 %>%
  filter(is.na(blims_sample_i_ds) & is.na(flims_sample_i_ds))
#from first submittal ID row due to nesting

#look at which data have both a blims and flims id
both_ids <- lab_list3 %>%
  filter(blims_sample_i_ds != "NA" & flims_sample_i_ds != "NA")
#1104

#look for any repeats of IDs between blims and flims in lab list
any(lab_list4[[5]] %in% lab_list4[[10]])
#false, so no repeats

#format collection date column of wdl and lab_list
wdl5$collection_date <- as.Date(wdl5$collection_date, format = "%m/%d/%Y %H:%M")
lab_list4$collection_date <- as.Date(lab_list4$collection_date, format = "%m/%d/%Y %H:%M")

#make a new df with records in wdl5 but not in lab_list4 based on collection_date
dates_wdl <- anti_join(wdl5, lab_list4, by = "collection_date")
#276

#make a new df with records in lab_list1v3 but not in wdl5 based on collection_date
dates_lab <- anti_join(lab_list4, wdl5, by = "collection_date")
#178

#check if any of the records in the lab data that don't have collecion dates in wdl have matching flims/blims IDs in wdl$wdl_sample_id
lab_blims_matches <- dates_lab %>% filter(blims_sample_i_ds %in% wdl5$wdl_sample_id) #78
lab_flims_matches <- dates_lab %>% filter(flims_sample_i_ds %in% wdl5$wdl_sample_id) #0

#check if any of the records in the wdl data that don't have collecion dates in lab have matching flims/blims IDs in lab_list1v3$flims_sample_i_ds/lab_list1v3$blims_sample_i_ds
wdl_blims_matches <- dates_wdl %>% filter(wdl_sample_id %in% lab_list4$blims_sample_i_ds) #84
wdl_flims_matches <- dates_wdl %>% filter(wdl_sample_id %in% lab_list4$flims_sample_i_ds) #0

#join wdl5 with lab_list4 based on matching sample IDs 
join1 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "blims_sample_i_ds")) %>%
  rename('wdl_blims_id' = 'wdl_sample_id')

join2 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "flims_sample_i_ds"))%>%
  rename('wdl_flims_id' = 'wdl_sample_id')

result <- bind_rows(join1, join2)
#752 samples

#looking at the result df, there are instances where the sample collection dates do not match between the wdl5 and lab_list4
join3 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "blims_sample_i_ds", "collection_date" = "collection_date")) %>%
  rename('wdl_blims_id' = 'wdl_sample_id')

join4 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "flims_sample_i_ds", "collection_date" = "collection_date"))%>%
  rename('wdl_flims_id' = 'wdl_sample_id')

result2 <- bind_rows(join3, join4)
#204 samples, so 548 samples where the lab's list and wdl export disagree on the sample collection date

#filter for samples that have mismatched collection dates, rename collection date fields for traceability, id_type field defines what lab_list FLIMS/BLIMS ID matched w/the wdl_sample_id
non_matching_dates <- result %>%
  filter(collection_date.x != collection_date.y) %>%
  rename(wdl_collection_date = collection_date.x,
         lab_collection_date = collection_date.y)
#548


#outstanding question: how many, and which, samples are on wdl with flims vs blims ID that have both per lab's list
both_ids_vs_wdl <- wdl5 %>%
  inner_join(both_ids, by = c("wdl_sample_id" = "lab_sample_id"))
##having issues here


#how many and which are on wdl and not in lab list

#how many and which are not on wdl but on lab list



#join wdl5 with lab_long on sample ID and collection date, see what samples have matching collection dates and sample IDs
matching_samples <- wdl5 %>%
  inner_join(lab_long, by = c("wdl_sample_id" = "lab_sample_id", "collection_date" = "collection_date"))
#35 records

#see if any of the samples from check1 are present in the matching_samples df
check2 <- check1[check1$wdl_sample_id %in% matching_samples$wdl_sample_id, ]
#10

#join dfs based on sample ids
sample_id_matches <- wdl5 %>%
  inner_join(lab_long, by = c("wdl_sample_id" = "lab_sample_id"))
#752

#filter for samples that have mismatched collection dates, rename collection date fields for traceability, id_type field defines what lab_list FLIMS/BLIMS ID matched w/the wdl_sample_id
non_matching_dates <- sample_id_matches %>%
  filter(collection_date.x != collection_date.y) %>%
  rename(wdl_collection_date = collection_date.x,
         lab_collection_date = collection_date.y)
#548


