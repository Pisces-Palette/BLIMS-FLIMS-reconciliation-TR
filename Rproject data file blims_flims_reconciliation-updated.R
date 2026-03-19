#####################################################
# FLIMS/BLIMS Reconciliation
# By: Katey Rein & Taylor Rohlin 
#     DWR Quality Assurance Unit

#####################################################
### Load libraries and import data
#####################################################
library(readxl)
library(janitor)
library(here)
library(tidyverse)
library(data.table)
library(writexl)

#Pull in all data except lab file
lab_file <- "BLIMS - FLIMS Transfer (1).xlsx"

all_files <- list.files(pattern='*updated.xlsx', recursive = TRUE)
wdl_files <- setdiff(all_files, lab_file)
wdl.list <- lapply(wdl_files, read_excel)

#combine the wdl datasets
wdl1 <- rbindlist(wdl.list, fill = TRUE)
wdl1 <- clean_names(wdl1)

#read in crosswalk from lab
lab_list <- read_excel("BLIMS - FLIMS Transfer (1).xlsx")
lab_list <- clean_names(lab_list)

names(wdl1)
names(lab_list)

#####################################################
### Format and manipulate WDL and Lab data 
#####################################################

#remove unnecessary columns from both dataframes
wdl2 <- select(wdl1, collection_date, short_station_name, sample_code, station_number,data_owner, data_status, long_station_name, description)
lab_list1 <- select(lab_list, collection_date, station_name, flims_sample_i_ds, blims_sample_i_ds, flims_submittal_id,  blims_submittal_id)
#taylor removed these fields from the lab list: error_occurred_skipped,  blims_flims_complete, key, awaiting_field_spreadsheet, requires_lab_update

#rename columns in lab's list to match what's in wdl, adjust those in wdl for ease of use
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
wdl4 <- wdl3 %>%  distinct(collection_date, short_station_wdl, wdl_sample_id, station_num_wdl, data_owner, data_status_wdl, long_station_wdl, description)
                           
#Check if there are any duplicates using just wdl_sample_id as unique identifier. shouldn't be any
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

#format collection date column of wdl and lab_list
wdl5$collection_date <- as.Date(wdl5$collection_date, format = "%m/%d/%Y %H:%M")
#lab_list4$collection_date <- as.Date(lab_list4$collection_date, format = "%m/%d/%Y %H:%M")

#create a long format of lab_list4 with both sample ID types in one column
lab_long <- lab_list4 %>%
  select(collection_date, station_lab, blims_sample_i_ds, flims_sample_i_ds) %>%
  pivot_longer(
    cols = c(blims_sample_i_ds, flims_sample_i_ds),
    names_to = "id_type",
    values_to = "lab_sample_id"
  )


#####################################################
###  Analyses
#####################################################

#look at which data have both flims and blims IDs missing
na_both_ids <- lab_list4 %>%
  filter(is.na(blims_sample_i_ds) & is.na(flims_sample_i_ds))
#none

#look at which data have both a blims and flims id
both_ids <- lab_list4 %>%
  filter(blims_sample_i_ds != "NA" & flims_sample_i_ds != "NA")
#1104 samples

#look for any repeats of IDs between blims and flims in lab list
any(lab_list4[[3]] %in% lab_list4[[4]])
#false, so no repeats

#join wdl5 with lab_list4 based on matching sample IDs 
join1 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "blims_sample_i_ds")) %>%
  rename('wdl_blims_id' = 'wdl_sample_id')
  #934 samples with matching BLIMS IDs

join2 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "flims_sample_i_ds"))%>%
  rename('wdl_flims_id' = 'wdl_sample_id',
         wdl_collection_date = collection_date.x,
         lab_collection_date = collection_date.y)
  #46 samples with matching FLIMS IDs

result <- bind_rows(join1, join2)
#980 samples between wdl and Lab that have a matching sample ID (FLIMS or BLIMS).

#join dfs based on sample ids
sample_id_matches <- wdl5 %>%
  inner_join(lab_long, by = c("wdl_sample_id" = "lab_sample_id")) %>%
  rename(wdl_collection_date = collection_date.x,
         lab_collection_date = collection_date.y)
#980 same as "result" but kept this code to assist with analyses below. id_type column denotes the sample ID type present in lab data

#filter ID matches to discern when collection dates differ
non_matching_dates <- sample_id_matches %>%
  filter(wdl_collection_date != lab_collection_date) 
  #669 samples that share sample IDs but have different collection dates between wdl and lab. id_type column denotes the sample ID type present in lab data         

#looking at the result df, there are instances where the sample collection dates do not match between the wdl5 and lab_list4
join3 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "blims_sample_i_ds", "collection_date" = "collection_date")) %>%
  rename('wdl_blims_id' = 'wdl_sample_id')
  #301 samples that share a BLIMS ID and have the same collection date

join4 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "flims_sample_i_ds", "collection_date" = "collection_date"))%>%
  rename('wdl_flims_id' = 'wdl_sample_id')
  #10 samples that share a FLIMS ID and have the same collection date

result2 <- bind_rows(join3, join4)
#311 samples where sample IDs AND collection dates match.

#join wdl5 with lab_long on sample ID and collection date, see what samples have matching collection dates AND sample IDs
matching_samples <- wdl5 %>%
  inner_join(lab_long, by = c("wdl_sample_id" = "lab_sample_id", "collection_date" = "collection_date"))
#311 samples, same as "result2", but formatted differently. id_type column denotes what type of sample ID from the lab data matches.

#records in wdl5 with no matching sample ID in lab_long
non_matching_ids <- wdl5 %>%
  anti_join(lab_long, by = c("wdl_sample_id" = "lab_sample_id"))
#453 samples in wdl with no matching sample IDs in lab

#make a df of the matching IDs. same as "sample_id_matches" and "result", but formatted a little different.      
matching <- bind_rows(
  lab_list4 %>%
    semi_join(
      sample_id_matches %>% filter(id_type == "flims_sample_i_ds"),
      by = c("flims_sample_i_ds" = "wdl_sample_id")
    ),
  lab_list4 %>%
    semi_join(
      sample_id_matches %>% filter(id_type == "blims_sample_i_ds"),
      by = c("blims_sample_i_ds" = "wdl_sample_id")
    )
) %>%
  distinct()

#create new lab list that excludes known matches between wdl and lab. should contain lab samples that don't have a matching sample ID in wdl
lab_list5 <- lab_list4 %>%
  filter(
    !flims_submittal_id %in% matching$flims_submittal_id,
    !blims_submittal_id %in% matching$blims_submittal_id
  )
#remove example row
lab_list5 <- lab_list5 %>% filter(station_lab != "Example")
#122 samples in lab that dont have matching IDs in wdl

#filter out sample records that list year 2025 in the sample IDs in Lab
#first for blims IDs
year1 <- lab_list4 %>%
  mutate(
    mmyy = str_extract(blims_sample_i_ds, "^[A-Za-z]+(\\d{4})") %>%
      str_sub(-4, -1),   
    
    year = str_sub(mmyy, 3, 4)  
  ) %>%
  filter(year == "25")
#just the example file (ignore)

#second is for flims IDs
year_2025_lab <- lab_list4 %>%
  mutate(
    mmyy = str_extract(flims_sample_i_ds, "^[A-Za-z]+(\\d{4})") %>%
      str_sub(-4, -1),   
    
    year = str_sub(mmyy, 3, 4)  
  ) %>%
  filter(year == "25")
#1057 samples where flims ID uses year 25 for 2024 data

#filter out sample records that list year 2025 in the sample IDs in WDL
year_2025_wdl <- wdl5 %>%
  mutate(
    mmyy = str_extract(wdl_sample_id, "^[A-Za-z]+(\\d{4})") %>%
      str_sub(-4, -1),   
    
    year = str_sub(mmyy, 3, 4)  
  ) %>%
  filter(year == "25")
#4 samples where sample ID uses year 25 for 2024 data.

### Final Findings, with data pulled 01/15-16/2026:
#########################################################################################*
#*  46 samples in wdl that use flims IDs that match the lab data.    
#*  934 samples in wdl that use blims IDs that match the lab data 
#*  A total of 980 samples in wdl that match lab based on sample ID
#  
#*  669 samples that share sample IDs but have different collection dates
#  
#*  301 samples in wdl match BLIMS IDs and collection dates in lab
#*  10 samples in wdl match FLIMS IDs and collection dates in lab
#*  A total of 311 samples in wdl that match sample IDs and collection dates in lab
#  
#*  lab_list5 contains 122 lab samples that dont have matching flims or blims IDs in wdl 
#  
#*  non_matching_ids contains 453 wdl samples that dont have matching sample IDs in lab   
#########################################################################################*

#generate lists of project specific sample numbers for the various queries 
prefix_table <- lab_list5 %>% #This one will have some non standard project names due to metadata in sample ID column 
  mutate(prefix = str_extract(blims_sample_i_ds, "^[A-Za-z]+")) %>%
  count(prefix, name = "sample_count")


###Q for Taylor 1/16/26 - I'm not seeing a df named wdl_no_lab in this final code, so cannot run command starting on line 258

prefix_table_wdl <- wdl_no_lab %>%
  mutate(prefix = str_extract(wdl_sample_id, "^[A-Za-z]+")) %>%
  count(prefix, name = "sample_count")

prefix_table_match_no_date <- non_matching_dates %>%
  mutate(prefix = str_extract(wdl_sample_id, "^[A-Za-z]+")) %>%
  count(prefix, name = "sample_count")

prefix_table_match <- sample_id_matches %>%
  mutate(prefix = str_extract(lab_sample_id, "^[A-Za-z]+")) %>%
  count(prefix, name = "sample_count")

prefix_table_correct <- matching_samples %>%
  mutate(prefix = str_extract(wdl_sample_id, "^[A-Za-z]+")) %>%
  count(prefix, name = "sample_count")

#############################################
#### exporting data 
#############################################
write.csv(sample_id_matches, "Matched IDs (no date)_updated.csv", row.names = FALSE) #980 samples
write.csv(non_matching_dates, " Matched IDs (date difference)_updated.csv", row.names = FALSE) #669 samples
write.csv(non_matching_ids, "WDL list unmatched to lab list_updated.csv", row.names = FALSE) #453 samples
write.csv(matching_samples, "Matched IDs (with date)_updated.csv", row.names = FALSE) #311 samples
write.csv(lab_list5, "Lab list unmatched to WDL list_updated.csv", row.names = FALSE) #122 samples
write.csv(year_2025_lab, "year_2025_lab_updated.csv", row.names = FALSE) #1057 samples
write.csv(year_2025_wdl, "WDL list 2025 ID_updated.csv", row.names = FALSE) #4 samples
write.csv(join2, "Reported to WDL with FLIMS ID not BLIMS ID_updated.csv", row.names = FALSE) #46 samples

