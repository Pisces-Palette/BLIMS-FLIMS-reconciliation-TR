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

#removed these fields from the lab list: error_occurred_skipped,  blims_flims_complete, key, awaiting_field_spreadsheet, requires_lab_update

lab_list1 <- select(lab_list, station_name,  collection_date, flims_submittal_id, flims_sample_i_ds, blims_submittal_id, blims_sample_i_ds)

view(unique(lab_list1$station_name))

#lab's ID of the station is not the official short/long/number for the station submitted by field group, so will have to deal with this later to match up

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

#taylor noted this out since he removed these fields earlier  
#lab_list2 <- mutate(lab_list2, 
                 # error_occurred_skipped = as.character(error_occurred_skipped),
                 # blims_flims_complete = as.character(blims_flims_complete),
                 # key = as.character(key))

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
##Note: 1/2/26 Katey updated this since columns had changed order
any(lab_list4[[4]] %in% lab_list4[[6]])
#false, so no repeats

#format collection date column of wdl and lab_list
wdl5$collection_date <- as.Date(wdl5$collection_date, format = "%m/%d/%Y %H:%M")
lab_list4$collection_date <- as.Date(lab_list4$collection_date, format = "%m/%d/%Y %H:%M")


#join wdl5 with lab_list4 based on matching sample IDs 
join1 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "blims_sample_i_ds")) %>%
  rename('wdl_blims_id' = 'wdl_sample_id')

join2 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "flims_sample_i_ds"))%>%
  rename('wdl_flims_id' = 'wdl_sample_id')

wdl_lab_list_join <- bind_rows(join1, join2)
#752 samples

##there are 706 samples from the lab's list that are on WDL with a sample ID that is a BLIMS ID 
##there are 46 samples from the lab's list that are on WDL with a sample ID that is a FLIMS ID 

#looking at the result df, there are instances where the sample collection dates do not match between the wdl5 and lab_list4
join3 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "blims_sample_i_ds", "collection_date" = "collection_date")) %>%
  rename('wdl_blims_id' = 'wdl_sample_id')

join4 <- wdl5 %>%
  inner_join(lab_list4, by = c("wdl_sample_id" = "flims_sample_i_ds", "collection_date" = "collection_date"))%>%
  rename('wdl_flims_id' = 'wdl_sample_id')

wdl_lab_list_join_datematch <- bind_rows(join3, join4)
#204 samples, so 548 samples where the lab's list and wdl export disagree on the sample collection date

#filter for samples that have mismatched collection dates, rename collection date fields for traceability, id_type field defines what lab_list FLIMS/BLIMS ID matched w/the wdl_sample_id
non_matching_dates <- wdl_lab_list_join %>%
  filter(collection_date.x != collection_date.y) %>%
  rename(wdl_collection_date = collection_date.x,
         lab_collection_date = collection_date.y)
#548

# Reshape lab_list4 to long format to combine IDs for antijoin

lab_long <- lab_list4 %>%
  select(station_lab, blims_sample_i_ds, flims_sample_i_ds) %>%
  pivot_longer(cols = c(blims_sample_i_ds, flims_sample_i_ds),
               names_to = "id_type",
               values_to = "lab_sample_id")

non_matching_ids_wdl <- wdl5 %>%
  anti_join(lab_long, by = c("wdl_sample_id" = "lab_sample_id"))
#270


####################################################################
# further analysis/circling back 
####################################################################
# Questions:
#(1) How many, and which, samples are on wdl with flims
#    vs blims ID that have both per lab's list 

#(2) How many and which are on wdl and not in lab list

#(3) How many and which are not on wdl but on lab list
####################################################################

#reduce lab_long to exclude records that have NA in sample ID field
lab_long2 <- lab_long %>%
  filter(
    !is.na(lab_sample_id))
#21 records excluded

#remove specific records from the dataframe
##Katey's note 1/2/26: need to come back to these parallel FLIMS submittal ID IDs
lab_long3 <- lab_long2 %>%
  filter(
    !lab_sample_id %in% c(
      "Parallel FLIMS Submittal from 2024",
      "Cannot Print COC",
      "Error: ① Occurred during Acception; submittal went through and samples are appearing in backlog",
      "Error: Duplicate sample was created with BLIMS ID; Renamed duplicate to OM0824B00022-DUP",
      "Error: ②Occurred during <Proceed>; Hit <Proceed> again; Submittal went through and samples are in backlog",
      "Error: ③Occurred after submitting sample condition; submittal went through",
      "Received by Erik Senter",
      "Parallel FLIMS Submittal from 2024; New FLIMS E0725B0030",
      "Parallel FLIMS Submittal from 2024; New FLIMS E0725B0031",
      "Error: ⑤ Occurred during  <Proceed>"
    )
  )
#21 records

#reduce number of fields present in the wdl df
##Katey's note 1/2/26: let's keep these in for now
#wdl6 <- wdl5 %>%
  select(collection_date, short_station_wdl, station_num_wdl, wdl_sample_id, )


#confirm which samples between wdl and lab have flims IDs
flims_samples <- wdl6 %>%
  inner_join(
    lab_long3 %>% filter(id_type == "flims_sample_i_ds"),
    by = c("wdl_sample_id" = "lab_sample_id")
  )
#46 (confirmed earlier)

#confirm which samples between wdl and lab have blims IDs
blims_samples <- wdl6 %>%
  inner_join(
    lab_long3 %>% filter(id_type == "blims_sample_i_ds"),
    by = c("wdl_sample_id" = "lab_sample_id")
  )
#706 (confirmed earlier)


##Q for Taylor from Katey 1/2/26: what is all this and how come it's been removed from run code?

#full join to see full list of stations between both that have matching flims ID (WDL df is the base)
#flims_all <- wdl6 %>%
  #left_join(
   # lab_long3 %>% filter(id_type == "flims_sample_i_ds"),
  #  by = c("wdl_sample_id" = "lab_sample_id")
 # )
    #review number of rows with NA for ID type
  #   sum(is.na(flims_all$id_type))
     #976 records in WDL with NA for ID type. 46 records with FLIMS ID (confirmed earlier) 

#full join to see full list of stations between both that have matching flims ID (WDL df is the base)
#blims_all <- wdl6 %>%
  #left_join(
    #lab_long3 %>% filter(id_type == "blims_sample_i_ds"),
   # by = c("wdl_sample_id" = "lab_sample_id")
  #)

      #review number of rows with NA for ID type
      #sum(is.na(blims_all$id_type))
      #316 records in WDL with NA for ID type. 706 with BLIMS ID (confirmed) 


      
#confirm which samples between wdl and lab have matching IDs regardless of collection date. ID type listed. lab df is base.
sample_id_matches <- lab_long3 %>%
  rename(collection_date_lab = collection_date) %>%
  inner_join(
    wdl6 %>% 
      rename(collection_date_wdl = collection_date),
    by = c("lab_sample_id" = "wdl_sample_id")
  )
#752 records, confirming our findings 
     
     #confirm "non_matching_dates" df.
      match_id_not_date <- sample_id_matches %>%
        filter(collection_date_lab != collection_date_wdl)
        #Yes, 548 records that match ID but not collection date.same as non_matching_dates df
     
      ##################################################################### *
      #* so 46 samples in WDL that use flims IDs that match the lab data    *
      #*  and 706 samples in wdl that use blims IDs that match the lab data *
      #*   A total of 752 samples that match based on ID,  flims or blims   *
      ##################################################################### *      

#make a copy of the sample_id_matches df but with the full lab list data.      
matching <- bind_rows(
    lab_list4 %>%
        semi_join(sample_id_matches, by = c("flims_sample_i_ds" = "lab_sample_id")),
        lab_list4 %>%
          semi_join(sample_id_matches, by = c("blims_sample_i_ds" = "lab_sample_id"))
            ) %>%
             distinct()

#create new lab list that excludes known matches between wdl and lab
lab_list5 <- lab_list4 %>%
  filter(
    !flims_submittal_id %in% matching$flims_submittal_id,
    !blims_submittal_id %in% matching$blims_submittal_id
  )
#remove NA IDs & example row
lab_list5 <- lab_list5 %>% filter(!is.na(flims_sample_i_ds))
lab_list5 <- lab_list5 %>% filter(station_lab != "Example")

      #########################################################################################*
  #(3)#* lab_list5 contains 348 lab records that dont have a matching flims or blims ID in wdl *
      #########################################################################################*

           
#records in wdl6 that dont appear in lab_long3 based on sample IDs
wdl_no_lab <- wdl6 %>%
  anti_join(lab_long3, by = c("wdl_sample_id" = "lab_sample_id"))
#270 records (same as "non_matching_ids") in wdl that dont have a matching ID in Lab

      ##########################################################################################*
  #(2)#* non_matching_ids contains 270 wdl records that dont have a matching sample ID in lab's list   *
      ##########################################################################################*

#ensure collection date field is formatted the same  
wdl6$collection_date <- as.Date(wdl6$collection_date)
lab_long3$collection_date <- as.Date(lab_long3$collection_date)

#join wdl5 with lab_long on sample ID and collection date, see what samples have matching collection dates and sample IDs
matching_samples <- wdl6 %>%
  inner_join(lab_long3, by = c("wdl_sample_id" = "lab_sample_id", "collection_date" = "collection_date"))
##204 records

      ###################################################################################*
#(1?) #* matching_samples contains 204 records that have a matching flims or blims ID    *
      #* AND have matching collection dates between both wdl and lab                     *
      ###################################################################################*

#verify the records in matching_samples are present in the sample_id_matches df
all(matching_samples$wdl_sample_id %in% sample_id_matches$lab_sample_id)
#true


##Q for Taylor from KR 1/2/26: I'm curious about this query related to stations and what insight this might get us
#look for duplicates in wdl (station names repeating on the same date) 
wdl_dup <- wdl6 %>%
  add_count(short_station_wdl, collection_date, name = "n") %>%
  filter(n > 1)

#create lab station list
lab_stations <- lab_long3 %>%
  distinct(station_lab)
##many stations not present in WDL (NR, BL, N10, N11, NC11).
## SM data for Nov & Dec not present in Lab but present in WDL

#create wdl station list
wdl_stations <- wdl6 %>%
  distinct(short_station_wdl)














########################
# exporting dataframes #
########################
write.csv(sample_id_matches, "sample_id_matches.csv", row.names = FALSE)
write.csv(non_matching_dates, "non_matching_dates.csv", row.names = FALSE)
write.csv(non_matching_ids, "non_matching_ids.csv", row.names = FALSE)
write.csv(matching_samples, "matching_samples.csv", row.names = FALSE)
write.csv(lab_list5, "lab_list5.csv", row.names = FALSE)


