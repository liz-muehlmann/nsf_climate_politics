### file description ###########################################################
##                                                                            ##
## This file handles the processing steps necessary for the local view data   ##
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ## 
##                                                                            ##  
##                        last save: 2025-03-08                               ##  
## Output:                                                                    ##
##      /LocalView/data/modified/individual_datasets/lvClean_transcript.rdata 
##      /LocalView/data/modified/individual_datasets/lvClean_noTranscript.rdata
##      /LocalView/data/modified/individual_datasets/lv_countyYear_noNA.rdata  
##      /LocalView/data/modified/individual_datasets/lv_countyYear_withNA.rdata
##                                                                            ##    
## ########################################################################## ## 

#   ____________________________________________________________________________
#   load packages                                                           ####
#       Note: the tidyverse() package is loaded below through the geographic_processing.r file

library(arrow)                                     # open and work with parquet format (Local View)
library(readtext)                                  # read filepaths (Local View)

#   ____________________________________________________________________________
#   state and county information is necessary for aggregating to            ####
#   the county-year level processing the geography data is done in the 
#   /processing_scripts/geography.r file
#   this will also load the preliminaries file at /processing_scripts/prelims.r

source("./code/processing/geography.r")

#   ____________________________________________________________________________
#   local view                                                              ####

##  ............................................................................
##  load raw data: n = 153,452                                              ####
lvRaw <- open_dataset("./data/original/Local_View/")  
lvRaw <- Scanner$create(lvRaw)
lvRaw <- lvRaw$ToTable()
lvRaw <- as.data.frame(lvRaw) 

##  ............................................................................
##  create sample: n = 103,379                                              ####
##      years: 2010+ 
##      discard: ACS columns, original caption columns, and rows with no 
##      caption available five places use incorrect fips and are manually fixed.

lv <- lvRaw %>% 
    rename(place_fips = st_fips,
           meeting_type = place_govt) %>% 
    separate(meeting_date, into = c("transcript_year", 
                                    "transcript_month", 
                                    "transcript_day"), sep = "-") %>% 
    filter(transcript_year >= 2010 &                                            # n = 715
          !(caption_text_clean == "<No caption available>") &                   # n = 49,640
          state_name != "Alaska") %>%                                           # n = 134
    mutate(place_fips = ifelse(place_fips == "5151650", "5135000", place_fips), # hampton city, VA - 161
           place_fips = ifelse(place_fips == "5151710", "5157000", place_fips), # norfolk city, VA - 6 
           place_fips = ifelse(place_fips == "5151730", "5161832", place_fips), # petersburg city, VA - 9
           place_fips = ifelse(place_fips == "2501325", "2501370", place_fips), # amherst town, MA - 413
           place_fips = ifelse(place_fips == "2527100", "2527060", place_fips), # greenfield, MA (incorrect place fips) - 496
           place_fips = ifelse(place_name == "Gadsden city", "0128696",         # does not have fips - 155
                               place_fips),
           meeting_date = make_date(transcript_year, transcript_month, transcript_day)) %>%                                 
    rename(transcript_id = vid_id) %>% 
    select(-starts_with(c("acs_", "channel", "vid_")), -caption_text)

##  ............................................................................
##  merge lv with county, county subdivision, and place data                ####

## 9,434 transcripts use two digit state + five digit county fips as place fips
lvCounty <- lv %>% 
    filter(grepl("County", place_name)) %>% 
    mutate(stcounty_fips = str_sub(place_fips, 3, 7),
           county_name = place_name) %>% 
    createFips()

## 93,945 transcripts use place (88,023) or county subdivision data (5,922)
lvPlace <- lv %>% 
    filter(!grepl("County", place_name)) %>% 
    left_join(ipcdp, by = "place_fips")

# 5,922 transcripts use county-subdivision information
lvCountySub <- lvPlace %>% 
    filter(is.na(stcounty_fips)) %>% 
    select(-county_name, -stcounty_fips) %>%
    left_join(countySub, by = "place_fips")

## 88,023 transcripts use correct place data
lvPlace <- lvPlace %>% 
    filter(!is.na(stcounty_fips)) 

## add county information to local view data
lvCounty <- ipcdp %>% 
    right_join(lvCounty, multiple = "any")

## combine and save clean version of Local View data before further processing
lvClean_transcript <- rbind(lvPlace, lvCounty, lvCountySub) %>% 
    select(-state_fips, -county_fips) %>% 
    createFips() %>% 
    distinct(transcript_id, .keep_all = TRUE)

# save(lvClean_transcript, file = "./data/modified/lv_clean_transcript.rdata")

#   ____________________________________________________________________________
#   calculate climate change use by meeting                                 ####

lvClean_noTranscript <- lvClean_transcript %>% 
    mutate(caption_text_clean = str_to_lower(caption_text_clean),               # convert caption_text_clean column to lower case
           n_ccMentions = str_count(caption_text_clean, "climate change"),      # raw number of climate change mentions
           n_gwMentions = str_count(caption_text_clean, "global warming"),      # raw number of global warming mentions
           n_ccgwMentions = ifelse(n_ccMentions == 0 & n_gwMentions == 0,       # total number of CC & GW mentions; 0 if neither CC or GW mentioned
                                  0, (n_ccMentions + n_gwMentions)), 
           ccBinary = ifelse(n_ccMentions > 0, 1, 0),                           # binary indicator; 1 = CC mention, 0 = no CC mention
           gwBinary = ifelse(n_gwMentions > 0, 1, 0),                           # binary indicator; 1 = GW mention, 0 = no GW mention 
           ccgwBinary = ifelse(n_ccgwMentions > 0, 1, 0)) %>%                   # binary indicator; 1 = CC or GW mention, 0 = no mention
    select(-caption_text_clean) %>% 
    distinct(.keep_all = TRUE)

# save(lvClean_noTranscript, file = "./data/modified/lv_clean_noTranscript.rdata")

#   ____________________________________________________________________________
#   aggregate local view data to the county level (n 3,668)                 ####

lv_countyLevel_noNA <- lvClean_noTranscript %>% 
    group_by(transcript_year, stcounty_fips) %>% 
    mutate(n_transcripts = n(),                                                 # number of transcripts in a county-year
           n_meetingTypes = n_distinct(meeting_type),                           # number of meeting types in a county-year
           n_ccMentions = sum(n_ccMentions),                                    # number of climate change meetings in a county-year
           n_gwMentions = sum(n_gwMentions),                                    # number of global warming meetings in a county-year
           n_ccgwMentions = sum(n_ccgwMentions),                                # number of CC or GW meetings in a county-year
           n_scriptCC = sum(ccBinary),                                          # number of transcripts with at least one mention of climate change
           n_scriptGW = sum(gwBinary),                                          # number of transcripts with at least one mention of global warming
           n_scriptCCGW = sum(ccgwBinary),                                      # number of transcripts with at least one mention of CC or GW
           ccBinary = ifelse(n_ccMentions > 0, 1, 0),                           # binary indicator; 1 = county had at least one mention of CC in a county-year
           gwBinary = ifelse(n_gwMentions > 0, 1, 0),                           # binary indicator; 1 = county had at least one mention of GW in a county-year
           ccgwBinary = ifelse(n_ccgwMentions > 0, 1, 0),                       # binary indicator; 1 = county had at least one mention of CC or GW in that year
           prop_ccMentions = n_ccMentions/n_transcripts,                        # proportion of climate change mentions in a county-year
           prop_gwMentions = n_gwMentions/n_transcripts,                        # proportion of global warming mentions in a county-year
           prop_ccgwMentions = n_ccgwMentions/n_transcripts,                    # proportion of CC or GW mentions in a county-year 
           prop_scriptCC = n_scriptCC/n_transcripts,
           prop_scriptGW = n_scriptGW/n_transcripts,
           prop_scriptCCGW = n_scriptCCGW/n_transcripts) %>%                    # proportion of transcripts with at least one mention CC or GW mention
    distinct(transcript_year, stcounty_fips, .keep_all = TRUE) %>% 
    ungroup() %>% 
    select(transcript_year, state_name, state_fips, county_name, county_fips,
           stcounty_fips, ccBinary, gwBinary, ccgwBinary, meeting_date,
           starts_with(c("n_", "prop_"))) %>% 
    left_join(counties)

# save(lv_countyLevel_noNA, file = "./data/modified/lv_countyLevel_noNA.rdata")

#   ____________________________________________________________________________
#   merge with all counties                                                 ####
#     NA indicates there was no transcript in that county for that year 
#     (n = 43,512)

lv_countyLevel_withNA <- list()
for(y in unique(lv_countyLevel_noNA$transcript_year)) {
    lv_countyLevel_withNA[[y]] <- lv_countyLevel_noNA %>%
        filter(transcript_year == y) %>%
        select(-transcript_year) %>%
        right_join(counties) %>%
        mutate(transcript_year = y,
               has_transcript = ifelse(is.na(n_meetingTypes), 0, 1))}

lv_countyLevel_withNA <- bind_rows(lv_countyLevel_withNA)

# save(lv_countyLevel_withNA, file = "./data/modified/lv_countyLevel_withNA.rdata")

##  ............................................................................
##  lv Sample                                                               ####

sample_lvTranscript <- lvClean_noTranscript %>% slice_sample(n = 50, replace = FALSE)
# write.csv(sample_lvTranscript, "./results/samples/lvTranscript_sample.csv", row.names = FALSE)

sample_lvCounty <- lv_countyLevel_noNA %>% slice_sample(n = 50, replace = FALSE)
# write.csv(sample_lvCounty, "./results/samples/lvCounty_sample.csv", row.names = FALSE)
