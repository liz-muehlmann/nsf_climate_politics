### file description ###########################################################
##                                                                            ##
## This file uses the complete clean local view data without transcripts to   ##
##      calculate the number of days between a FEMA disaster declaration and  ##
##      the meeting date                                                      ##
##                                                                            ##
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##
##              ./processing_scripts/local_view.r                             ##
##          FEMA (2010-2023)                                                  ##
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2        
##                                                                            ##
## Output:                                                                    ##
##      /LocalView/results/summaries/LargeFiles/nearestDeclaration_beforeMeeting.csv
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load preliminaries and data                                             ####

library(lubridate)                                             # work with dates

source("./code/processing/geography.r")    
load("./data/modified/local_view/lv_clean_noTranscript.rdata")
load("./data/modified/fema/fema_declarationLevel.rdata")

cdp_geo <- st_read("./data/original/2020_CountySubNational/tlgdb_2020_a_us_substategeo.gdb", layer = "Census_Designated_Place") %>% 
    select(GEOID, SHAPE) %>% 
    rename(place_fips = GEOID,
           place_shape = SHAPE)

place_geo <- st_read("./data/original/2020_CountySubNational/tlgdb_2020_a_us_substategeo.gdb", layer = "Incorporated_Place") %>% 
    select(GEOID, SHAPE) %>% 
    rename(place_fips = GEOID,
           place_shape = SHAPE) %>% 
    rbind(cdp_geo)

# n = 103,350
lv <- lvClean_noTranscript 

# n = 30,135
fema <- fema_declarationLevel %>%
    mutate(declaration_date = make_date(fema_year, fema_month, fema_day)) %>%
    select(femaDeclarationString, declaration_date, stcounty_fips, fema_nDecCountyYear, fema_nDecTypeCountyYear)

#   ____________________________________________________________________________
#   merge data                                                              ####

# all disasters, all years
# n = 1,182,916
## NOTE: interval() returns a positive value if the start happened before the end
## declaration (start) happened before meeting (end) the value is positive
lvFema <- left_join(lv, fema, by = "stcounty_fips", relationship = "many-to-many") %>%
    mutate(meeting_date = ymd(meeting_date),
           declaration_date = ymd(declaration_date),
           months_btwn_decMeeting = round(interval(declaration_date, meeting_date)/months(1), 2),
           time_btwn_decMeetingFactor = case_when(
               months_btwn_decMeeting <= 0 ~ "Declaration after meeting",
               months_btwn_decMeeting <= 1 ~ "1 month",
               months_btwn_decMeeting <= 2 ~ "2 months",
               months_btwn_decMeeting <= 6 ~ "3-6 months",
               months_btwn_decMeeting <= 9 ~ "7-9 months",
               months_btwn_decMeeting <= 12 ~ "10-12 months",
               months_btwn_decMeeting <= 24 ~ "2 years",
               months_btwn_decMeeting <= 36 ~ "3 years",
               months_btwn_decMeeting <= 48 ~ "4 years",
               months_btwn_decMeeting <= 60 ~ "5 years",
               months_btwn_decMeeting <= 72 ~ "6 years",
               months_btwn_decMeeting <= 84 ~ "7 years",
               months_btwn_decMeeting <= 96 ~ "8 years",
               months_btwn_decMeeting <= 108 ~ "9 years",
               months_btwn_decMeeting <= 120 ~ "10 years",
               months_btwn_decMeeting <= 132 ~ "11 years",
               months_btwn_decMeeting <= 144 ~ "12 years",
               months_btwn_decMeeting <= 156 ~ "13 years",
               months_btwn_decMeeting <= 168 ~ "14 years",
               months_btwn_decMeeting <= 180 ~ "15 years",
               TRUE ~ "No declaration between 2009-2022"),
           time_btwn_decMeetingFactor = as.factor(time_btwn_decMeetingFactor)) %>% 
    group_by(transcript_id) %>% 
    mutate(nDec_fiveYears = sum(time_btwn_decMeetingFactor %in%      
                                 c("1 month", "2 months", "3-6 months", "7-9 months", 
                                   "10-12 months", "2 years", "3 years", 
                                   "4 years", "5 years")),
        nDec_sixYears = sum(time_btwn_decMeetingFactor %in%      
                                c("6 years", "7 years", "8 years", "9 years", 
                                  "10 years", "11 years", "12 years", "13 years", 
                                  "14 years", "15 years")),
        nDec_oneYear = sum(time_btwn_decMeetingFactor %in%
                               c("1 month", "2 months", "3-6 months", "7-9 months", 
                                 "10-12 months")),
        nDec_twoYears = sum(time_btwn_decMeetingFactor == "2 years"),
        nDec_fiveYears = replace_na(nDec_fiveYears, 0),
        nDec_sixYears = replace_na(nDec_sixYears, 0)) %>% 
    ungroup()

# save(lvFema, file = "./data/modified/lvFema_all.rdata")

nearest_dec <- lvFema %>% 
    group_by(transcript_id) %>% 
    filter(months_btwn_decMeeting > 0) %>% 
    slice(which.min(months_btwn_decMeeting)) 

dec_after_meeting <- lvFema %>% 
    anti_join(nearest_dec, by = "transcript_id") %>% 
    distinct(transcript_id, .keep_all = TRUE)

# n = 103,350
lvFema_transcriptLevel <- bind_rows(nearest_dec, dec_after_meeting)

# save(lvFema_transcriptLevel, file = "./data/modified/lvFema_transcriptLevel.rdata")

#   ____________________________________________________________________________
#   summarize places that have only declarations six years ago              ####

lvFema_sixYears <- lvFema_transcriptLevel %>% 
    filter(time_btwn_decMeetingFactor %in%      
               c("6 years", "7 years", "8 years", "9 years", 
                 "10 years", "11 years", "12 years", "13 years", 
                 "14 years", "15 years"))

#   ____________________________________________________________________________
#   save a sample of lvFema transcript level data                           ####

lvFema_sample <- lvFema_transcriptLevel[sample(nrow(lvFema_transcriptLevel), 100), ]

# write.csv(lvFema_sample, "./LocalView/data/samples/lvFema_sample.csv", row.names = FALSE)

#   ____________________________________________________________________________
#   save a cross tab of number of transcripts in each time_btwn_decMeetingFactor

lvFema_crosstab <- lvFema_transcriptLevel %>% 
    group_by(time_btwn_decMeetingFactor) %>% 
    summarize(n_transcripts = n())

# write.csv(lvFema_crosstab, "./LocalView/data/samples/lvFema_crosstab.csv", row.names = FALSE)


#   ____________________________________________________________________________
#   add in geographic boundaries                                            ####

lvFema_counties <- countiesGeo %>% 
    select(stcounty_fips, geometry) %>% 
    right_join(lvFema_transcriptLevel, by = "stcounty_fips", relationship = "one-to-many") 

lvFema_counties_allYears <- lvFema_counties %>% 
    group_by(stcounty_fips) %>% 
    mutate(nTranscript_County_AllYears = n()) %>% 
    ungroup() %>% 
    distinct(stcounty_fips, .keep_all = TRUE)

# st_write(lvFema_counties, "./data/modified/lvFema_transcriptLevel_counties.gpkg")
# st_write(lvFema_counties_allYears, "./data/modified/lvFema_transcriptLevel_counties_allYears.gpkg")


lvFema_places <- lvFema_transcriptLevel %>% left_join(place_geo, by = "place_fips")

# st_write(lvFema_places, "./data/modified/lvFema_transcriptLevel_places.gpkg")




























