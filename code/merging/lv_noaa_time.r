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
##          NOAA (2009-2023)                                                  ##
##              https://www.ncdc.noaa.gov/stormevents/ftp.jsp                 ##        
##                                                                            ##
## Output:                                                                    ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load preliminaries and data                                             ####

library(lubridate)                                             # work with dates

source("./code/processing/geography.r")    
load("./data/modified/local_view/lv_clean_noTranscript.rdata")
load("./data/modified/noaa/noaa_episodeCountyLevel.rdata")

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
lv <-  lvClean_noTranscript 

noaa <- noaa_episodeCountyLevel %>% 
    select(-state_fips)

# all episodes, all years
# n = 1,540,042
## NOTE: interval() returns a positive value if the start happened before the end
## declaration (start) happened before meeting (end) the value is positive
lvNoaa <- left_join(lv, noaa, by = "stcounty_fips", relationship = "many-to-many") %>% 
    mutate(meeting_date = ymd(meeting_date),
           episode_date = ymd(noaa_episode_beginDate),
           months_btwn_episodeMeeting = round(interval(noaa_episode_beginDate, meeting_date)/months(1), 2),
           time_btwn_episodeMeetingFactor = case_when(
               months_btwn_episodeMeeting <= 0 ~ "episode after meeting",
               months_btwn_episodeMeeting <= 1 ~ "1 month",
               months_btwn_episodeMeeting <= 2 ~ "2 months",
               months_btwn_episodeMeeting <= 6 ~ "3-6 months",
               months_btwn_episodeMeeting <= 9 ~ "7-9 months",
               months_btwn_episodeMeeting <= 12 ~ "10-12 months",
               months_btwn_episodeMeeting <= 24 ~ "2 years",
               months_btwn_episodeMeeting <= 36 ~ "3 years",
               months_btwn_episodeMeeting <= 48 ~ "4 years",
               months_btwn_episodeMeeting <= 60 ~ "5 years",
               months_btwn_episodeMeeting <= 72 ~ "6 years",
               months_btwn_episodeMeeting <= 84 ~ "7 years",
               months_btwn_episodeMeeting <= 96 ~ "8 years",
               months_btwn_episodeMeeting <= 108 ~ "9 years",
               months_btwn_episodeMeeting <= 120 ~ "10 years",
               months_btwn_episodeMeeting <= 132 ~ "11 years",
               months_btwn_episodeMeeting <= 144 ~ "12 years",
               months_btwn_episodeMeeting <= 156 ~ "13 years",
               months_btwn_episodeMeeting <= 168 ~ "14 years",
               months_btwn_episodeMeeting <= 180 ~ "15 years",
               TRUE ~ "No episode between 2009-2022"),
           time_btwn_episodeMeetingFactor = as.factor(time_btwn_episodeMeetingFactor)) %>% 
    group_by(transcript_id) %>% 
    mutate(
        nEpisode_fiveYears = sum(time_btwn_episodeMeetingFactor %in%      
                                     c("1 month", "2 months", "3-6 months", "7-9 months", 
                                       "10-12 months", "2 years", "3 years", 
                                       "4 years", "5 years")),
        nEpisode_sixYears = sum(time_btwn_episodeMeetingFactor %in%      
                                    c("6 years", "7 years", "8 years", "9 years", 
                                      "10 years", "11 years", "12 years", "13 years", 
                                      "14 years", "15 years")),
        nEpisode_oneYear = sum(time_btwn_episodeMeetingFactor %in%
                                   c("1 month", "2 months", "3-6 months", "7-9 months", 
                                     "10-12 months")),
        nEpisode_twoYears = sum(time_btwn_episodeMeetingFactor == "2 years"),
        nEpisode_fiveYears = replace_na(nEpisode_fiveYears, 0),
        nEpisode_sixYears = replace_na(nEpisode_sixYears, 0)) %>% 
    ungroup()

# save(lvNoaa, file = "./LocalView/data/modified/merged_datasets/lvNoaa_all.rdata")

nearest_episode <- lvNoaa %>% 
    group_by(transcript_id) %>% 
    filter(months_btwn_episodeMeeting > 0) %>% 
    slice(which.min(months_btwn_episodeMeeting))

episode_after_meeting <- lvNoaa %>% 
    anti_join(nearest_episode, by = "transcript_id") %>% 
    distinct(transcript_id, .keep_all = TRUE)

lvNoaa_transcriptLevel <- bind_rows(nearest_episode, episode_after_meeting)

# save(lvNoaa_transcriptLevel, file = "./data/modified/lvNoaa_transcriptLevel.rdata")


#   ____________________________________________________________________________
#   summarize the places that have only episodes six years ago              ####

lvNoaa_sixYears <- lvNoaa_transcriptLevel %>% 
    filter(time_btwn_episodeMeetingFactor %in%      
               c("6 years", "7 years", "8 years", "9 years", 
                 "10 years", "11 years", "12 years", "13 years", 
                 "14 years", "15 years"))

