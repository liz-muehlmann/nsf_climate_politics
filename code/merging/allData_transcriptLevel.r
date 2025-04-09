### file description ###########################################################
##                                                                            ##
## This file merges the clean versions of the data below into one data set.   ##    
##      The data is at the county-year level. Individual data                 ##    
##      processing files are linked below.                                    ##                            
##      Detailed process documentation is available upon request.             ##
##                                                                            ##
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##
##              ./processing_scripts/local_view.r                             ##
##          Algara-Sharif (2008-2020)                                         ##
##              Sharif (2021) "Replication Data for: Partisanship &           ##
##              Nationalization in American Elections: Evidence from          ##
##              Presidential, Senatorial, &                                   ##
##              Gubernatorial Elections in the U.S. Counties, 1872-2020",     ##
##              https://doi.org/10.7910/DVN/DGUMFI                            ##
##              ./processing_scripts/algara_sharif.r                          ##
##          American Community Survey (2010, 2015, 2020)                      ##
##              2006-2010 ACS > 2008                                          ##
##              2011-2015 ACS > 2012 Election                                 ##
##              2016-2020 ACS > 2016 & 2020 Elections                         ##
##              ./processing_scripts/acs.r                                    ##
##          FEMA (2010-2023)                                                  ##
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
##          Climate Change Vulnerability (2010-2023)                          ##
##              https://github.com/wachiuphd/CVI                              ##
##              ./processing_scripts/cvi.r                                    ##
##          USDA Rural-Urban                                                  ##
##              https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/documentation/ 
##              ./processing_scripts/rural_urban.r                            ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load libraries                                                          ####

library(tidyverse)          # data manipulation
`%notin%` <- Negate(`%in%`) # create not in operator

#   ____________________________________________________________________________
#   load data                                                               ####

source("./code/processing/geography.r")                                         # county data
load("./data/modified/local_view/lv_clean_noTranscript.rdata")                  # local view
load("./data/modified/cvi.rdata")                                               # CVI
load("./data/modified/acs.rdata")                                               # ACS
load("./data/modified/algara_sharif.rdata")                                     # elections
load("./data/modified/rural_urban.rdata")                                       # rural-urban
load("./data/modified/lvFema_transcriptLevel.rdata")                                          # FEMA
load("./data/modified/lvNoaa_transcriptLevel.rdata")                                          # NOAA

#   ____________________________________________________________________________
#   merge county data with local view                                       ####

lvCounties <- left_join(lvClean_noTranscript, counties, 
                        relationship="many-to-one")

#   ____________________________________________________________________________
#   separate american community survey                                      ####

for (y in unique(acs$acs_year)) {
    a <- acs %>%
        filter(acs_year == y)
    assign(paste("acs", y, sep = ""), a)
}

##  ............................................................................
##  merge lv-cvi to acs                                                     ####

lv_acs <- list()

for (y in unique(lvCounties$transcript_year)) {
    ad <- lvCounties %>%
        filter(transcript_year == y)
    
    if (y <= 2014) {
        lv_acs[[y]] <- ad %>%
            left_join(acs2010)
    }
    
    if (y >= 2015 & y <= 2019) {
        lv_acs[[y]] <- ad %>%
            left_join(acs2015)
    }
    
    if (y >= 2020) {
        lv_acs[[y]] <- ad %>%
            left_join(acs2020)
    }
}

lv_acs <- bind_rows(lv_acs)

#   ____________________________________________________________________________
#   separate rural urban designations                                       ####

for (y in unique(ruralUrban$ru_year)) {
    ru <- ruralUrban %>%
        filter(ru_year == y)
    
    assign(paste("ru", y, sep = ""), ru)
}

##  ............................................................................
##  merge lv-cvi-acs with rural-urban                                       ####

lvACS_ru <- list()

for (y in unique(lv_acs$transcript_year)) {
    ad <- lv_acs %>%
        filter(transcript_year == y)
    
    if (y <= 2012) {
        lvACS_ru[[y]] <- ad %>%
            left_join(ru2003)
    }
    
    if (y >= 2013 & y <= 2022) {
        lvACS_ru[[y]] <- ad %>%
            left_join(ru2013)
    }
    
    if (y == 2023) {
        lvACS_ru[[y]] <- ad %>%
            left_join(ru2023)
    }
}

lvACS_ru <- bind_rows(lvACS_ru)

#   ____________________________________________________________________________
#   separate algara-sharif election results                                 ####

for (y in unique(algara$election_year)) {
    e <- algara %>%
        filter(election_year == y)
    
    assign(paste("algara", y, sep = ""), e)
}

##  ............................................................................
##  merge lv-cvi-acs-ruralUrban with algara sharif elections                ####

lvacsru_algara <- list()

for (y in unique(lvACS_ru$transcript_year)) {
    ad <- lvACS_ru %>%
        filter(transcript_year == y)
    
    if (y <= 2012) {
        lvacsru_algara[[y]] <- ad %>%
            left_join(algara2008)
    }
    
    if (y >= 2013 & y <= 2016) {
        lvacsru_algara[[y]] <- ad %>%
            left_join(algara2012)
    }
    
    if (y >= 2017 & y <= 2020) {
        lvacsru_algara[[y]] <- ad %>%
            left_join(algara2016)
    }
    
    if (y >= 2021) {
        lvacsru_algara[[y]] <- ad %>%
            left_join(algara2020)
    }
}

lvacsru_algara <- bind_rows(lvacsru_algara)

#   ____________________________________________________________________________
#   merge lv with climate change vulnerability                              ####

lvACSruAlgaraCVI_transcriptLevel <- left_join(lvacsru_algara, cvi,
                                              by = "stcounty_fips",
                                              relationship = "many-to-one") %>%
    relocate(census_division, census_region, transcript_year,
             .after = county_fips) %>%
    relocate(ru_year, .after = edu_percentPop)

# save(lvACSruAlgaraCVI_transcriptLevel, file = "./LocalView/data/modified/merged_datasets/lvACSruAlgaraCVI_transcriptLevel.rdata")

allData_transcriptLevel <- left_join(lvacsru_algara, cvi,
                                     by = "stcounty_fips",
                                     relationship = "many-to-one") %>%
    left_join(lvFema_transcriptLevel) %>%
    left_join(lvNoaa_transcriptLevel) 

# save(allData_transcriptLevel, file = "./data/modified/all_data/allData_transcriptLevel.rdata")

#   ____________________________________________________________________________
#   add in county geometry                                                  ####

allData_transcriptLevel_geo <- allData_transcriptLevel %>%
    left_join(countiesGeo, relationship = "many-to-one") 

# allData_transcriptLevel_geo <- allData_transcriptLevel %>%
#     st_write("./data/modified/all_data/allData_transcriptLevel_geo.gpkg")

allData_latestMeeting <- allData_transcriptLevel_geo %>%
    group_by(stcounty_fips) %>%
    slice(which.max(meeting_date))

# allData_latestMeeting %>%
#     st_write("./data/modified/all_data/allData_latestMeeting.gpkg")
# 
















