### file description ###########################################################
##                                                                            ##
## This file handles the processing steps necessary for the FEMA disaster     ##
##      declarations                                                          ##
##      Data included:                                                        ##
##          FEMA (2010-2023)                                                  ##
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
##                                                                            ##
## Output:                                                                    ##
##      /LocalView/data/modified/individual_datasets/fema_declarationLevel.rdata
##      /LocalView/data/modified/individual_datasets/fema_countyLevel.rdata   ##
##      /LocalView/data/samples/femaDecLevel_sample.csv                       ##
##      /LocalView/data/samples/femaCountyLevel_sample.csv                    ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load libraries and custom functions                                     ####

library(stringdist)         # fuzzy matching
library(fuzzyjoin)          # fuzzy matching
source("./code/preliminaries/analysis_prelims.r")
source("./code/processing/geography.r")

##  ............................................................................
##  remove unnecessary data sets                                            ####

rm(cdp, countySub, ip, ipcdp, sample_ipcdp, states, states_abbr, statesGeo)


#   ____________________________________________________________________________
#   load data                                                               ####

counties <- counties %>% 
  select(-state_fips, -county_fips)

## n = 704 
census_native_land <- read_sf("./data/original/2020_NativeLand/cb_2020_us_aiannh_500k/cb_2020_us_aiannh_500k.shp") %>% 
  select(GEOID, NAME, NAMELSAD) %>% 
  st_drop_geometry() 

#   ____________________________________________________________________________
#   FEMA disaster declarations                                              ####

## total number of declarations: 65,430 (1,509 state level declarations)
## 39,336 between 2004-2023
## 31,286 between 2004-2023 and within the incident types of interest
## n = 30,135
fema_declarationLevel <- read.csv("./data/original/2023_FEMA.csv") %>%
  rename(
    fema_id = id,
    state_fips = fipsStateCode,
    county_fips = fipsCountyCode,
    fema_declarationType = declarationType,
    fema_incidentType = incidentType) %>%
  separate(declarationDate,
    into = c("fema_year", "fema_month", "date"),
    sep = "-") %>%
  mutate(fema_day = str_sub(date, 0, 2)) %>%
  filter(
    fema_year >= 2004 & fema_year < 2023, 
    fema_incidentType == "Coastal Storm" |
    fema_incidentType == "Fire" |
    fema_incidentType == "Flood" |
    fema_incidentType == "Freezing" |
    fema_incidentType == "Hurricane" |
    fema_incidentType == "Mud/Landslide" |
    fema_incidentType == "Severe Ice Storm" |
    fema_incidentType == "Severe Storm" |
    fema_incidentType == "Snowstorm" |
    fema_incidentType == "Tornado" |
    fema_incidentType == "Typhoon" |
    fema_incidentType == "Winter Storm" |
    fema_incidentType == "Tropical Storm" |
    fema_incidentType == "Tsunami") %>%
  padFips() %>%
  createFips() %>%
  excludeStates()

# write.csv(fema_declarationLevel, "./data/modified/fema_declarationLevel.csv", row.names = FALSE)

##  ............................................................................
##     separate the state level declarations                                ####

## n = 10
fema_stateDeclarations <- fema_declarationLevel %>% 
  filter(county_fips == "000" &
         designatedArea == "Statewide")


##  ............................................................................
##  separate native land declarations                                       ####
## n = 129 unique reservations
fema_native_decs <- fema_declarationLevel %>% 
  filter(county_fips == "000" &
           designatedArea != "Statewide") %>% 
  distinct(placeCode, .keep_all = TRUE) %>% 
  mutate(designatedArea = str_replace(designatedArea,                                       
                                      pattern = " Indian Reservation| Reservation", ""),    ## standardize the names to match the shortened Census names 
         designatedArea = str_replace(designatedArea,"\\s*\\s*\\([^\\)]+\\)", ""))          ## removes anything inside parenthesis 


#   ____________________________________________________________________________
#   fuzzy match based on reservation name                                   ####

# this returns 154 results
# 19 fuzzy matches were verified (name discrepancies)
# 13 are manually updated (no census match returned)
matched <- stringdist_join(
  fema_native_decs,
  census_native_land,
  by = c("designatedArea" = "NAME"),
  mode = "left",
  method = "jw",
  max_dist = 0.2) %>% 
  select(GEOID, NAME, designatedArea, femaDeclarationString) 

## 92 return a 100% match
verified <- matched %>% 
  subset(NAME == designatedArea) %>% 
  mutate(match_type = "verified")

## 62 locations need to have their GEOIDs manually verified
unverified <- anti_join(matched, verified)

## data is manually verified in excel
# write.csv(unverified, "./data/modified/fema_native_unverified.csv", row.names = FALSE)

## 17 are mismatched and can be discarded
unverified <- read.csv("./data/modified/fema_native_unverified.csv") %>% 
  mutate(GEOID = as.character(GEOID)) %>% 
  filter(match_type != "discard")

## 111 verified either first or second round
verified2 <- unverified %>% 
  filter(match_type == "verified") %>% 
  bind_rows(verified)

## 26 unmatched rows
unverified <- anti_join(unverified, verified2)

## export and update in excel
# write.csv(unverified, "./data/modified/fema_native_unverified2.csv", row.names = FALSE)





  # group_by(fema_year, stcounty_fips) %>%
  # mutate(
  #   fema_nDecCountyYear = n(),                                                  # range: 1-21 (with state level decs) 1-9 (without state level decs)
  #   fema_nDecTypeCountyYear = n_distinct(fema_declarationType)) %>%             # range: 1-3
  # ungroup() %>%
  # select(
  #   fema_id, fema_year, fema_month, fema_day, fema_declarationType,
  #   fema_incidentType, state_fips, county_fips, stcounty_fips,
  #   fema_nDecCountyYear, fema_nDecTypeCountyYear) 



##  ............................................................................
##  fema without state level declarations                                   ####

# fema_wo_statewideDec <- fema_declarationLevel %>%
#   filter(county_fips != "000")

##  ............................................................................
# create county-year level fema data                                        ####

## loop through fema and merge it with county data
f_merged <- list()
for (y in unique(fema_declarationLevel$fema_year)) {
  f <- fema_declarationLevel %>%
    filter(fema_year == y) %>% 
    distinct(stcounty_fips, .keep_all = TRUE) 

  if (y == 2009) {
    f_merged[[y]] <- f %>%
      right_join(counties, by = "stcounty_fips") %>%
      mutate(
        fema_year = as.numeric(y),
        transcript_year = fema_year + 1,
        fema_decBinary = ifelse(is.na(fema_nDecCountyYear), 0, 1)
      )
  } else if (y >= 2010 & y <= 2014) {
    f_merged[[y]] <- f %>%
      right_join(counties, by = "stcounty_fips") %>%
      mutate(
        fema_year = as.numeric(y),
        transcript_year = fema_year + 1,
        fema_decBinary = ifelse(is.na(fema_nDecCountyYear), 0, 1)
      )
  } else if (y >= 2015 & y <= 2019) {
    f_merged[[y]] <- f %>%
      right_join(counties, by = "stcounty_fips") %>%
      mutate(
        fema_year = as.numeric(y),
        transcript_year = fema_year + 1,
        fema_decBinary = ifelse(is.na(fema_nDecCountyYear), 0, 1)
      )
  } else {
    f_merged[[y]] <- f %>%
      right_join(counties, by = "stcounty_fips") %>%
      mutate(
        fema_year = as.numeric(y),
        transcript_year = fema_year + 1,
        fema_decBinary = ifelse(is.na(fema_nDecCountyYear), 0, 1)
      )
  }
}

## n = 59,052
## 3,105 unique incidents in 3,108 counties (all years)
fema_countyLevel <- bind_rows(f_merged) %>%
  mutate(transcript_year = as.character(transcript_year),
         fema_nDecTypeCountyYear = ifelse(is.na(fema_nDecTypeCountyYear), 0, fema_nDecTypeCountyYear),
         fema_nDecCountyYear = ifelse(is.na(fema_nDecCountyYear), 0, fema_nDecCountyYear)) %>%
  select(stcounty_fips, fema_year, transcript_year, fema_nDecCountyYear,
    fema_nDecTypeCountyYear, fema_decBinary)

# save(fema_countyLevel, file = "./LocalView/data/modified/individual_datasets/fema_countyLevel.rdata")

# fema_countyLevel_geo <- fema_countyLevel %>%
#   left_join(countiesGeo) %>%
#   st_write("./GIS/modified/fema_countyLevel_geo.gpkg")


##  ............................................................................
##  FEMA Sample                                                             ####

sample_femaDecLevel <- fema_declarationLevel %>% sample_n(50)
# write.csv(sample_femaDecLevel, "./LocalView/data/samples/femaDecLevel_sample.csv", row.names = FALSE)

sample_femaCountyLevel <- fema_countyLevel %>% sample_n(50)
# write.csv(sample_femaCountyLevel, "./LocalView/data/samples/femaCountyLevel_sample.csv", row.names = FALSE)

