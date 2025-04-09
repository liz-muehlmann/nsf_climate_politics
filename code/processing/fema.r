### file description ###########################################################
##                                                                            ##
## This file handles the processing steps necessary for the FEMA disaster     ##
##      declarations                                                          ##
##      Data included:                                                        ##
##          FEMA (2004-2023)                                                  ##
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

rm(cdp, countySub, ip, ipcdp, sample_ipcdp, states, statesGeo)

#   ____________________________________________________________________________
#   load data                                                               ####

## n = 3,108
counties <- counties %>% 
  select(-state_fips, -county_fips)

## n = 704 
census_native_land <- read_sf("./data/original/2020_NativeLand/cb_2020_us_aiannh_500k/cb_2020_us_aiannh_500k.shp") %>% 
  select(GEOID, NAME, NAMELSAD) %>% 
  st_drop_geometry() 

#   ____________________________________________________________________________
#   FEMA disaster declarations                                              ####

## total number of declarations
## 30,135 rows
## 2,192 unique declarations in 49 states
## 10 statewide declarations; 184 native declarations
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

#   ____________________________________________________________________________
#   state level declarations                                                ####

## n = 10 declarations, 7 states, 10 rows
fema_state_decs <- fema_declarationLevel %>% 
  filter(county_fips == "000" &
           designatedArea == "Statewide") %>% 
  left_join(states_abbr) %>% 
  select(-county_fips, -stcounty_fips)

##  ............................................................................
##  disaggregate statewide declarations                                     ####

## n = 10 declarations, 654 counties
fema_state_decs <- counties %>% 
  filter(state_name %in% fema_state_decs$state_name) %>% 
  left_join(fema_state_decs, relationship="many-to-many") %>% 
  select(-state_name, -starts_with("census"), -n_countiesInState, -state_abbr, -county_name)

#   ____________________________________________________________________________
#   native land declarations                                                ####

## n = 184 unique declarations
## n = 127 native land areas
fema_native_decs <- fema_declarationLevel %>% 
  filter(county_fips == "000" &
           designatedArea != "Statewide") %>% 
  mutate(designatedArea = str_replace(designatedArea,                                       
                                      pattern = " Indian Reservation| Reservation", ""),    ## standardize the names to match the shortened Census names 
         designatedArea = str_replace(designatedArea,"\\s*\\s*\\([^\\)]+\\)", ""),          ## removes anything inside parenthesis 
         designatedArea = str_replace(designatedArea, "Pueblo of", ""),                     ## remove pueblo from names
         designatedArea = str_replace(designatedArea, "Rancheria", ""),                     ## remove rancheria from names
         designatedArea = str_replace(designatedArea, "Pueblo", ""),
         designatedArea = str_replace(designatedArea, "Community", ""))

##  ............................................................................
##  fuzzy match based on reservation name                                   ####

## 408 matches
## 270 are 100% matches
## 138 need to be manually verified in excel
## 184 unique declarations
## 125 unique native land areas (by name) 
## 132 unique native land areas (by GEOID)
fuzzy_match <- stringdist_join(
  fema_native_decs,
  census_native_land,
  by = c("designatedArea" = "NAME"),
  mode = "left",
  method = "jw",
  max_dist = 0.2) %>% 
  select(GEOID, NAME, designatedArea, femaDeclarationString) %>% 
  mutate(NAME = str_trim(NAME, side = "both"),
         designatedArea = str_trim(designatedArea, side = "both"),
         match_type = ifelse(NAME == designatedArea, "verified", NA))           ## 92 return a 100% match; 58 unique declarations


## data is manually matched in excel/ArcGIS
# write.csv(fuzzy_match,
#           "./data/modified/fema/fema_native_fuzzy_matched.csv",
#           row.names = FALSE)

##  ............................................................................
##  load verified fema data                                                 ####

## 409 rows
## 68 discard + 1 White Mountain row
## 70 manually verified in ArcGIS (270 previously verified)
## 135 unique native lands (by GEOID)
## 124 unique native lands (by name)
## 184 declarations
fuzzy_match <- read.csv("./data/modified/fema/fema_native_fuzzy_matched.csv") %>% 
  filter(match_type != "discard",
         GEOID != 7745) %>%                                                     #  7745 = White Mountain, Alaska
  select(-NAME) %>% 
  mutate(GEOID = as.character(str_pad(GEOID, 4, "left", "0")))

##  ............................................................................
##  add county information to matched fema data                             ####

## 325 counties with native land in them
overlap <- read.csv("./data/modified/fema/native_county_overlap.csv") %>% 
  mutate(GEOID = str_pad(GEOID, 4, "left", "0")) %>% 
  rename(stcounty_fips = GEOID_1) %>%
  select(-NAMELSAD_1)

## 124 unique native lands
## 250 counties
## 184 declarations
fema_native_matched <- left_join(fuzzy_match, 
                                 overlap, by = "GEOID", 
                                 relationship = "many-to-many") %>% 
  padFips() %>% 
  createFips() %>% 
  excludeStates()

##  ............................................................................
##  merge with native declaration data                                      ####

## 184 disasters
## 124 native lands
## 250 counties
fema_native_decs <- fema_native_decs %>% 
  select(-stcounty_fips, -state_fips, -county_fips, -designatedArea) %>% 
  right_join(fema_native_matched, 
             fema_native_decs, 
             by = "femaDeclarationString",
             relationship = "many-to-many") %>% 
  select(-match_type, -AREA, -PERCENTAGE, -GEOID, -county_fips, -NAMELSAD)

#   ____________________________________________________________________________
#   combine state & native declarations                                     ####

## 878 counties
## 29 states
## 194 declarations
fema_state_decs <- rbind(fema_state_decs, fema_native_decs) %>% 
  distinct(stcounty_fips, femaDeclarationString, .keep_all = TRUE) 

#   ____________________________________________________________________________
#   n declarations                                                          ####

## n = 32,137 rows
## n = 2,192 unique declarations
## n = 49 states
## n = 3,109 counties
fema_declarationLevel <- fema_declarationLevel %>% 
  filter(!(county_fips == "000")) %>% 
  bind_rows(fema_state_decs) %>% 
  group_by(fema_year, stcounty_fips) %>% 
  mutate(fema_nDecCountyYear = n(),                                             # range: 1-10
         fema_nDecTypeCountyYear = n_distinct(fema_declarationType)) %>%        # range: 1-3
  ungroup() %>% 
  select(femaDeclarationString, fema_year, fema_month, fema_day, fema_declarationType,
         fema_incidentType, state_fips, stcounty_fips,
         fema_nDecCountyYear, fema_nDecTypeCountyYear) %>% 
  createFips()

# save(fema_declarationLevel, file = "./data/modified/fema/fema_declarationLevel.rdata")

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

## n = 20,208
## 3,108 counties (all years)
fema_countyLevel <- bind_rows(f_merged) %>%
  mutate(transcript_year = as.character(transcript_year),
         fema_nDecTypeCountyYear = ifelse(is.na(fema_nDecTypeCountyYear), 0, fema_nDecTypeCountyYear),
         fema_nDecCountyYear = ifelse(is.na(fema_nDecCountyYear), 0, fema_nDecCountyYear)) %>%
  select(stcounty_fips, fema_year, transcript_year, fema_nDecCountyYear,
         fema_nDecTypeCountyYear, fema_decBinary)

save(fema_countyLevel, file = "./data/modified/fema/fema_countyLevel.rdata")

# fema_countyLevel_geo <- fema_countyLevel %>%
#   left_join(countiesGeo) %>%
#   st_write("./data/modified/fema/fema_countyLevel_geo.gpkg")


##  ............................................................................
##  FEMA Sample                                                             ####

sample_femaDecLevel <- fema_declarationLevel %>% sample_n(50)
# write.csv(sample_femaDecLevel, "./LocalView/data/samples/femaDecLevel_sample.csv", row.names = FALSE)

sample_femaCountyLevel <- fema_countyLevel %>% sample_n(50)
# write.csv(sample_femaCountyLevel, "./LocalView/data/samples/femaCountyLevel_sample.csv", row.names = FALSE)

