### file description ###########################################################
##                                                                            ##
## This file processes the National Oceanic and Atmospheric Association data  ##
##                                                                            ##
##      Data included:                                                        ##
##          NOAA (2009-2023)                                                  ##
##              https://www.ncdc.noaa.gov/stormevents/ftp.jsp                 ##        
##                                                                            ##
##      Output:                                                               ##                                                    
##          /LocalView/data/modified/individual_datasets/noaa_episodeCountyLevel.rdata
##          /LocalView/data/modified/individual_datasets/noaa_countyLevel.rdata 
##          /GIS/modified/noaa_countyLevel_geo.gpkg                           ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load preliminaries and data                                             ####

library(data.table)             # load multiple files
library(fs)                     # load multiple files

source("./code/preliminaries/analysis_prelims.r")      
source("./code/processing/geography.r")

## remove unnecessary data sets
rm(cdp, ip, ipcdp, sample_ipcdp, countySub, states_abbr, statesGeo)

#   ____________________________________________________________________________
#   function to convert property and crop units                             ####

convert_units <- function(x) {
    
    # If x is a vector (more than one element), apply recursively
    if (length(x) > 1) {
        return(sapply(x, convert_units))
    }
    
    # If the string is empty, return 0
    if (x == "") return(0)
    if (is.na(x)) return(0)
    
    # If the string is in scientific notation, return it as numeric
    x_str <- as.character(x)
    if (grepl("e", x_str, ignore.case = TRUE)) {
        return(as.numeric(x_str))
    }
    
    # If x is already numeric, just return it
    if (class(x) == "numeric") return(x)
    
    # Named vector of scale for 'K' and 'M' units
    unit_scale <- c("k" = 1e3, "m" = 1e6)
    
    # Clean up input: remove commas and whitespace, and make lowercase
    x_str <- gsub(",", "", trimws(tolower(x_str)))
    
    # Extract the unit character (e.g., 'k' or 'm')
    unit_char <- gsub("[^a-z]", "", x_str)
    
    # Extract the numeric part of the string
    x_num <- as.numeric(gsub("[a-z]", "", x_str))
    
    # Look up the multiplier for the extracted unit character
    multiplier <- unit_scale[match(unit_char, names(unit_scale))]
    multiplier[is.na(multiplier)] <- 1  # If no unit, assume multiplier of 1
    
    # Return the numeric value multiplied by the appropriate unit scale
    return(x_num * multiplier)
}

#   ____________________________________________________________________________
#   remove unneeded data from geography.r file

counties <- counties %>% 
    select(stcounty_fips, state_fips, county_fips) 

#   ____________________________________________________________________________
#   noaa zone-to-county correlation file                                    ####

# original file: 516 zones, 59 states, 3269 counties
# after merge: 463 zones, 49 states (inc. DC), 3108 counties
noaa_correlation <- read.csv("./data/original/noaa_county_correlation.csv") %>% 
    select(state_abbr, stcounty_fips, zone, county_name, zone_name) %>% 
    mutate(zone = str_pad(zone, 3, "left", 0)) %>% 
    padFips() %>% 
    rename(noaa_zoneCode = zone) %>% 
    excludeStates() %>% 
    filter(state_abbr != "FM" &
           state_abbr != "MH" &
           state_abbr != "PW") %>% 
    left_join(counties)

#   ____________________________________________________________________________
#   load the details files                                                  ####

path <- "./data/original/NOAA/noaa_events/Details/"

files <- dir_ls(path)

## 2004-2024
## original data: 1,312,931 Unique Events; 238,597 Unique Episodes; 69 states
## our sample: 200,593 unique events; 49 states
noaa_episodeLevel <- files %>% 
    map_dfr(read.csv) %>%                                                       # load all data
    rename_with(tolower) %>% 
    select(episode_id, event_id, begin_yearmonth, begin_day, end_yearmonth,     # select columns of interest
       end_day, state, state_fips, event_type, cz_type, cz_fips, 
       cz_name, injuries_direct, injuries_indirect, deaths_direct, 
       deaths_indirect, damage_property, damage_crops, flood_cause) %>% 
    filter(event_type == "Blizzard" |                                           # select events of interest
           event_type == "Coastal Flood" |
           event_type == "Cold/Wind Chill" |
           event_type == "Drought" |
           event_type == "Excessive Heat" |
           event_type == "Extreme Cold/Wind Chill" |
           event_type == "Flash Flood" |
           event_type == "Flood" |
           event_type == "Heat" |
           event_type == "Heavy Rain" |
           event_type == "Heavy Snow" |
           event_type == "High Wind" |
           event_type == "Hurricane" |
           event_type == "Hurricane (Typhoon)" |
           event_type == "Ice Storm" |
           event_type == "Lakeshore Flood" |
           event_type == "Storm Surge/Tide" |
           event_type == "Thunderstorm Wind" |
           event_type == "Tornado" |
           event_type == "Tropical Storm" |
           event_type == "Tsunami" |
           event_type == "Wildfire" |
           event_type == "Winter Storm" |
           event_type == "Winter Weather" |
           event_type == "Hail" |
           event_type == "Tornado Depression") %>% 
    padFips() %>% 
    excludeStates() %>% 
    group_by(episode_id) %>% 
    mutate(cz_fips = str_pad(cz_fips, 3, "left", 0),
           damaged_property_converted = convert_units(damage_property),
           damaged_crops_converted = convert_units(damage_crops),
           noaa_episode_injuriesDirect = sum(injuries_direct),
           noaa_episode_injuriesIndirect = sum(injuries_indirect),
           noaa_episode_deathsDirect = sum(deaths_direct),
           noaa_episode_deathsIndirect = sum(deaths_indirect),
           noaa_episode_damagedProperty = sum(damaged_property_converted),      # sum property damage - by episode
           noaa_episode_cropDamage = sum(damaged_crops_converted),
           state = str_to_title(state),
           cz_name = str_to_title(cz_name),
           noaa_episode_beginYear = str_sub(begin_yearmonth,0, 4),              # create begin/end date
           noaa_episode_endYear = str_sub(end_yearmonth, 0, 4),
           noaa_episode_beginMonth = str_sub(begin_yearmonth, 5),
           noaa_episode_endMonth = str_sub(end_yearmonth, 5),
           noaa_episode_beginDate = make_date(noaa_episode_beginYear, noaa_episode_beginMonth, begin_day),
           noaa_episode_endDate = make_date(noaa_episode_endYear, noaa_episode_endMonth, end_day),
           noaa_year = str_sub(noaa_episode_beginDate, 1,4),
           noaa_episode_beginDate = ymd(noaa_episode_beginDate),
           noaa_episode_endDate = ymd(noaa_episode_endDate),
           noaa_nEventsInEpisode = n(),
           noaa_nEventTypesInEpisode = n_distinct(event_type),
           cz_type = ifelse(state_fips == 26 & cz_fips == "019",
                                "C", cz_type)) %>%                              # 10 rows use zone codes incorrectly
    select(episode_id, event_id, state_fips, noaa_year, event_type,             # select variables of interest
           cz_type, cz_fips, cz_name, noaa_episode_injuriesDirect, 
           noaa_episode_injuriesIndirect, noaa_episode_deathsDirect, 
           noaa_episode_deathsIndirect, noaa_episode_damagedProperty,
           noaa_episode_beginDate, noaa_episode_endDate, noaa_nEventsInEpisode, 
           noaa_nEventTypesInEpisode) %>% 
    ungroup() %>% 
    rename(noaa_episodeID = episode_id,                                         # rename columns    
           noaa_eventID = event_id,
           noaa_eventType = event_type,
           noaa_czType = cz_type,
           noaa_zoneCode = cz_fips,
           noaa_czName = cz_name,
           noaa_episode_beginDate = noaa_episode_beginDate,
           noaa_episode_endDate = noaa_episode_endDate) %>% 
    distinct(noaa_episodeID, .keep_all = TRUE)

##  ............................................................................
##  separate rows that use county vs zone fips                              ####

## n = 135,734,
noaa_county <- noaa_episodeLevel %>% 
    filter(noaa_czType == "C") %>% 
    mutate(stcounty_fips = paste0(state_fips, noaa_zoneCode, "")) %>% 
    group_by(stcounty_fips, noaa_year) %>% 
    mutate(n_episodesCountyYear = n_distinct(noaa_episodeID),
           n_eventsCountyYear = sum(noaa_nEventsInEpisode),
           n_eventTypesCountyYear = sum(noaa_nEventTypesInEpisode)) %>% 
    ungroup() %>% 
    group_by(noaa_episodeID) %>% 
    mutate(n_countiesInEpisode = n_distinct(stcounty_fips)) %>% 
    ungroup() 

## n = 86,135 rows use zone codes instead of county fips
## n = 2,982 unique counties within those zones
## n = 84,179 unique episodes; 2,982 counties; 427 zones
noaa_zones <- noaa_episodeLevel %>% 
    filter(noaa_czType == "Z") %>% 
    left_join(noaa_correlation, by = c("noaa_zoneCode", "state_fips"), 
              relationship = "many-to-many") %>% 
    distinct(stcounty_fips, noaa_episodeID, .keep_all = TRUE) %>%               ## this is to make sure that duplicates aren't counted in the mutate
    group_by(stcounty_fips, noaa_year) %>% 
    mutate(n_episodesCountyYear = n_distinct(noaa_episodeID),
           n_eventsCountyYear = sum(noaa_nEventsInEpisode),
           n_eventTypesCountyYear = sum(noaa_nEventTypesInEpisode)) %>% 
    ungroup() %>% 
    group_by(noaa_episodeID) %>% 
    mutate(n_countiesInEpisode = n_distinct(stcounty_fips)) %>% 
    ungroup() %>% 
    select(-county_fips, -state_abbr, -zone_name, -county_name) 
    
## contains distinct rows of county and episode
## counties can have multiple episodes and episodes can span counties
## n = 216,860
## n = 196,113 unique episodes
## contains rows for both counties and zones for some episodes
noaa_episodeCountyLevel <- rbind(noaa_county, noaa_zones) %>% 
    distinct(noaa_episodeID, stcounty_fips, .keep_all = TRUE) %>% 
    select(-noaa_eventID, -noaa_eventType, -noaa_czType, -noaa_zoneCode)

# save(noaa_episodeCountyLevel, file = "./data/modified/noaa_episodeCountyLevel.rdata")

#   ____________________________________________________________________________
#   create county-year level data                                           ####

# some of the noaa years do not have events, this section adds the missing counties

n_merged <- list()
for (y in unique(noaa_episodeCountyLevel$noaa_year)){
    n <- noaa_episodeCountyLevel %>% 
        filter(noaa_year == y) %>% 
        distinct(stcounty_fips, .keep_all = TRUE) 
    
    if (y >= 2004 & y <= 2009){
        n_merged[[y]] <- n %>% 
            select(-state_fips) %>% 
            right_join(counties, by = "stcounty_fips") %>% 
            mutate(
                noaa_year = as.numeric(y),
                transcript_year = noaa_year + 1,
                noaa_nEpisodeBinary = ifelse(is.na(n_episodesCountyYear), 0, 1)
            )
    } else if (y >= 2010 & y <= 2014) {
        n_merged[[y]] <- n %>% 
            select(-state_fips) %>% 
            right_join(counties, by = "stcounty_fips") %>% 
            mutate(
                noaa_year = as.numeric(y),
                transcript_year = noaa_year + 1,
                noaa_nEpisodeBinary = ifelse(is.na(n_episodesCountyYear), 0, 1)
            )
    } else if (y >= 2015 & y <= 2019) {
        n_merged[[y]] <- n %>% 
            select(-state_fips) %>% 
            right_join(counties, by = "stcounty_fips") %>% 
            mutate(
                noaa_year = as.numeric(y),
                transcript_year = noaa_year + 1,
                noaa_nEpisodeBinary = ifelse(is.na(n_episodesCountyYear), 0, 1)
            )
    } else {
        n_merged[[y]] <- n %>% 
            select(-state_fips) %>% 
            right_join(counties, by = "stcounty_fips") %>% 
            mutate(
                noaa_year = as.numeric(y),
                transcript_year = noaa_year + 1,
                noaa_nEpisodeBinary = ifelse(is.na(n_episodesCountyYear), 0, 1)
            )
    }
}

noaa_countyLevel <- bind_rows(n_merged) %>% 
    mutate(transcript_year = as.character(transcript_year),
           n_episodesCountyYear = ifelse(is.na(n_episodesCountyYear), 0, n_episodesCountyYear)) %>% 
    select(-noaa_czName, -county_fips, -state_fips, -noaa_czName)

# save(noaa_countyLevel, file = "./data/modified/noaa_countyLevel.rdata")

## write and save as geopackage
# noaa_countyLevel_geo <- noaa_countyLevel %>%
#     left_join(countiesGeo) %>%
#     st_write("./data/modified/noaa_countyLevel_geo.gpkg")












