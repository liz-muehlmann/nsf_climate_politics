## file description ############################################################
##                                                                            ##
## This file handles the geography data needed to for working with the        ## 
##          local view data                                                   ##
##          Incorporated/Census Designated Places (2023)                      ## 
##              IP/CDP csvs were saved using the tabulate intersection        ##
##              function in ArcGIS                                            ##
##          County Subdivisions                                               ##
##              https://www.census.gov/library/reference/code-lists/ansi.html#cousub
##          Counties (2010, 2015, 2020)                                       ##
##              tigris                                                        ##
##          States                                                            ##
##              tigris                                                        ## 
##                                                                            ##
## Output:                                                                    ##    
##          None                                                              ##
################################################################################

#   ____________________________________________________________________________
#   load libraries and custom functions                                     ####

library(tigris)                                 # download geographic boundaries
library(sf)                                     # work with shapefiles

source("./code/preliminaries/analysis_prelims.r")

#   ____________________________________________________________________________
#   state boundaries                                                        ####

statesGeo <- states()
st_crs(statesGeo) <- "EPSG:4326"
statesGeo <- statesGeo %>% 
    rename(state_fips = STATEFP,
           state_name = NAME,
           census_region = REGION,
           census_division = DIVISION,
           state_abbr = STUSPS) %>%
    select(state_fips, state_name, state_abbr, census_division, census_region, geometry) %>% 
    excludeStates()

states_abbr <- statesGeo %>% 
    st_drop_geometry() 

states <- statesGeo %>% 
    st_drop_geometry() %>% 
    select(-state_abbr)

#   ____________________________________________________________________________
#   county boundaries                                                       ####

countiesGeo <- counties(year = 2020, cb = TRUE) %>% 
    rename(stcounty_fips = GEOID,
           county_name = NAMELSAD,
           state_fips = STATEFP,
           county_fips = COUNTYFP) %>% 
    fixCounties2020() %>% 
    excludeStates() %>% 
    left_join(states) %>% 
    group_by(state_name) %>% 
    mutate(n_countiesInState = n_distinct(stcounty_fips)) %>% 
    select(stcounty_fips, state_name, state_fips, county_fips, county_name, 
           census_division, census_region, n_countiesInState) %>% 
    ungroup()

counties <- countiesGeo %>% 
    st_drop_geometry()

#   ____________________________________________________________________________
##  county subdivisions                                                     ####

countySub <- read.csv("./data/modified/2020_SubcountyDivisions.csv")  %>% 
    padFips() %>% 
    excludeStates() %>% 
    mutate(place_fips = paste(state_fips, countysub_fips, sep="")) %>% 
    createFips() %>% 
    select(stcounty_fips, place_fips, county_name)

#   ____________________________________________________________________________
##  incorporated and census designated places                               ####

ip <- majority(read.csv("./data/modified/2020_ip_overlap.csv")) %>% 
    padFips() %>% 
    createFips()

cdp <- majority(read.csv("./data/modified/2020_cdp_overlap.csv")) %>% 
    padFips() %>% 
    createFips()

ipcdp <- rbind(ip, cdp) %>%
    select(-place_name, -coverage)

##  ............................................................................
##  ipcdp Sample                                                            ####

sample_ipcdp <- ipcdp %>% slice_sample(n = 50, replace = FALSE)
# write.csv(sample_ipcdp, "./results/samples/2020_ipcdp_sample.csv", row.names = FALSE)

