################################################################################ 
##                                                                            ##
##  This file includes the custom functions used to process local view data.  ##
##                                                                            ##
##  Output:                                                                   ##
##      None                                                                  ##
################################################################################ 

################################################################################ 
##                                                                            ##
##                                                                            ##
##                          load necessary packages                           ##
##                                                                            ##   
##                                                                            ##
################################################################################ 

`%notin%` <- Negate(`%in%`)                        # create not in operator
library(tidyverse)                                 # data manipulation 
library(tidycensus)                                # ACS data
library(readxl)                                    # work with xlsx files (CVI)
library(strcode)                                   # easy code separators
library(quanteda)                                  # text analysis
options(strcode = list(insert_with_shiny = FALSE,  # set options
                       char_length = 80, 
                       hash_in_sep= TRUE))

################################################################################ 
##                                                                            ##
##                                                                            ##
##                      Geographic identifier functions                       ##
##                                                                            ##   
##                                                                            ##
################################################################################ 

##  ............................................................................
##  Calculate the majority overlap of places to county                      ####
##      Arguments:                                                            ##
##        df: a data frame                                                    ##
##      Returns:                                                              ##
##        A data frame with only places with majority overlap on a county     ##
##   ...........................................................................

majority <- function(df){
    df %>% mutate(coverage = round(coverage, 2)) %>% 
        filter(coverage != 0) %>% 
        group_by(place_fips) %>% 
        filter(coverage == max(coverage, na.rm = TRUE)) 
}

##  ............................................................................
##  Pad FIPS to the appropriate length                                      ####
##      Arguments:                                                            ##
##           df: a data frame with one or more geographic identifying columns.##                                 
##      Returns:                                                              ##
##           A data frame with fips padded to 2, 3, 5, or 7 digits            ##
##   ...........................................................................

padFips <- function(df) {
    if ("stcounty_fips" %in% colnames(df)) {
        df <- df %>%
            mutate(stcounty_fips = str_pad(stcounty_fips, 5, side = "left",  "0"))
    }
    
    # 3 digit county
    if ("county_fips" %in% colnames(df)) {
        df <- df %>%
            mutate(county_fips = str_pad(county_fips, 3, side = "left",  "0"))
    }
    
    # 5 digit subcounty division
    if ("countysub_fips" %in% colnames(df)) {
        df <- df %>%
            mutate(countysub_fips = str_pad(countysub_fips, 5, side = "left",  "0"))
    }
    if ("state_fips" %in% colnames(df)) {
        df <- df %>%
            mutate(state_fips = str_pad(state_fips, 2, side = "left",  "0"))
    } 
    
    # 2 digit state + 5 digit place
    if ("place_fips" %in% colnames(df)) {
        df <- df %>%
            mutate(place_fips = str_pad(place_fips, 7, side = "left",  "0"))
    }
    
    # 5 digit place
    if ("placeOnly" %in% colnames(df)) {
        df <- df %>%
            mutate(placeOnly = str_pad(placeOnly, 5, side = "left",  "0"))
    }
    return(df)
}

##  ............................................................................
##  Create missing FIPS columns.                                            ####
##      Arguments:                                                            ##
##           df: a data frame with one or more geographic identifying columns.##                                 
##      Returns:                                                              ##
##           A data frame with state, county, and state+county FIPS           ##
##   ...........................................................................

createFips <- function(df){
    # create state and county fips if not already in the data
    if ("tract_fips" %in% colnames(df)){
        df <- df %>% 
            mutate(stcounty_fips = str_sub(tract_fips, 1, 5))
    }
    
    if ("stcounty_fips" %in% colnames(df) & !("state_fips" %in% colnames(df)) 
        & !("county_fips" %in% colnames(df))) {
        df <- df %>%
            mutate(state_fips = str_sub(stcounty_fips, 0, 2),
                   county_fips = str_sub(stcounty_fips, 3, 5))
    }
    
    # create stcounty_fips if not already in the data
    if (("state_fips" %in% colnames(df)) & ("county_fips" %in% colnames(df)) 
        & !("stcounty_fips" %in% colnames(df))) {
        df <- df %>%
            mutate(stcounty_fips = paste(state_fips, county_fips, sep=""))
    }
    
    return(df)
}

##  ............................................................................
##  Fix 2020 county names and fips.                                         ####
##      Arguments:                                                            ##
##           df: a data frame with one or more geographic identifying columns.##                                 
##      Returns:                                                              ##
##           A data frame with corrected 2020 county information.             ##
##   ...........................................................................

fixCounties2020 <- function(df){
    # 2-digit state + 3 digit county
    if ("stcounty_fips" %in% colnames(df)) {
        df <- df %>%
            mutate(stcounty_fips = ifelse(stcounty_fips == 46113, 46102, 
                                          stcounty_fips),
                   stcounty_fips = ifelse(stcounty_fips == 51515, 51019, 
                                          stcounty_fips),
                   stcounty_fips = ifelse(stcounty_fips == 51560, 51005, 
                                          stcounty_fips))
    }
    
    # fix county names 
    if ("county_name" %in% colnames(df)) {
        df <- df %>% 
            mutate(county_name = ifelse(stcounty_fips == "46102", 
                                        "Oglala Lakota County", county_name),
                   county_name = ifelse(stcounty_fips == "11001", 
                                        "District of Columbia", county_name),
                   county_name = ifelse(stcounty_fips == "35013", 
                                        "Dona Ana County", county_name),
                   county_name = ifelse(stcounty_fips == "51560", 
                                        "Alleghany County", county_name))
       
    }
    return(df)
}

##  ............................................................................
##  Exclude Alaska, Hawaii, and the U.S. territories                        ####
##      Arguments:                                                            ##
##           df: a data frame with one or more geographic identifying columns.##                                 
##      Returns:                                                              ##
##           A data frame with only the contiguous 48 states.                 ##
##   ...........................................................................

excludeStates <- function(df) {
    # keep only the contiguous 48 states & the District of Columbia  
    if ("state_fips" %in% colnames(df)) {
        df <- df %>%
            filter(state_fips < "57" &
                   state_fips != "02" &
                   state_fips != "15")
    }
    
    if ("state_name" %in% colnames(df)) {
        df <- df %>% 
            filter(state_name != "Alaska" & 
                   state_name != "Hawaii" &
                   state_name != "United States Virgin Islands" &
                   state_name != "Commonwealth of the Northern Mariana Islands" &
                   state_name != "Guam" & 
                   state_name != "American Samoa" &
                   state_name != "Puerto Rico")
    } 
    
    if ("state_abbr" %in% colnames(df)) {
        df <- df %>% 
            filter(state_abbr != "AK",
                   state_abbr != "HI",
                   state_abbr != "VI",
                   state_abbr != "MP",
                   state_abbr != "GU",
                   state_abbr != "AS",
                   state_abbr != "PR")
    }
    return(df)
}

################################################################################ 
##                                                                            ##
##                                                                            ##
##                              Data specific functions                       ##
##                                                                            ##   
##                                                                            ##
################################################################################ 

##  ............................................................................
##  Refactor the USDA rural-urban values to three categories                ####
##      Arguments:                                                            ##
##           df: a data frame with a rural-urbaneographic identifying columns.##                                 
##      Returns:                                                              ##
##           A data frame with only the contiguous 48 states.                 ##
##   ...........................................................................

rural_urban <- function(df){
    df %>% mutate(rural_urban_5pt = case_when(rural_urban == 4 ~ 4,
                                                 rural_urban == 6 ~ 4,
                                                 rural_urban == 8 ~ 4,
                                                 rural_urban == 5 ~ 5,
                                                 rural_urban == 7 ~ 5,
                                                 rural_urban == 9 ~ 5,
                                                 .default = as.numeric(rural_urban)),
                  rural_urban_3pt = case_when(rural_urban == 2 ~ 1,
                                                  rural_urban == 3 ~ 1,
                                                  rural_urban == 4 ~ 2,
                                                  rural_urban == 6 ~ 2,
                                                  rural_urban == 8 ~ 2,
                                                  rural_urban == 5 ~ 3,
                                                  rural_urban == 7 ~ 3,
                                                  rural_urban == 9 ~ 3,
                                                  .default = 
                                                  as.numeric(rural_urban))) %>% 
        select(stcounty_fips, state_fips, county_fips, rural_urban, 
               rural_urban_5pt, rural_urban_3pt)
}

