## file description ############################################################
##                                                                            ##
## This file compiles summary statistics                                      ##
##                                                                            ##    
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##              
##          Algara-Sharif (2008-2020)                                         ##
##              Sharif (2021) "Replication Data for: Partisanship &           ##
##              Nationalization in American Elections: Evidence from          ##
##              Presidential, Senatorial, & Gubernatorial Elections in        ## 
##              the U.S. Counties, 1872-2020",                                ##      
##              https://doi.org/10.7910/DVN/DGUMFI                            ##      
##          American Community Survey (2010, 2015, 2020)                      ##
##              2006-2010 ACS > 2008                                          ##
##              2011-2015 ACS > 2012 Election                                 ##
##              2016-2020 ACS > 2016 & 2020 Elections                         ##
##          FEMA (2010-2023)                                                  ## 
##              https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2
##          Climate Change Vulnerability (2010-2023)                          ## 
##              https://github.com/wachiuphd/CVI                              ##
##          USDA Rural-Urban                                                  ## 
##          Incorporated/Census Designated Places (2023)                      ## 
##              IP/CDP csvs were saved using the tabulate intersection        ##
##              function in ArcGIS                                            ##
##          Virginia                                                          ##
##              https://www.census.gov/library/reference/code-lists/ansi.2020.html#cousub
##          Counties (2010, 2015, 2020)                                       ##
##              tigris                                                        ##
##          States                                                            ##
##              tigris                                                        ##   
##                                                                            ##
## Output:                                                                    ##    
##          /results/summaries/StateTranscripts.csv                           ##
##          /results/summaries/YearState.csv                                  ##    
##          /results/summaries/YearState_place.csv                            ##    
##          /results/summaries/YearState_type.csv                             ##    
##          /results/summaries/YearState_mentions.csv                         ##    
##          /results/summaries/prop_climate.csv                               ##    
##          /results/summaries/prop_globalWarming.csv                         ##    
##          /results/summaries/prop_ccgwMentions.csv                          ##    
##                                                                            ##
## ########################################################################## ## 


#   ____________________________________________________________________________
#   load libraries and data                                                 ####

library(tidyverse)
library(openxlsx)                                  # save tables
library(flextable)                                 # make tables
library(gt)                                        # summary rows

load("./data/modified/local_view/lv_clean_noTranscript.rdata")
load("./data/modified/all_data/allData_transcriptLevel.rdata")
load("./data/modified/all_data/allData_State.rdata")

#   ____________________________________________________________________________
#   transcripts available                                                   ####

##  ............................................................................
##  available transcripts: states                                           ####

stateTranscripts <- lvClean_noTranscript %>%
    group_by(state_name) %>% 
    count()

# write.csv(stateTranscripts, "./results/summaries/StateTranscripts.csv", row.names = FALSE)

##  ............................................................................
##  available transcripts: year and state                                   ####

yearState <- lvClean_noTranscript %>% 
    group_by(transcript_year, state_name) %>% 
    count()

# write.csv(yearState, "./results/summaries/YearState.csv", row.names = FALSE)

##  ............................................................................
##  available transcripts: place                                            ####

yearState_place <- lvClean_noTranscript %>%
    group_by(transcript_year, state_name, place_name) %>% 
    count()

# write.csv(yearState_place, "./results/summaries/YearState_place.csv",
#           row.names = FALSE)

##  ............................................................................
##  available transcripts: meeting type                                     ####

yearState_type <- lvClean_noTranscript %>%
    group_by(transcript_year, state_name, meeting_type) %>% 
    count()

# write.csv(yearState_type, "./results/summaries/YearState_type.csv",
#           row.names = FALSE)

##  ............................................................................
##  availble transcripts: year, state, mentions                             ####

yearState_mentions <- allData_state %>% 
    select(state_name, transcript_year, n_countiesInState, state_n_transcripts, 
           state_n_scriptCC, state_n_scriptGW, state_n_scriptCCGW)

# write.csv(yearState_mentions,
#           "./results/summaries/YearState_mentions.csv",
#           row.names = FALSE)

#   ____________________________________________________________________________
#   balanced panel check                                                    ####

## number of counties in each state-year with a transcript
n_transcripts_year <- lvClean_noTranscript %>% 
    group_by(stcounty_fips) %>% 
    mutate(n_transcripts_year = n()) %>% 
    ungroup() 

## balanced panel check
table(n_transcripts_year$n_transcripts_year)


#   ____________________________________________________________________________
#   proportion: mentions                                                    ####

## count and proportion of climate change use by year
prop_mentions <- lvClean_noTranscript %>% 
    select(transcript_year, stcounty_fips, n_ccMentions, ccBinary, n_gwMentions,
           gwBinary, ccgwBinary) %>% 
    group_by(transcript_year) %>% 
    mutate(year_nCounties = n_distinct(stcounty_fips),                          # total distinct counties in the LV data by year
           year_n_transcripts = n(),                                            # total number of transcripts in a year
           year_nScriptCC = sum(ccBinary),                                      # number of transcripts in a year with at least one mention of climate change
           year_nCCmentions = sum(n_ccMentions),                                # total number of climate change mentions in a year
           year_propScriptCC = year_nScriptCC/year_n_transcripts,               # proportion of transcripts with at least one climate change mention
           year_propCCmentions = year_nCCmentions/year_n_transcripts,           # proportion of all mentions of climate change in a given year
           year_ccBinary = ifelse(ccBinary > 0, 1, 0),                          # binary indicator of whether there's at least one mention of climate change in that year
           year_nScriptGW = sum(gwBinary),
           year_nGWmentions = sum(n_gwMentions),
           year_propScriptGW = year_nScriptGW/year_n_transcripts,
           year_gwBinary = ifelse(gwBinary > 0, 1, 0)) 

##  ............................................................................
##  proportion: climate                                                     ####

prop_climate <- prop_mentions %>%                      
    filter(year_ccBinary == 1) %>%  
    mutate(n_countiesCCmention = n_distinct(stcounty_fips),                     # number of unique counties that mention climate change in a year 
           prop_countiesCCMention = n_countiesCCmention/year_nCounties) %>%     # proportion of counties in the data that mention climate change at least once
    select(transcript_year, starts_with("year_")) %>% 
    distinct(transcript_year, .keep_all = TRUE)

# write.csv(prop_climate, "./results/summaries/prop_climate.csv",
#           row.names = FALSE)

##  ............................................................................
##  proportion: global warming                                              ####

prop_globalWarming <- prop_mentions %>% 
    filter(year_gwBinary == 1) %>%  
    mutate(n_countiesGWmention = n_distinct(stcounty_fips),                     # number of unique counties that mention global warming in a year 
           prop_countiesGWMention = n_countiesGWmention/year_nCounties) %>%     # proportion of counties in the data that mention global warming at least once
    select(transcript_year, starts_with("year_")) %>% 
    distinct(transcript_year, .keep_all = TRUE)

# write.csv(prop_globalWarming, "./results/summaries/prop_globalWarming.csv",
#           row.names = FALSE)

##  ............................................................................
##  proportion: climate change & global warming                             ####

prop_ccgwMentions <- prop_mentions %>% 
    filter(year_ccBinary == 1 | year_gwBinary == 1) %>% 
    mutate(n_countiesCCGWmention = n_distinct(stcounty_fips),
           prop_countiesCCGWmention = n_countiesCCGWmention/year_nCounties) %>% 
    select(transcript_year, starts_with("year_")) %>% 
    distinct(transcript_year, .keep_all = TRUE)

# write.csv(prop_ccgwMentions, "./results/summaries/prop_ccgwMentions.csv",
#           row.names = FALSE)


#   ____________________________________________________________________________
#   prop tables by county                                                   ####

for(state in unique(lvClean_noTranscript$state_name)){
    s <- lvClean_noTranscript %>% 
        filter(state_name == state) %>% 
        group_by(transcript_year, county_name) %>% 
        summarize(n_script = n(),
                  n_script_ccMention = sum(ccBinary),
                  total_ccMention = sum(n_ccMentions)) %>% 
        ungroup() %>% 
        mutate(n_distinct_counties = n_distinct(county_name),
               n_script_ccMention = paste("(", n_script_ccMention, ")", sep = "")) %>% 
        group_by(transcript_year) %>% 
        mutate(n_counties_inYear = n_distinct(county_name),
               nScript_nCC = paste(n_script, n_script_ccMention, sep = " "))
    
    unique_counties <- s %>%
        select(transcript_year, n_counties_inYear) %>%
        distinct(transcript_year, .keep_all = TRUE) %>%
        pivot_wider(names_from = transcript_year, 
                    values_from = n_counties_inYear) %>% 
        mutate(across(where(is.numeric), as.character))
    
    n_unique_allYears <- s %>% 
        ungroup() %>% 
        select(n_distinct_counties) %>% 
        distinct(n_distinct_counties)
    
    n_counties_inState <- allData_transcriptLevel %>% 
        filter(state_name == state) %>% 
        select(n_countiesInState) %>% 
        distinct(n_countiesInState)
    
    s_table <- s %>%
        select(transcript_year, county_name, nScript_nCC) %>% 
        pivot_wider(names_from = transcript_year, values_from = nScript_nCC) %>% 
        mutate(across(where(is.numeric), as.character)) %>% 
        bind_rows(unique_counties) %>% 
        mutate(county_name = ifelse(is.na(county_name), 
                                    "Unique Counties in Year", county_name)) %>% 
        gt(rowname_col = "county_name") %>% 
        tab_header(
            title = md(paste("**", state, "**", sep="")),
            subtitle = "Transcripts & Climate Change Mentions") %>% 
        tab_source_note(
            source_note = "Number of transcripts in county-year 
            (Number of transcripts with at least one climate change mention) 
            in parentheses") %>%
        tab_source_note("`---` represents no transcripts 
                        in that county-year") %>% 
        tab_source_note(paste("Number of Counties in State:", 
                              n_counties_inState)) %>% 
        tab_source_note(paste("Number of Unique Counties in All Years:",
                              n_unique_allYears)) %>% 
        tab_stubhead(label = "County Name") %>% 
        sub_missing(rows = everything(), 
                    missing_text = "---") 
    
        gtsave(s_table, file = paste("./results/summaries/state_tables/",
                                     state, ".docx"))
    
}
