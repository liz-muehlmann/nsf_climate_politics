### file description ###########################################################
##                                                                            ##
## This file handles the processing steps necessary for the USDA Rural        ##
##      Urban designations                                                    ##
##      Data included:                                                        ##
##          USDA Rural-Urban (2003, 2013, 2023)                               ## 
##              https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/documentation/
##                                                                            ## 
## Output:                                                                    ##
##  /LocalView/data/modified/rural_urban.rdata                                ##
##                                                                            ## 
################################################################################ 

#   ____________________________________________________________________________
#   load libraries and custom functions                                     ####

source("./code/preliminaries/analysis_prelims.r")       

#   ____________________________________________________________________________    
#   USDA Rural-Urban Designations                                           ####            

ru03 <- read_excel("./data/original/2020_USDA_RuralUrban/2003USDA_RuralUrbanCodes.xls") %>% 
    rename(stcounty_fips = "FIPS Code",
           rural_urban = "2003 Rural-urban Continuum Code",
           state_abbr = State) %>% 
    fixCounties2020() %>% 
    createFips() %>% 
    excludeStates() %>% 
    rural_urban() %>% 
    mutate(ru_year = 2003) %>% 
    filter(stcounty_fips != "51515") %>% 
    distinct(stcounty_fips, .keep_all=TRUE)

ru13 <- read_excel("./data/original/2020_USDA_RuralUrban/2013USDA_ruralUrbanCodes.xls") %>% 
    rename(stcounty_fips = FIPS,
           rural_urban = RUCC_2013) %>% 
    fixCounties2020() %>% 
    createFips() %>% 
    excludeStates() %>% 
    rural_urban() %>% 
    mutate(ru_year = 2013) %>% 
    filter(stcounty_fips != "51515") %>% 
    distinct(stcounty_fips, .keep_all=TRUE)

ct <- ru13 %>% 
    slice(which(state_fips == "09")) %>% 
    mutate(ru_year = 2023)

ru23 <- read_excel("./data/original/2020_USDA_RuralUrban/2023USDA_ruralUrbanCodes.xlsx") %>% 
    rename(stcounty_fips = FIPS,
           rural_urban = RUCC_2023) %>% 
    fixCounties2020() %>% 
    createFips() %>% 
    filter(state_fips != "09") %>% 
    rural_urban() %>%
    excludeStates() %>% 
    mutate(ru_year = 2023) %>% 
    bind_rows(ct)

ruralUrban <- rbind(ru03, ru13, ru23)


# save(ruralUrban, file = "./data/modified/rural_urban.rdata")


##  ............................................................................
##  rural-urban 

sample_ru <- ruralUrban[sample(nrow(ruralUrban), 50), ]
# write.csv(sample_ru, "./results/samples/usda_rural_urban.csv", row.names = FALSE)

