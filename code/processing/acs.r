### file description ###########################################################
##                                                                            ##
## This file handles the processing steps necessary for the American          ##
##      Community Survey                                                      ##
##      Data included:                                                        ##
##          American Community Survey (2010, 2015, 2020)                      ##
##              2006-2010 ACS > 2008                                          ##
##              2011-2015 ACS > 2012 Election                                 ##
##              2016-2020 ACS > 2016 & 2020 Elections                         ##
## Output:                                                                    ##  
##      /LocalView/data/modified/acs.rdata                                    ##
##                                                                            ##    
## ########################################################################## ## 

#   ____________________________________________________________________________
#   load libraries and custom functions                                     ####

source("./code/preliminaries/analysis_prelims.r")      

#   ____________________________________________________________________________    
#   American Community Survey                                               ####            

##  ............................................................................
##  define ACS years & variables to select                                  ####

acs_years <- list(2010, 2015, 2020)
acs_vars <- c("B02001_002E",        # white
             "B02001_003E",         # Black
             "B02001_004E",         # American Indian
             "B02001_005E",         # Asian
             "B02001_006E",         # Native Hawaiian, Pacific Islander
             "B03001_003E",         # Hispanic
             "B01002_001E",         # median age
             "B25064_001E",         # median gross rent
             "B19001_001E",         # median house hold income
             "B01003_001E",         # total population     
             "B15002_002E",         # over 25 male population, education total
             "B15002_006E",         # over 25 male, no high school
             "B15002_010E",         # over 25 male, high school
             "B15002_014E",         # over 25 male, associate's
             "B15002_015E",         # over 25 male, bachelor's
             "B15002_016E",         # over 25 male, master's
             "B15002_017E",         # over 25 male, professional degree
             "B15002_018E",         # over 25 male, doctorate
             "B15002_019E",         # over 25 female population, education total
             "B15002_023E",         # over 25 female, no high school
             "B15002_028E",         # over 25 female, high school
             "B15002_031E",         # over 25 female, associate's
             "B15002_032E",         # over 25 female, bachelor's
             "B15002_033E",         # over 25 female, master's
             "B15002_034E",         # over 25 female, professional degree
             "B15002_035E")         # over 25 female, doctorate

##  ............................................................................
##  download and clean ACS years (2010, 2015, 2020)                         ####

acs <- list()

for(y in acs_years) {
    df <- get_acs(geography = "county", year = y, geometry = FALSE, 
                  variables = acs_vars) %>% 
        select(-moe) %>% 
        rename(stcounty_fips = GEOID) %>% 
        separate(NAME, into = c("county_name", "state_name"), sep = ",") %>% 
        mutate(state_name = str_trim(state_name),
               var_names = 
                   case_when(variable == "B02001_002" ~ "white",
                             variable == "B02001_003" ~ "black",              
                             variable == "B02001_004" ~ "amind",              
                             variable == "B02001_005" ~ "asian",              
                             variable == "B02001_006" ~ "nhapi",              
                             variable == "B03001_003" ~ "hispanic",           
                             variable == "B01002_001" ~ "med_age",             
                             variable == "B25064_001" ~ "med_grossRent",       
                             variable == "B19001_001" ~ "med_hhic",            
                             variable == "B01003_001" ~ "total_pop",     
                             variable == "B15002_014" ~ "eduM_aa",      
                             variable == "B15002_015" ~ "eduM_ba",      
                             variable == "B15002_016" ~ "eduM_ma",      
                             variable == "B15002_017" ~ "eduM_prof",    
                             variable == "B15002_018" ~ "eduM_phd",     
                             variable == "B15002_031" ~ "eduF_aa",      
                             variable == "B15002_032" ~ "eduF_ba",      
                             variable == "B15002_033" ~ "eduF_ma",      
                             variable == "B15002_034" ~ "eduF_prof",    
                             variable == "B15002_035" ~ "eduF_phd",
                             TRUE ~ as.character(variable)),
               state_name = str_trim(state_name)) %>%
        excludeStates() %>% 
        createFips() %>% 
        select(-variable) %>% 
        pivot_wider(values_from = estimate,
                    names_from = var_names) %>%
        mutate(edu_percentPop = (eduM_aa + eduF_aa + eduM_ba + eduF_ba + 
                                     eduM_ma + eduF_ma + 
                                     eduM_prof + eduF_prof + eduM_phd + 
                                     eduF_phd)/total_pop,
               perc_white = white/total_pop,
               perc_black = black/total_pop,
               perc_hispanic = hispanic/total_pop,
               other = amind + asian + nhapi,
               perc_other = other/total_pop,
               med_hhic = med_hhic/10000,
               acs_year = y) %>%
        select(stcounty_fips, state_name, acs_year, med_age, total_pop, 
               med_grossRent, med_hhic, perc_white, perc_black, perc_hispanic, 
               perc_other, edu_percentPop) 
    
    assign(paste("acs", y, sep=""), df)
}

acs2010 <- acs2010 %>% 
    filter(stcounty_fips != 51515) %>% 
    fixCounties2020()


acs <- rbind(acs2010, acs2015, acs2020)

# save(acs, file = "./data/modified/acs.rdata")


##  ............................................................................
##  ACS Sample                                                              ####

sample_acs <- acs %>% sample_n(50)
# write.csv(sample_acs, "./results/samples/acs_sample.csv", row.names = FALSE)






