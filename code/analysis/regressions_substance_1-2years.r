################################################################################
##                                                                            ##
##                          Substantive Regressions                           ##
##                                                                            ##
##  Data: transcript-level local view data with fema/ noaa information        ##
##  Model: climate change or global warming binary (DV)                       ##
##         ~ Days since FEMA declaration or NOAA episode                      ##
##  DV: binary indicator for if there was a CC or GW mention                  ##
##  IV: Days since disaster declaration or episode, last one or two years     ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load preliminaries and data                                             ####

source("./LocalView/code/analysis_scripts/regression_preliminaries.r")
load("./LocalView/data/modified/merged_datasets/allData_transcriptLevel.rdata")

allData_transcriptLevel <- allData_transcriptLevel %>% 
    mutate(log_totalPop = log(total_pop),
           log_medhhic = log(med_hhic),
           party_lean = ifelse(round(DVP, 2) <= .50, 
                               "Leans Republican", "Leans Democratic"),
           anyDec_oneYear = ifelse(nDec_oneYear > 0, 1, 0),
           anyEpisode_oneYear = ifelse(nEpisode_oneYear > 0, 1, 0),
           anyDec_oneYear = ifelse(nDec_oneYear > 0, 1, 0),
           anyEpisode_oneYear = ifelse(nEpisode_oneYear > 0, 1, 0),
           anyDec_twoYears = ifelse(nDec_twoYears > 0, 1, 0),
           anyEpisode_twoYears = ifelse(nEpisode_twoYears > 0, 1, 0),
           transcript_year = as.numeric(transcript_year)) 

#   ____________________________________________________________________________
#   table design                                                            ####

set_flextable_defaults(
    font.size = 12, 
    font.family = "Garamond",
    line_spacing = 1,
    padding = 0,
    table.layout = "autofit")

#   ____________________________________________________________________________
##  rq3.1 - ARE THERE MORE MENTIONS OF CC/GW AFTER NATURAL DISASTERS/EXTREME EVENTS? ####
##                                                                            ##
##                               ONE YEAR                                     ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  rq3.1 - Main Models                                                       ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq3.1 - Between effects; Any Declaration                                 ####
###  linear time; county-level controls; county clustered S.E.
rq3.1_anyDec_oneYear <- felm(ccgwBinary ~ anyDec_oneYear + DVP + rural_urban_3pt + 
                       log(total_pop) + log(med_hhic) + perc_white + 
                       edu_percentPop + transcript_year + overall_cvi + 
                       census_division|0|0|stcounty_fips, 
                   data = allData_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq3.1 - Between effects; Number of Declarations                          ####
###  linear time; county-level controls; county clustered S.E. 

rq3.1_nDec_oneYear <- felm(ccgwBinary ~ nDec_oneYear + DVP + rural_urban_3pt + 
                     log(total_pop) + log(med_hhic) + perc_white + 
                     edu_percentPop + transcript_year + overall_cvi + 
                     census_division|0|0|stcounty_fips, 
                 data=allData_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq3.1 - Between effects; Any Episode                                     ####
###  linear time; county-level controls; county clustered S.E.
rq3.1_anyEpisode_oneYear <- felm(ccgwBinary ~ anyEpisode_oneYear + DVP + rural_urban_3pt + 
                           log(total_pop) + log(med_hhic) + perc_white + 
                           edu_percentPop + transcript_year + overall_cvi + 
                           census_division|0|0|stcounty_fips, 
                       data = allData_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq3.1 - Between effects; Number of Episodes                          ####
###  linear time; county-level controls; county clustered S.E. 

rq3.1_nEpisode_oneYear <- felm(ccgwBinary ~ nEpisode_oneYear + DVP + rural_urban_3pt + 
                         log(total_pop) + log(med_hhic) + perc_white + 
                         edu_percentPop + transcript_year + overall_cvi + 
                         census_division|0|0|stcounty_fips, 
                     data=allData_transcriptLevel)


## Table 3.1: Are there more mentions of CC/GW after natural disasters/extreme events?
modelsummary(list(rq3.1_anyDec_oneYear, rq3.1_nDec_oneYear, rq3.1_anyEpisode_oneYear, rq3.1_nEpisode_oneYear),
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 3.1: Is a place more likely to mention climate change/global warming after natural disasters/extreme events?",
             output = "flextable") %>% 
    fema_noaa_header() #%>%
    # save_as_docx(path = "./LocalView/results/regressions/substance/250129_rq3.1_Table3_oneYear.docx")


#   ____________________________________________________________________________
##  rq3.2 - ARE THERE MORE MENTIONS OF CC/GW AFTER NATURAL DISASTERS/EXTREME EVENTS? ####
##                                                                            ##
##                               TWO YEARS                                    ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  rq3.2 - Main Models                                                       ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq3.2 - Between effects; Any Declaration                                 ####
###  linear time; county-level controls; county clustered S.E.
rq3.2_anyDec_twoYears <- felm(ccgwBinary ~ anyDec_twoYears + DVP + rural_urban_3pt + 
                                 log(total_pop) + log(med_hhic) + perc_white + 
                                 edu_percentPop + transcript_year + overall_cvi + 
                                 census_division|0|0|stcounty_fips, 
                             data = allData_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq3.2 - Between effects; Number of Declarations                          ####
###  linear time; county-level controls; county clustered S.E. 

rq3.2_nDec_twoYears <- felm(ccgwBinary ~ nDec_twoYears + DVP + rural_urban_3pt + 
                               log(total_pop) + log(med_hhic) + perc_white + 
                               edu_percentPop + transcript_year + overall_cvi + 
                               census_division|0|0|stcounty_fips, 
                           data=allData_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq3.2 - Between effects; Any Episode                                     ####
###  linear time; county-level controls; county clustered S.E.
rq3.2_anyEpisode_twoYears <- felm(ccgwBinary ~ anyEpisode_twoYears + DVP + rural_urban_3pt + 
                                     log(total_pop) + log(med_hhic) + perc_white + 
                                     edu_percentPop + transcript_year + overall_cvi + 
                                     census_division|0|0|stcounty_fips, 
                                 data = allData_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq3.2 - Between effects; Number of Episodes                          ####
###  linear time; county-level controls; county clustered S.E. 

rq3.2_nEpisode_twoYears <- felm(ccgwBinary ~ nEpisode_twoYears + DVP + rural_urban_3pt + 
                                   log(total_pop) + log(med_hhic) + perc_white + 
                                   edu_percentPop + transcript_year + overall_cvi + 
                                   census_division|0|0|stcounty_fips, 
                               data=allData_transcriptLevel)


## Table 3.1: Are there more mentions of CC/GW after natural disasters/extreme events?
modelsummary(list(rq3.2_anyDec_twoYears, rq3.2_nDec_twoYears, rq3.2_anyEpisode_twoYears, rq3.2_nEpisode_twoYears),
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 3.2: Is a place more likely to mention climate change/global warming after natural disasters/extreme events?",
             output = "flextable") %>% 
    fema_noaa_header() #%>%
# save_as_docx(path = "./LocalView/results/regressions/substance/250129_rq3.2_Table3.2_twoYears.docx")

#   ____________________________________________________________________________
##  rq4.1 - DO EXTREME WEATHER EVENTS CLOSE THE PARTISAN GAP IN MENTIONS?     ####
##                                                                            ##
##                                 ONE YEAR                                   ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  rq4.1 - Main Models                                                       ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq4.1 - between effects with DVP; any dec                                ####
###  DVP*anyDec_oneYear; county-level controls; county clustered S.E.

rq4.1_anyDec_oneYeardvp <- felm(ccgwBinary ~ anyDec_oneYear*DVP + rural_urban_3pt + 
                          log(total_pop) + log(med_hhic) + perc_white + 
                          edu_percentPop + transcript_year + overall_cvi +
                          census_division|0|0|stcounty_fips, 
                      data=allData_transcriptLevel)

rq4.1_anyEpidvp <- felm(ccgwBinary ~ anyEpisode_oneYear*DVP + rural_urban_3pt + 
                          log(total_pop) + log(med_hhic) + perc_white + 
                          edu_percentPop + transcript_year + overall_cvi +
                          census_division|0|0|stcounty_fips, 
                      data=allData_transcriptLevel)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq4.1 - between effects with DVP; n dec                                  ####
###  DVP*ndec; county-level controls; county clustered S.E.

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
rq4.1_nDec_oneYeardvp <- felm(ccgwBinary ~ nDec_oneYear*DVP + rural_urban_3pt + 
                        log(total_pop) + log(med_hhic) + perc_white + 
                        edu_percentPop + transcript_year + overall_cvi +
                        census_division|0|0|stcounty_fips, 
                    data=allData_transcriptLevel)

rq4.1_nEpisodedvp <- felm(ccgwBinary ~ nEpisode_oneYear*DVP + rural_urban_3pt + 
                            log(total_pop) + log(med_hhic) + perc_white + edu_percentPop + 
                            transcript_year + overall_cvi +
                            census_division|0|0|stcounty_fips, 
                        data=allData_transcriptLevel)

## Table 4.1: Do extreme weather events close the partisan gap in mentions?
modelsummary(list(rq4.1_anyDec_oneYeardvp, rq4.1_nDec_oneYeardvp, rq4.1_anyEpidvp, rq4.1_nEpisodedvp),
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 4.1: Do extreme weather events close the partisan gap in mentions?",
             output = "flextable") %>% 
    fema_noaa_header() #%>%
    # save_as_docx(path = "./LocalView/results/regressions/substance/250129_rq4.1_Table4.1_oneYear.docx")


#   ____________________________________________________________________________
##  rq4.2 - DO EXTREME WEATHER EVENTS CLOSE THE PARTISAN GAP IN MENTIONS?     ####
##                                                                            ##
##                                 ONE YEAR                                   ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  rq4.2 - Main Models                                                       ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq4.2 - between effects with DVP; any dec                                ####
###  DVP*anyDec_twoYears; county-level controls; county clustered S.E.

rq4.2_anyDec_twoYearsdvp <- felm(ccgwBinary ~ anyDec_twoYears*DVP + rural_urban_3pt + 
                                    log(total_pop) + log(med_hhic) + perc_white + 
                                    edu_percentPop + transcript_year + overall_cvi +
                                    census_division|0|0|stcounty_fips, 
                                data=allData_transcriptLevel)

rq4.2_anyEpidvp <- felm(ccgwBinary ~ anyEpisode_twoYears*DVP + rural_urban_3pt + 
                            log(total_pop) + log(med_hhic) + perc_white + 
                            edu_percentPop + transcript_year + overall_cvi +
                            census_division|0|0|stcounty_fips, 
                        data=allData_transcriptLevel)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  rq4.2 - between effects with DVP; n dec                                  ####
###  DVP*ndec; county-level controls; county clustered S.E.

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
rq4.2_nDec_twoYearsdvp <- felm(ccgwBinary ~ nDec_twoYears*DVP + rural_urban_3pt + 
                                  log(total_pop) + log(med_hhic) + perc_white + 
                                  edu_percentPop + transcript_year + overall_cvi +
                                  census_division|0|0|stcounty_fips, 
                              data=allData_transcriptLevel)

rq4.2_nEpisodedvp <- felm(ccgwBinary ~ nEpisode_twoYears*DVP + rural_urban_3pt + 
                              log(total_pop) + log(med_hhic) + perc_white + edu_percentPop + 
                              transcript_year + overall_cvi +
                              census_division|0|0|stcounty_fips, 
                          data=allData_transcriptLevel)

## Table 4.1: Do extreme weather events close the partisan gap in mentions?
modelsummary(list(rq4.2_anyDec_twoYearsdvp, rq4.2_nDec_twoYearsdvp, rq4.2_anyEpidvp, rq4.2_nEpisodedvp),
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 4.2: Do extreme weather events close the partisan gap in mentions?",
             output = "flextable") %>% 
    fema_noaa_header() #%>%
    # save_as_docx(path = "./LocalView/results/regressions/substance/250129_rq4.2_Table4.2_twoYears.docx")


