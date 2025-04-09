################################################################################
##                                                                            ##
##                          Substantive Regressions                           ##
##                                                                            ##
##  Data: transcript-level local view data with fema/ noaa information        ##
##  Model: climate change or global warming binary (DV)                       ##
##         ~ Days since FEMA declaration or NOAA episode                      ##
##  DV: binary indicator for if there was a CC or GW mention                  ##
##  IV: Days since disaster declaration or episode, last five years           ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load preliminaries and data                                             ####

source("./code/preliminaries/regression_preliminaries.r")
load("./data/modified/all_data/allData_transcriptLevel.rdata")

allData_transcriptLevel <- allData_transcriptLevel %>% 
    mutate(log_totalPop = log(total_pop),
           log_medhhic = log(med_hhic),
           party_lean = ifelse(round(DVP, 2) <= .50, 
                               "Leans Republican", "Leans Democratic"),
           anyDec_fiveYears = ifelse(nDec_fiveYears > 0, 1, 0),
           anyEpisode_fiveYears = ifelse(nEpisode_fiveYears > 0, 1, 0),
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
##  RQ1 - IS CLIMATE CHANGE BEING DISCUSSED & HOW                           ####
##                                                                            ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  RQ1 - Main Model                                                        ####
##  Between effects; linear time; county-level controls; county clustered S.E.

rq1 <- felm(ccgwBinary ~ rural_urban_3pt + log(total_pop) + log(med_hhic) + 
                perc_white + edu_percentPop + transcript_year + overall_cvi +
                census_division|0|0|stcounty_fips, data=allData_transcriptLevel)

##  Table 1: Is Climate Change Being Discussed?
modelsummary(rq1,
             coef_map = all_coefs,
             stars = stars,  
             gof_map = gof_felm,         
             glance = glance_custom.felm,
             title = "Table 1: Is Climate Change Being Mentioned?",
             output = "flextable") %>% 
    add_header_row(values = c("",
                              "Dependent Variable: 
                              Climate Change/Global Warming Mention"),
                     colwidths = c(1,1)) %>% 
    align(align = c("center"), part = "header") #%>% 
    # save_as_docx(path = "./results/regressions/250324_rq1_Table1.docx")

##  ............................................................................
##  RQ1 - ROBUSTNESS CHECKS                                                 ####
############################################################################# ##

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ1.1 - Penalized Maximum Likelihood                                    ####

rq1.1 <- glm(ccgwBinary ~ rural_urban_3pt + log_totalPop + log_medhhic +
                 perc_white + edu_percentPop + transcript_year + 
                 overall_cvi + census_division, 
             data=allData_transcriptLevel, family = "binomial", method = brglmFit)

## Table 1.1: Penalized Maximum Likelihood
modelsummary(rq1.1,
             coef_map = all_coefs,
             stars = stars,  
             gof_map = gof_pml,
             title = "Table 1.1: Penalized Maximum Likelihood",
             output = "flextable") %>% 
    dv_header()  #%>% 
    # save_as_docx(path = "./results/regressions/250324_rq1_Table1-1_PML.docx")
             


## Table 1.1a: Penalized Maximum Likelihood Marginal Effects - Average Slopes
rq1.1a <- marginaleffects::avg_slopes(rq1.1, 
                                      variables = c("rural_urban_3pt", 
                                                    "log_totalPop",  
                                                    "log_medhhic", 
                                                    "perc_white",
                                                    "edu_percentPop",    
                                                    "transcript_year",
                                                    "overall_cvi")) 

rq1.1a %>% 
    fmt_me()  #%>%
    # save_as_docx(path = "./results/regressions/250324_rq1_Table1-1a_ME.docx")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ1.2 - Time as a Factor Variable                                      ####

##  Between effects; time as a factor; county-level controls; county clustered S.E.
rq1.2 <- felm(ccgwBinary ~ rural_urban_3pt + log(total_pop) + 
                  log(med_hhic) + perc_white + edu_percentPop + overall_cvi +
                  as.factor(transcript_year) + census_division|0|0|stcounty_fips, 
              data=allData_transcriptLevel)

## Table 1.2 - Is Climate Change Being Discussed (Time as a Factor)?
modelsummary(rq1.2,
             coef_map = all_coefs,
             stars = stars,  
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 1.2: Is Climate Change Being Discussed (Linear Time)?",
             output = "flextable") %>% 
    dv_header() #%>% 
    # save_as_docx(path = "./results/regressions/250324_rq1_Table1-2.docx")

#   ____________________________________________________________________________
##  RQ2 - DOES CC/GW MENTION VARY BY VOTE SHARE?                            ####
##                                                                            ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  RQ2 - Main Models                                                       ####
##  Between effects with DVP; linear time; county-level controls; county clustered S.E.
##  Within county variation means that CVI is completely co-linear because
##     CVI only has one value in the dataset = time invariant 
rq2 <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + 
                perc_white + edu_percentPop + transcript_year + overall_cvi +
                census_division|0|0|stcounty_fips, 
            data=allData_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ2 - Between effects with DVP; Linear Time                                   ####
##  DVP*time Interaction; county-level controls; county clustered S.E.

rq2_int_linear <- felm(ccgwBinary ~ DVP*transcript_year + rural_urban_3pt + 
                           log(total_pop) + log(med_hhic) + perc_white + 
                           edu_percentPop + transcript_year + overall_cvi +
                           census_division|0|0|stcounty_fips, 
                       data=allData_transcriptLevel)

## Table 2: Do Climate Change/Global Warming Mentions Vary by Vote Percentage?
modelsummary(list(rq2, rq2_int_linear),
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 2: Do Climate Change/Global Warming Mentions Vary by Vote Percentage?",
             output = "flextable") %>%
    add_header_row(values = c("",
                              "Dependent Variable: Climate Change/Global Warming Mention"),
                   colwidths = c(1,2)) %>% 
    align(align = c("center"), part = "header") %>% 
    padding(padding = 0, part = "all") #%>% 
    # save_as_docx(path = "./results/regressions/250324_rq2_Table2.docx")

##  ............................................................................
##  RQ2 - ROBUSTNESS CHECKS                                                 ####
############################################################################# ##

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ2.1 - Within Effects; County FE; Linear Time                          ####
##   county-level controls; county clustered S.E.

rq2.1 <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + 
                  perc_white + overall_cvi + edu_percentPop + overall_cvi +
                  transcript_year|stcounty_fips|0|stcounty_fips, 
              data=allData_transcriptLevel)

## Table 2.1 Do Climate Change/Global Warming Mentions Vary by Vote Percentage?
modelsummary(rq2.1,
             coef_map = all_coefs,
             stars = stars,  
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 2.1: Within Effects with County Fixed Effects",
             output = "flextable") %>% 
    dv_header() #%>% 
    # save_as_docx(path = "./results/regressions/250324_rq2_Table2-1.docx")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ2.2 - Penalized Maximum Likelihood                                    ####

rq2.2 <- glm(ccgwBinary ~ DVP +  rural_urban_3pt + log_totalPop + log_medhhic +
                 perc_white + edu_percentPop + transcript_year + overall_cvi,
             data=allData_transcriptLevel,
             family = "binomial", method = brglmFit)

## Table 2.2: Penalized Maximum Likelihood
modelsummary(rq2.2,
             coef_map = all_coefs,
             stars = stars,  
             gof_map = gof_pml,
             # glance = glance_custom.pml,
             title = "Table 2.2: Penalized Maximum Likelihood",
             output = "flextable") %>% 
    dv_header()  #%>% 
    # save_as_docx(path = "./results/regressions/250324_rq2_Table2-2.docx")

## Table 2.1a: Penalized Maximum Likelihood Marginal Effects - Average Slopes
rq2.2a <- marginaleffects::avg_slopes(rq2.2, 
                                     variables = c("DVP", 
                                                   "rural_urban_3pt", 
                                                   "log_totalPop",
                                                   "log_medhhic",
                                                   "perc_white",
                                                   "edu_percentPop",
                                                   "transcript_year",
                                                   "overall_cvi"))

rq2.2a %>%
    fmt_me() #%>% 
    # save_as_docx(path = "./results/regressions/250324_rq2_Table 2-2a_ME.docx")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ2.3 - Between effects with DVP; Factor time                                   ####
##  DVP*time Interaction; county-level controls; county clustered S.E.
rq2.3 <- felm(ccgwBinary ~ DVP*as.factor(transcript_year) + rural_urban_3pt + 
                  log(total_pop) + log(med_hhic) + perc_white + 
                  edu_percentPop + overall_cvi + 
                  as.factor(transcript_year)|0|0|stcounty_fips, 
              data=allData_transcriptLevel)

modelsummary(rq2.3,
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             # glance = glance_custom.pml,
             title = "Table 2.3: Between Effects, Time as a Factor",
             output = "flextable") %>% 
    dv_header() #%>% 
    # save_as_docx(path = "./results/regressions/250324_rq2_Table2-3.docx")

#   ____________________________________________________________________________
##  RQ3 - ARE THERE MORE MENTIONS OF CC/GW AFTER NATURAL DISASTERS/EXTREME EVENTS? ####
##                                                                            ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  RQ3 - Main Models                                                       ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ3 - Between effects; Any Declaration                                 ####
###  linear time; county-level controls; county clustered S.E.
rq3_anyDec <- felm(ccgwBinary ~ anyDec_fiveYears + DVP + rural_urban_3pt + 
                       log(total_pop) + log(med_hhic) + perc_white + 
                       edu_percentPop + transcript_year + overall_cvi + 
                       census_division|0|0|stcounty_fips, 
                   data = allData_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ3 - Between effects; Number of Declarations                          ####
###  linear time; county-level controls; county clustered S.E. 

rq3_nDec <- felm(ccgwBinary ~ nDec_fiveYears + DVP + rural_urban_3pt + 
                     log(total_pop) + log(med_hhic) + perc_white + 
                     edu_percentPop + transcript_year + overall_cvi + 
                     census_division|0|0|stcounty_fips, 
                 data=allData_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ3 - Between effects; Any Episode                                     ####
###  linear time; county-level controls; county clustered S.E.
rq3_anyEpisode <- felm(ccgwBinary ~ anyEpisode_fiveYears + DVP + rural_urban_3pt + 
                           log(total_pop) + log(med_hhic) + perc_white + 
                           edu_percentPop + transcript_year + overall_cvi + 
                           census_division|0|0|stcounty_fips, 
                       data = allData_transcriptLevel)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ3 - Between effects; Number of Episodes                          ####
###  linear time; county-level controls; county clustered S.E. 

rq3_nEpisode <- felm(ccgwBinary ~ nEpisode_fiveYears + DVP + rural_urban_3pt + 
                         log(total_pop) + log(med_hhic) + perc_white + 
                         edu_percentPop + transcript_year + overall_cvi + 
                         census_division|0|0|stcounty_fips, 
                     data=allData_transcriptLevel)


## Table 3: Are there more mentions of CC/GW after natural disasters/extreme events?
modelsummary(list(rq3_anyDec, rq3_nDec, rq3_anyEpisode, rq3_nEpisode),
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 3: Is a place more likely to mention climate change/global warming after natural disasters/extreme events?",
             output = "flextable") %>% 
    fema_noaa_header() #%>%
    # save_as_docx(path = "./results/regressions/250324_rq3_Table3.docx")

#   ____________________________________________________________________________
##  RQ4 - DO EXTREME WEATHER EVENTS CLOSE THE PARTISAN GAP IN MENTIONS?     ####
##                                                                            ##
##                                                                            ##
## ########################################################################## ##

##  ............................................................................
##  RQ4 - Main Models                                                       ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ4 - between effects with DVP; any dec                                ####
###  DVP*anydec; county-level controls; county clustered S.E.

rq4_anyDecdvp <- felm(ccgwBinary ~ anyDec_fiveYears*DVP + rural_urban_3pt + 
                       log(total_pop) + log(med_hhic) + perc_white + 
                       edu_percentPop + transcript_year + overall_cvi +
                       census_division|0|0|stcounty_fips, 
                   data=allData_transcriptLevel)

rq4_anyEpidvp <- felm(ccgwBinary ~ anyEpisode_fiveYears*DVP + rural_urban_3pt + 
                          log(total_pop) + log(med_hhic) + perc_white + 
                          edu_percentPop + transcript_year + overall_cvi +
                          census_division|0|0|stcounty_fips, 
                      data=allData_transcriptLevel)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
###  RQ4 - between effects with DVP; n dec                                  ####
###  DVP*ndec; county-level controls; county clustered S.E.

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
rq4_ndecdvp <- felm(ccgwBinary ~ nDec_fiveYears*DVP + rural_urban_3pt + 
                        log(total_pop) + log(med_hhic) + perc_white + 
                        edu_percentPop + transcript_year + overall_cvi +
                        census_division|0|0|stcounty_fips, 
                    data=allData_transcriptLevel)

rq4_nEpisodedvp <- felm(ccgwBinary ~ nEpisode_fiveYears*DVP + rural_urban_3pt + 
                            log(total_pop) + log(med_hhic) + perc_white + edu_percentPop + 
                            transcript_year + overall_cvi +
                            census_division|0|0|stcounty_fips, 
                        data=allData_transcriptLevel)

## Table 4: Do extreme weather events close the partisan gap in mentions?
modelsummary(list(rq4_anyDecdvp, rq4_ndecdvp, rq4_anyEpidvp, rq4_nEpisodedvp),
             coef_map = all_coefs,
             stars = stars,
             gof_map = gof_felm,
             glance = glance_custom.felm,
             title = "Table 4: Do extreme weather events close the partisan gap in mentions?",
             output = "flextable") %>% 
    fema_noaa_header() #%>%
    # save_as_docx(path = "./results/regressions/250324_rq4_Table4.docx")


##  ............................................................................
##  RQ4 - Plotted Interactions                                              ####

## plot theme
set_theme(base = theme_classic(),
          theme.font = "serif")

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ4 - lm; ndec*DVP plot                                                 ####
### county-level controls; county clustered S.E.

ndec5_dvp_lm <- lm(ccgwBinary ~ nDec_fiveYears*DVP + rural_urban_3pt + 
                       log(total_pop) + log(med_hhic) + perc_white + edu_percentPop +
                       census_division + transcript_year + overall_cvi,
                   data = allData_transcriptLevel)

ndec5_plot <- plot_model(ndec5_dvp_lm, 
           type = "int",
           axis.title = c("Number of Declarations Last 5 Years", 
                          "Climate Change/Global Warming Mention"),
           title = "Predicted Values of Climate Change/Global Warming Mention") 

# ggsave("./results/visualizations/250325_ndec5_dvp_lm.jpg", ndec5_plot)

                                          ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ4 - lm; anydec*DVP plot                                               ####
### county-level controls; county clustered S.E.

anydec5_dvp_lm <- lm(ccgwBinary ~ anyDec_fiveYears*DVP + rural_urban_3pt + 
                         log(total_pop) + log(med_hhic) + perc_white + edu_percentPop +
                         census_division + transcript_year + overall_cvi,
                     data = allData_transcriptLevel)

anydec5_plot <- plot_model(anydec5_dvp_lm, 
           type = "int",
           axis.title = c("Any Declaration Last Five Years", 
                          "Climate Change/Global Warming Mention"),
           title = "Predicted Values of Climate Change/Global Warming Mention") 

# ggsave("./results/visualizations/250325_anydec5_dvp_lm.jpg", anydec5_plot)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ4 - lm; nEpisode*DVP plot                                                 ####
### county-level controls; county clustered S.E.

nEpisode5_dvp_lm <- lm(ccgwBinary ~ nEpisode_fiveYears*DVP + rural_urban_3pt + 
                           log(total_pop) + log(med_hhic) + perc_white + edu_percentPop +
                           census_division + transcript_year + overall_cvi,
                       data = allData_transcriptLevel)

nEpisode5_plot <- plot_model(nEpisode5_dvp_lm, 
           type = "int",
           axis.title = c("Number of Episodes Last 5 Years", 
                          "Climate Change/Global Warming Mention"),
           title = "Predicted Values of Climate Change/Global Warming Mention") 

# ggsave("./results/visualizations/250325_nEpisode5_dvp_lm.jpg", nEpisode5_plot)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### RQ4 - lm; anyEpisode*DVP plot                                               ####
### county-level controls; county clustered S.E.

anyEpisode5_dvp_lm <- lm(ccgwBinary ~ anyEpisode_fiveYears*DVP + rural_urban_3pt + 
                             log(total_pop) + log(med_hhic) + perc_white + edu_percentPop +
                             census_division + transcript_year + overall_cvi,
                         data = allData_transcriptLevel)

anyEpisode5_plot <- plot_model(anyEpisode5_dvp_lm, 
           type = "int",
           axis.title = c("Any NOAA Episode Last Five Years", 
                          "Climate Change/Global Warming Mention"),
           title = "Predicted Values of Climate Change/Global Warming Mention") 

# ggsave("./results/visualizations/250325_anyEpisode5_dvp_lm.jpg", anyEpisode5_plot)










