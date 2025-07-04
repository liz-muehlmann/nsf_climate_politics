####################################################################################################
##                                                                                                ##
##                                     Inclusion Regressions                                      ##
##                                                                                                ##
##  Data: County-Year with NA for no transcript in that county-year                               ##    
##  Model: has a transcript ~ DVP + Controls                                                      ##    
##  DV: Whether a county has a transcript (1 = has transcript, 0 = no transcript)                 ##    
##  IV: Democratic Voting Percentage + Controls                                                   ##
##  Controls: log total pop, log median household income; median age, percent white, non-hispanic;## 
##            population with college education, rural-urban-suburban,                            ##
##            climate change vulnerability index, binary indicator for disaster                   ##
##            declaration in previous year                                                        ##    
##                                                                                                ##
####################################################################################################


#   ____________________________________________________________________________
#   load preliminaries and data                                             ####

source("./code/preliminaries/regression_preliminaries.r")

load("./data/modified/all_data/allData_countyLevel_noNA.rdata") 
load("./data/modified/all_data/allData_countyLevel_withNA.rdata")
load("./data/modified/all_data/allData_transcriptLevel.rdata")


#   ____________________________________________________________________________
#   inclusion: linear time                                                  ####

inclusion_linear <- lm(has_transcript ~ DVP + log(total_pop) + log(med_hhic) + 
                       med_age + perc_white + edu_percentPop + 
                       as.factor(rural_urban_3pt) + overall_cvi + 
                       as.factor(fema_decBinary) + as.numeric(transcript_year), 
                       data = allData_countyLevel_withNA)


#   ____________________________________________________________________________
#   inclusion: dummy time                                                   ####

inclusion_dummy <- lm(has_transcript ~ DVP + log(total_pop) + log(med_hhic) + 
                      med_age + perc_white + edu_percentPop + 
                      as.factor(rural_urban_3pt) + overall_cvi  + 
                      as.factor(fema_decBinary) + as.factor(transcript_year), 
                      data = allData_countyLevel_withNA)


#   ____________________________________________________________________________
#   inclusion: census fixed effects                                         ####

inclusion_divisionFE <- plm(has_transcript ~ DVP + log(total_pop) + 
                            log(med_hhic) + med_age + perc_white + 
                            edu_percentPop + as.factor(rural_urban_3pt) + 
                            overall_cvi  + as.factor(fema_decBinary),
                            data = allData_countyLevel_withNA, 
                            index = c("census_division"), model="within")


#   ____________________________________________________________________________
#   saving inclusion into our sample regressions with dummy and linear type ####

title_inclusion <- "Inclusion Into Our Sample"
modelsummary(list("Linear Time" = inclusion_linear,
                  "Dummy Time" = inclusion_dummy),
             coef_map = all_coefs,
             stars = stars,
             title = title_inclusion,
             gof_map = gof_pml,
             output = "gt",
             # output = "./results/regressions/inclusion_regressions/inclusion_all_years.docx"
             )

#   ____________________________________________________________________________
#   inclusion into our sample by year                                       ####                ####

for(y in unique(allData_countyLevel_withNA$transcript_year)){
    
    df <- allData_countyLevel_withNA %>%
        filter(transcript_year == y)
    
    m_inc <- lm(has_transcript ~ DVP + perc_white + log(med_hhic) + 
                edu_percentPop + as.factor(rural_urban_3pt) + overall_cvi + 
                log(total_pop) + as.factor(fema_decBinary), 
                data = df)
    
    assign(paste("inclusion", y, sep="_"), m_inc)   
}


##  ............................................................................
#   save inclusion regressions 2010-2016                                    ####                ####

# modelsummary(list("2010" = inclusion_2010,
#                   "2011" = inclusion_2011,
#                   "2012" = inclusion_2012,
#                   "2013" = inclusion_2013,
#                   "2014" = inclusion_2014,
#                   "2015" = inclusion_2015,
#                   "2016" = inclusion_2016),
#              coef_map = all_coefs,
#              stars = stars,
#              title = title_inclusion,
#              gof_map = gof_pml,
#              output = "./results/regressions/inclusion_regressions/2010-2016_Inclusion.docx")


##  ................................................................................................
#   save inclusion regressions 2017-2023                                                        ####

# modelsummary(list("2017" = inclusion_2017,
#                   "2018" = inclusion_2018,
#                   "2019" = inclusion_2019,
#                   "2020" = inclusion_2020,
#                   "2021" = inclusion_2021,
#                   "2022" = inclusion_2022,
#                   "2023" = inclusion_2023),
#              coef_map = all_coefs,
#              stars = stars,
#              title = title_inclusion,
#              gof_map = gof_pml,
#              output = "./results/regressions/inclusion_regressions/2017-2023_Inclusion.docx")
