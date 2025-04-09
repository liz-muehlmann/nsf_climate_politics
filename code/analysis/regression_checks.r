################################################################################
##                                                                            ##
##                          checks for fixed effects                          ##
##                                                                            ##
##  Data: transcript-level local view data with fema meeting information      ##
##                                                                            ##
################################################################################

## FELM does what m&p say -- so the interpretation would work for any of the 
## three regression tables!

## --- transcripts in more or less democratic counties where the place is
## DVP is at County -- so County SE (more conservative)

## TIME FE - County SE
##### by2 <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + 
##### log(med_hhic) + perc_white + edu_percentPop |transcript_year|0|stcounty_fips,
##### data=df)

##### this is the model we'll use - 


source("./LocalView/code/analysis_scripts/regressions_preliminaries.r")
load("./LocalView/data/modified/lvFema_transcriptLevel.rdata")

lvFema_transcriptLevel <- lvFema_transcriptLevel %>% 
    mutate(nDec_fiveYearsFactor = as.factor(nDec_fiveYears),                    ## factor variables
           nDec_sixYearsFactor = as.factor(nDec_sixYears),
           place_fips_factor = as.factor(place_fips),
           transcript_year = as.numeric(str_sub(meeting_date, 1, 4)),
           transcript_yearFactor = as.factor(transcript_year),                
           log_totalPop = log(total_pop),                                       ## log variables
           log_medhhic = log(med_hhic),
           party_lean = ifelse(round(DVP, 2) <= .50,                            ## add in party lean
                               "Leans Republican", "Leans Democratic")) %>% 
    group_by(place_fips) %>% 
    mutate(ccgwBinary_mean_place = mean(ccgwBinary),                            ## place mean; n-places; n-place-years
           DVP_mean_place = mean(DVP),                                          
           n_place = length(place_fips),
           unique_years_place = length(unique(transcript_year))) %>% 
    ungroup() %>% 
    group_by(stcounty_fips) %>%                                                 ## county mean; n-county; n-county-years
    mutate(ccgwBinary_mean_county = mean(ccgwBinary),               
           DVP_mean_county = mean(DVP),
           n_county = length(stcounty_fips),
           unique_years_county = length(unique(transcript_year))) %>% 
    ungroup() %>%                                                               ## de-mean data; place level
    mutate(dvp_demean_place = DVP - DVP_mean_place,
           ccgw_demean_place = ccgwBinary - ccgwBinary_mean_place,
           dvp_demean_county = DVP - DVP_mean_county,
           ccgw_dmean_county = ccgwBinary - ccgwBinary_mean_county)

## dvp_demean_place and ccgw_demean_place are relative to teach places mean,
## is the value high or low in our data set

## be up front -- this is not a random sample either at the place level or over time
## some places show up more than others and if they start, they likely continue

## dvp_demean_place = 0 is where places have no variation in DVP over time
## because the transcripts occurred within the same administration
## they drop out when using place FE

## we might want to do county FE, not place -- conceptually since DVP is county
## 1% of the data would drop out if we use county

#   ____________________________________________________________________________
#   data checks                                                             ####

## number of places with only one transcript (n = 53)
table(lvFema_transcriptLevel$n_place)

## number of counties with only one transcript (n = 17)
table(lvFema_transcriptLevel$n_county)

## 0.0005128205 - will not affect results
mean(lvFema_transcriptLevel$n_place == 1)

## 0.0001644896 - will not affect results
mean(lvFema_transcriptLevel$n_county == 1)

## 2.5% of places with no variation in DVP; will drop out
mean(lvFema_transcriptLevel$dvp_demean_place == 0)

## 1% of counties with no variation in  placeDVP; will drop out
mean(lvFema_transcriptLevel$dvp_demean_county == 0)

## 1.0 with no variation in county DVP; will drop out
mean(lvFema_transcriptLevel$dvp_demean_place< 0.00001 & 
         lvFema_transcriptLevel$dvp_demean_place> -0.00001) 

## 8.6% places with tiny variation
mean(lvFema_transcriptLevel$dvp_demean_place< 0.001 & 
         lvFema_transcriptLevel$dvp_demean_place> -0.001) 

## 5.3% counties with tiny variation
mean(lvFema_transcriptLevel$dvp_demean_county< 0.001 & 
         lvFema_transcriptLevel$dvp_demean_county> -0.001) 

## 0.5% only have data from one year 
mean(lvFema_transcriptLevel$unique_years_place == 1) 

## 0.2% only have data from one year 
mean(lvFema_transcriptLevel$unique_years_place == 1) 


#   ____________________________________________________________________________
#   see how demeaning looks and works within and across places              ####

View(lvFema_transcriptLevel[order(lvFema_transcriptLevel$place_fips), 
                            c("place_fips", "stcounty_fips", "place_name", 
                              "county_name", "transcript_year", "n_place",
                              "unique_years_place",
                              "DVP", "DVP_mean_place", "dvp_demean_place",
                              "ccgwBinary", "ccgwBinary_mean_place", 
                              "ccgw_demean_place")])

#   ____________________________________________________________________________
#   calculate summary statistics                                            ####

## summary statistics (n; mean; standard deviation; min; max)
sumstat_place <- data.frame(lvFema_transcriptLevel[,c("DVP", "dvp_demean_place", 
                                                 "ccgwBinary", 
                                                 "ccgw_demean_place")])
stargazer(sumstat_place, type="text")

## histogram of raw and de-meaned DVP
hist(lvFema_transcriptLevel$DVP)
hist(lvFema_transcriptLevel$dvp_demean_place)

# n places with & without CCGW mention 0 (n = 99,134); 1 (n = 4,216)
table(lvFema_transcriptLevel$ccgwBinary)

## summary statistics (n; mean; standard deviation; min; max)
sumstat_county <- data.frame(lvFema_transcriptLevel[,c("DVP", "dvp_demean_county", 
                                                "ccgwBinary", 
                                                "ccgw_demean_county")])
stargazer(sumstat_county, type="text")

## histogram of raw and de-meaned DVP
hist(lvFema_transcriptLevel$DVP)
hist(lvFema_transcriptLevel$dvp_demean_county)

# n places with & without CCGW mention 0 (n = 99,134); 1 (n = 4,216)
table(lvFema_transcriptLevel$ccgwBinary)

#   ____________________________________________________________________________
#   visualize raw & demeaned data                                           ####

## can on average say that CCGW is related to DVP
## raw data (slight positive relationship)
ggplot(lvFema_transcriptLevel, aes(x = DVP, y = ccgwBinary)) +
    geom_jitter(width = 0.2, height = 0.1) +  
    geom_smooth(method = "lm") +
    labs(title = "Raw data with jitter",
         x = "DVP",
         y = "CCGW") +
    theme_minimal()

## Y-axis is about change from average mention of CC/GW in meetings. 
## potential non-linearity
## few observations on the edges so CI is large
ggplot(lvFema_transcriptLevel, aes(x = dvp_demean_place, y = ccgw_demean_place)) +
    geom_smooth() +
    labs(title = "Demeaned",
         x = "DVP demeaned by place",
         y = "CCGW demeaned by place") +
    theme_minimal()

## weak relationship between CCGW & DVP
## put on the scale of the points -- kinda flat.
ggplot(lvFema_transcriptLevel, aes(x = dvp_demean_place, y = ccgw_demean_place)) +
    geom_point() +
    geom_smooth() +
    labs(title = "Demeaned",
         x = "DVP demeaned by place",
         y = "CCGW demeaned by place") +
    theme_minimal()

#   ____________________________________________________________________________
#   place FE regressions                                                    ####

##  ............................................................................
##  without time                                                            ####

## note: | fixed effects | IVs | clustered S.E.

## place clustered S.E. (with & without controls)
bw_noControls <- felm(ccgwBinary ~ DVP 
                      |0|0|place_fips,
                      data = lvFema_transcriptLevel)

bw <- felm(ccgwBinary ~ DVP +
               rural_urban_3pt + log_totalPop + log_medhhic + 
               perc_white + edu_percentPop
           |0|0|place_fips,
           data = lvFema_transcriptLevel)

## place F.E. and clustered S.E. (with & without controls)
fe_pl_noControls <- felm(ccgwBinary ~ DVP 
                         |place_fips|0|place_fips,
                         data = lvFema_transcriptLevel)

fe_pl <- felm(ccgwBinary ~ DVP + 
                  rural_urban_3pt + log_totalPop + log_medhhic + 
                  perc_white + edu_percentPop  
              |place_fips|0|place_fips, 
              data = lvFema_transcriptLevel)

## place clustered S.E.; county F.E.
fe_co_noControls <- felm(ccgwBinary ~ DVP  
                     |stcounty_fips|0|place_fips, 
                     data = lvFema_transcriptLevel)

fe_co <- felm(ccgwBinary ~ DVP + 
                  rural_urban_3pt + log_totalPop + log_medhhic + 
                  perc_white + edu_percentPop  
              |stcounty_fips|0|place_fips, 
              data = lvFema_transcriptLevel)

## table
models <- list(bw_noControls, bw, fe_pl_noControls, fe_pl, fe_co_noControls, fe_co)
stargazer(models,
          type = "text",
          add.lines = list(c("FE", "-", "-", "Place", "Place", "County", "County")))
## all have clustered SE
## 1 is bivarate - ccgw ~ DVP no controls, no fe
## 2 controls that might be a confounder to the relationship between ccgw~DVP
## 3 interp is below

##  ............................................................................
##  linear time                                                             ####


## time accounts for some of the variation (coefs get smaller)
## place clustered S.E. (with & without controls)
bwy_noControls <- felm(ccgwBinary ~ DVP + 
                       transcript_year 
                   |0|0|place_fips, 
                   data = lvFema_transcriptLevel)

bwy <- felm(ccgwBinary ~ DVP + 
                rural_urban_3pt + log_totalPop + log_medhhic + perc_white + 
                edu_percentPop + transcript_year 
            |0|0|place_fips, data = lvFema_transcriptLevel)

## place F.E. and clustered S.E. (with & without controls)
fey_pl_noControls <- felm(ccgwBinary ~ DVP + 
                          transcript_year
                      |place_fips|0|place_fips, 
                      data = lvFema_transcriptLevel)

fey_pl <- felm(ccgwBinary ~ DVP + 
                   rural_urban_3pt + log_totalPop + log_medhhic + perc_white + 
                   edu_percentPop + transcript_year 
               |place_fips|0|place_fips, 
               data = lvFema_transcriptLevel)

## place clustered S.E.; county F.E.
fey_co_noControls <- felm(ccgwBinary ~ DVP + 
                          transcript_year 
                      |stcounty_fips|0|place_fips, 
                      data = lvFema_transcriptLevel)

fey_co <- felm(ccgwBinary ~ DVP + 
                   rural_urban_3pt + log_totalPop + log_medhhic + perc_white + 
                   edu_percentPop + transcript_year 
               |stcounty_fips|0|place_fips, data = lvFema_transcriptLevel)

## table
linear_time_models <- list(bwy_noControls, bwy, fey_pl_noControls, fey_pl, 
                           fey_co_noControls, fey_co)
stargazer(linear_time_models, 
          type="text", 
          add.lines = list(c("FE", "-", "-", "Place", "Place", "County", "County")))


##  ............................................................................
##  time F.E.                                                               ####


## time F.E.; place clustered S.E. (with & without controls)
by2_noControls <- felm(ccgwBinary ~ DVP 
                  |transcript_year|0|place_fips, 
                  data=lvFema_transcriptLevel)

by2 <- felm(ccgwBinary ~ DVP + 
                rural_urban_3pt + log_totalPop + log_medhhic + perc_white + 
                edu_percentPop 
            |transcript_year|0|place_fips, 
            data=lvFema_transcriptLevel)

## time and place F.E.; place clustered S.E (with & without controls)
fey2_pl_noControls <- felm(ccgwBinary ~ DVP  
                           |place_fips + 
                               transcript_year|0|place_fips, 
                           data=lvFema_transcriptLevel)

fey2_pl <- felm(ccgwBinary ~ DVP + 
                    rural_urban_3pt + log_totalPop + log_medhhic + perc_white + 
                    edu_percentPop  
                |place_fips + transcript_year|0|place_fips, 
                data=lvFema_transcriptLevel)

## time and county F.E.; place clustered S.E. (with & without controls)
fey2_co_noControls <- felm(ccgwBinary ~ DVP  
                           |stcounty_fips + 
                               transcript_year|0|place_fips,
                           data=lvFema_transcriptLevel)

fey2_co <- felm(ccgwBinary ~ DVP + 
                    rural_urban_3pt + log_totalPop + log_medhhic + perc_white + 
                    edu_percentPop  
                |stcounty_fips + 
                    transcript_year|0|place_fips, 
                data=lvFema_transcriptLevel)

## table
time_fe_models <- list(by2_noControls, by2, fey2_pl_noControls, 
                       fey2_pl, fey2_co_noControls, fey2_co)
stargazer(time_fe_models, 
          type="text", 
          add.lines = list(c("FE", "Year", "Year", "Place + Year", "Place + Year", "County + Year", "County + Year")))


## all of the variables are being de-meaned in all of the regressions.
##  ............................................................................
##  How do place-FE regressions handle places with no variation in DVP?     ####


## this section tries to understand what FELM is doing because 
## DVP, ACS, and RU have different years 

## pl_bivar is the same as fe_pl_noControls
#pl_bivar <- felm(ccgwBinary ~ DVP |place_fips|0|place_fips, data=allData_transcriptLevel)

## more than one place
pl_nosingleton <- felm(ccgwBinary ~ DVP  
                       |place_fips|0|place_fips, 
                       data = lvFema_transcriptLevel, 
                       subset = n_place > 1)             

## more than one place and demeaned DVP isn't 0
pl_nosing_varDVP <- felm(ccgwBinary ~ DVP  
                         |place_fips|0|place_fips,
                        data = lvFema_transcriptLevel, 
                        subset = n_place > 1 &           
                            dvp_demean_place != 0)
## 0.2265823 
coef(fe_pl_noControls)

## 0.2265823 
coef(pl_nosing_varDVP)

## all coefficients are the same
## regression is ignoring cases without variation in DVP (~3%)
stargazer(fe_pl_noControls, pl_nosingleton, pl_nosing_varDVP, type="text") 
## observations are misleading because some are dropping out.


##  ............................................................................
##  between effects interpretation                                          ####

## no fixed effects

## S.D. is a 0.17 of DVP -- 17 percentage points
sd(lvFema_transcriptLevel$DVP)

## mean: 0.5 (51 percent)
mean(lvFema_transcriptLevel$DVP)

######################################################################
## a standard deviation higher DVP across places is associated with 
##     a 2.7 percentage point higher likelihood of mentioning CC/GW
coef(bw_noControls)['DVP']*sd(lvFema_transcriptLevel$DVP)
######################################################################

#   ____________________________________________________________________________
#   Mummolo & Peterson                                                      ####

## doing the checks in the M&P checklist

## intercept only, place F.E., no IV
## is exactly the same as de-meaning at the observation level
dvp_place_resid <- residuals(felm(DVP ~1|place_fips|0, 
                                  data = lvFema_transcriptLevel))

## residuals are the same as demeaned place
head(cbind(dvp_place_resid, lvFema_transcriptLevel$dvp_demean_place))

## histogram of residuals
## are exactly the same of the de-meaned values
hist(dvp_place_resid)
hist(lvFema_transcriptLevel$dvp_demean_place)

## S.D. is a 0.2 change in DVP (2 percentage change in DVP)
## counties hardly change their DVP over time
sd(dvp_place_resid)

## with bivariate model no controls the 
## 0.004967686
coef(fe_pl_noControls)['DVP']*sd(dvp_place_resid)
# a standard deviation larger increase in DVP within the place over time 
# is associated with a 0.5 percentage point increase in likelihood of mentioning CC/GW
# "as DVP changes within places over time, mentions of CC/GW in meetings change..."


################### CHECKING THE BLACK BOX OF FELM ###################

#   ____________________________________________________________________________
#   check that demeaning and FELM are doing the same thing -- yes           ####

## demeaned place F.E.; place clustered S.E. (with & without controls)
pl_demean <- felm(ccgw_demean_place ~ dvp_demean_place|0|0|place_fips, 
     data=lvFema_transcriptLevel)
# >>> these are fixed effects general linear models!! same as feglm from fixest()

pl_demean$coefficients

## place F.E.; place clustered S.E.
pl_fe_bivar <- felm(ccgwBinary ~ DVP|place_fips|0|place_fips, 
                    data = lvFema_transcriptLevel)
pl_fe_bivar$coefficients

## year and place F.E.; place clustered S.E.
pl_fe_year <- felm(ccgwBinary ~ DVP + transcript_year|place_fips|0|place_fips, 
                   data = lvFema_transcriptLevel)

## year and place F.E.; place clustered S.E.
pl_2fe_bivar <- felm(ccgwBinary ~ DVP |place_fips + transcript_year|0|place_fips, 
                     data = lvFema_transcriptLevel)

pl_2fe_bivar$coefficients

stargazer(pl_demean, pl_fe_bivar, pl_fe_year, pl_2fe_bivar, type = "text")

 
#   ____________________________________________________________________________
#   check that running place dummies is same as lfe -- yes                  ####
pl_all_dum <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log_totalPop + 
                       log_medhhic + perc_white + edu_percentPop + 
                       place_fips_factor + transcript_year 
                   |0|0|place_fips, data=lvFema_transcriptLevel)

# stargazer(pl_all, pl_all_dum, omit="place", type="text")
############ pl_al is not defined ###########


























