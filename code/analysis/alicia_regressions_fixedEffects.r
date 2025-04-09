rm(list=ls()) 
library(tidyverse)
library(lfe)
library(stargazer)

load("LocalView/data/modified/alicia_allData_transcriptLevel.rdata")
names(allData_transcriptLevel)

allData_transcriptLevel$place_fips_factor <- as.factor(allData_transcriptLevel$place_fips)
allData_transcriptLevel$transcript_year_num <- as.numeric(allData_transcriptLevel$transcript_year)


## ccgw and dvp means within place or county -- all data set
## mean of observations in data set
allData_transcriptLevel <- allData_transcriptLevel %>% 
  group_by(place_fips) %>% 
  mutate(ccgwBinary_mean_place = mean(ccgwBinary))

allData_transcriptLevel <- allData_transcriptLevel %>% 
  group_by(stcounty_fips) %>% 
  mutate(ccgwBinary_mean_county = mean(ccgwBinary))

allData_transcriptLevel <- allData_transcriptLevel %>% 
  group_by(place_fips) %>% 
  mutate(DVP_mean_place = mean(DVP))

allData_transcriptLevel <- allData_transcriptLevel %>% 
  group_by(stcounty_fips) %>% 
  mutate(DVP_mean_county = mean(DVP))

allData_transcriptLevel <- allData_transcriptLevel %>% 
  group_by(place_fips) %>% 
  mutate(n_place = length(place_fips))

allData_transcriptLevel <- allData_transcriptLevel %>% 
  group_by(stcounty_fips) %>% 
  mutate(n_county = length(stcounty_fips))


allData_transcriptLevel <- allData_transcriptLevel %>% 
  group_by(place_fips) %>% 
  mutate(unique_years_place = length(unique(transcript_year)))

allData_transcriptLevel <- allData_transcriptLevel %>% 
  group_by(stcounty_fips) %>% 
  mutate(unique_years_county = length(unique(transcript_year)))

## subtract off the means
## mechanically -- this does the same thing as the felm!
## this is what a FE does!
allData_transcriptLevel$dvp_demean_place <- allData_transcriptLevel$DVP - allData_transcriptLevel$DVP_mean_place
allData_transcriptLevel$ccgw_demean_place <- allData_transcriptLevel$ccgwBinary - allData_transcriptLevel$ccgwBinary_mean_place


table(allData_transcriptLevel$n_place)                # 53 singletons -- very few! good!
mean(allData_transcriptLevel$n_place==1)              # tiny share, so non-consideration won't affect results
#View(allData_transcriptLevel[allData_transcriptLevel$dvp_demean_place< 0.001 & allData_transcriptLevel$dvp_demean_place> -0.001,])
mean(allData_transcriptLevel$dvp_demean_place==0) # 2.5% with no variation in DVP - will drop out because don't have years to span it
mean(allData_transcriptLevel$dvp_demean_place< 0.00001 & allData_transcriptLevel$dvp_demean_place> -0.00001) # 2.5% with no variation in DVP - will drop out because don't have years to span it
mean(allData_transcriptLevel$dvp_demean_place< 0.001 & allData_transcriptLevel$dvp_demean_place> -0.001) #8.6% with tiny variation
mean(allData_transcriptLevel$unique_years_place==1) # 0.5% only have data from one year

# See how demeaning looks and works within and across places

View(allData_transcriptLevel[order(allData_transcriptLevel$place_fips),c("place_fips", "stcounty_fips", "place_name", "county_name", "transcript_year", "n_place","unique_years_place",
                                "DVP","DVP_mean_place", "dvp_demean_place",
                                "ccgwBinary", "ccgwBinary_mean_place", "ccgw_demean_place")])

sumstat <- data.frame(allData_transcriptLevel[,c("DVP", "dvp_demean_place", "ccgwBinary", "ccgw_demean_place")])
stargazer(sumstat, type="text")
hist(allData_transcriptLevel$DVP)
hist(allData_transcriptLevel$dvp_demean_place)
table(allData_transcriptLevel$ccgwBinary)

# visualize the raw and demeaned data

ggplot(allData_transcriptLevel, aes(x = DVP, y = ccgwBinary)) +
  geom_jitter(width = 0.2, height = 0.1) +  # Add jitter to points
  geom_smooth(method = "lm") +
  labs(title = "Raw data with jitter",
       x = "DVP",
       y = "CCGW") +
  theme_minimal()

ggplot(allData_transcriptLevel, aes(x = DVP, y = ccgwBinary)) +
  geom_jitter(width = 0.2, height = 0.1) +  # Add jitter to points
  geom_smooth() +
  labs(title = "Raw data with jitter",
       x = "DVP",
       y = "CCGW") +
  theme_minimal()

# Y-axis is about change from average mention of CC/GW in meetings. 
ggplot(allData_transcriptLevel, aes(x = dvp_demean_place, y = ccgw_demean_place)) +
  geom_smooth() +
  labs(title = "Demeaned",
       x = "DVP demeaned by place",
       y = "CCGW demeaned by place") +
  theme_minimal()

ggplot(allData_transcriptLevel, aes(x = dvp_demean_place, y = ccgw_demean_place)) +
  geom_point() +
  geom_smooth() +
  labs(title = "Demeaned",
       x = "DVP demeaned by place",
       y = "CCGW demeaned by place") +
  theme_minimal()

# Three regression tables
# No time
bw_nocont <- felm(ccgwBinary ~ DVP  |0|0|place_fips, data=allData_transcriptLevel)
bw <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + perc_white + edu_percentPop  |0|0|place_fips, data=allData_transcriptLevel)
fe_pl_nocont <- felm(ccgwBinary ~ DVP  |place_fips|0|place_fips, data=allData_transcriptLevel)
fe_pl <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + perc_white + edu_percentPop  |place_fips|0|place_fips, data=allData_transcriptLevel)
fe_co_nocont <- felm(ccgwBinary ~ DVP  |stcounty_fips|0|place_fips, data=allData_transcriptLevel)
fe_co <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + perc_white + edu_percentPop  |stcounty_fips|0|place_fips, data=allData_transcriptLevel)
stargazer(bw_nocont, bw, fe_pl_nocont, fe_pl, fe_co_nocont, fe_co, type="text", add.lines = list(c("FE", "-", "-", "Place", "Place", "County", "County")))

# Time linear trend
bwy_nocont <- felm(ccgwBinary ~ DVP + transcript_year_num |0|0|place_fips, data=allData_transcriptLevel)
bwy <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + perc_white + edu_percentPop + transcript_year_num |0|0|place_fips, data=allData_transcriptLevel)
fey_pl_nocont <- felm(ccgwBinary ~ DVP + transcript_year_num |place_fips|0|place_fips, data=allData_transcriptLevel)
fey_pl <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + perc_white + edu_percentPop + transcript_year_num |place_fips|0|place_fips, data=allData_transcriptLevel)
fey_co_nocont <- felm(ccgwBinary ~ DVP + transcript_year_num |stcounty_fips|0|place_fips, data=allData_transcriptLevel)
fey_co <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + perc_white + edu_percentPop + transcript_year_num |stcounty_fips|0|place_fips, data=allData_transcriptLevel)
stargazer(bwy_nocont, bwy, fey_pl_nocont, fey_pl, fey_co_nocont, fey_co, type="text", add.lines = list(c("FE", "-", "-", "Place", "Place", "County", "County")))

# Time FE
by2_nocon <- felm(ccgwBinary ~ DVP |transcript_year|0|place_fips, data=allData_transcriptLevel)
by2 <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + perc_white + edu_percentPop |transcript_year|0|place_fips, data=allData_transcriptLevel)
fey2_pl_nocon <- felm(ccgwBinary ~ DVP  |place_fips + transcript_year|0|place_fips, data=allData_transcriptLevel)
fey2_pl <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + perc_white + edu_percentPop  |place_fips + transcript_year|0|place_fips, data=allData_transcriptLevel)
fey2_co_nocon <- felm(ccgwBinary ~ DVP  |stcounty_fips + transcript_year|0|place_fips, data=allData_transcriptLevel)
fey2_co <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + perc_white + edu_percentPop  |stcounty_fips + transcript_year|0|place_fips, data=allData_transcriptLevel)
stargazer(by2_nocon, by2, fey2_pl_nocon, fey2_pl, fey2_co_nocon, fey2_co, type="text", add.lines = list(c("FE", "Year", "Year", "Place + Year", "Place + Year", "County + Year", "County + Year")))

# How do place-FE regressions handle places with no variation in DVP?
pl_bivar <- felm(ccgwBinary ~ DVP |place_fips|0|place_fips, data=allData_transcriptLevel)
pl_nosingleton <- felm(ccgwBinary ~ DVP  |place_fips|0|place_fips, data=allData_transcriptLevel, subset= n_place>1)
pl_nosing_varDVP <- felm(ccgwBinary ~ DVP  |place_fips|0|place_fips, data=allData_transcriptLevel, subset= n_place>1 & dvp_demean_place!=0)
coef(pl_bivar)
coef(pl_nosing_varDVP)
stargazer(pl_bivar, pl_nosingleton, pl_nosing_varDVP, type="text") # all coefficients are the same
# Really, the regression is ignoring cases without variation in DVP, but a bit under 3% in this category

# Between effects interpretation
sd(allData_transcriptLevel$DVP) # SD is a 0.17 percentage point of DVP
mean(allData_transcriptLevel$DVP) # mean is 0.5
coef(bw_nocont)['DVP']*sd(allData_transcriptLevel$DVP) # a standard deviation higher DVP across places is associated with a 2.7 percentage point higher likelihood of mentioning CC/GW

# Mummolo and Peterson
dvp_place_resid <- residuals(felm(DVP ~ 1|place_fips|0|place_fips, data=allData_transcriptLevel))
head(cbind(dvp_place_resid, allData_transcriptLevel$dvp_demean_place)) # resid is same as demeaned by place
hist(dvp_place_resid)
hist(allData_transcriptLevel$dvp_demean_place)
sd(dvp_place_resid) # SD is a 0.02 percentage point change in DVP
# tiny... counties hardly change their DVP over time... 
fe_pl_nocont <- felm(ccgwBinary ~ DVP|place_fips|0|place_fips, data=allData_transcriptLevel)
coef(fe_pl_nocont)['DVP']*sd(dvp_place_resid) 
# a standard deviation larger increase in DVP within the place over time 
# is associated with a 0.5 percentage point increase in mentioning CC/GW
# "as DVP changes within places over time, mentions of CC/GW in meetings change..."

# Check that demean doing the same as felm FE; dummy for time is same as adding it in FELM -- YES!
pl_demean <- felm(ccgw_demean_place ~ dvp_demean_place|0|0|place_fips, data=allData_transcriptLevel)
pl_demean$coefficients
pl_fe_bivar <- felm(ccgwBinary ~ DVP|place_fips|0|place_fips, data=allData_transcriptLevel)
pl_fe_bivar$coefficients
pl_fe_year <- felm(ccgwBinary ~ DVP + transcript_year|place_fips|0|place_fips, data=allData_transcriptLevel)
pl_2fe_bivar <- felm(ccgwBinary ~ DVP |place_fips + transcript_year|0|place_fips, data=allData_transcriptLevel)
pl_2fe_bivar$coefficients
stargazer(pl_de
          mean, pl_fe_bivar, pl_fe_year, pl_2fe_bivar, type="text")

# check that running place dummies is same as lfe -- yes
pl_all_dum <- felm(ccgwBinary ~ DVP + rural_urban_3pt + log(total_pop) + log(med_hhic) + perc_white + edu_percentPop + place_fips_factor + transcript_year |0|0|place_fips, data=allData_transcriptLevel)
stargazer(pl_all, pl_all_dum, omit="place", type="text")

