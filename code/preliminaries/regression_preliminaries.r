################################################################################
##                                                                            ##
##                          Regression Preliminaries                          ##
##                                                                            ##
################################################################################


#   ____________________________________________________________________________
#   load libraries                                                          ####
library(tidyverse)              # data manipulation
library(sjPlot)                 # plot predictions
library(sjmisc)                 # utility functions for sjplot
library(lfe)                    # fixed effects & clustered standard errors
library(stargazer)              # descriptive table output
library(brglm2)                 # penalized maximum likelihood
library(marginaleffects)        # marginal effects
library(ggplot2)                # plot themes
library(modelsummary)           # regression outputs
library(gt)                     # regression outputs
library(flextable)

#   ____________________________________________________________________________
#   customize goodness of fitness                                           ####

# format numbers
f <- function(x) format(round(x, 3), big.mark = ",")

# define glance function for RSE
glance_custom.felm <- function(x, ...){
    rse <- sqrt(sum(residuals(x) ^ 2) / x$df.residual)
    data.frame(RSE = format(rse, digits = 2))
}

# define stars
stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01)

# goodness of fit values to include in output
gof_felm <- list(
    list("raw" = "nobs",
         "clean" = "N",
         fmt = f),
    list("raw" = "r.squared",
         "clean" = "R-Squared",
         fmt = f),
    list("raw" = "adj.r.squared",
         "clean" = "Adj. R-Squared",
         fmt = f),
    list("raw" = "RSE",
         "clean" = "Residual Std. Error",
         fmt = f)
)

gof_pml <- list(
    list("raw" = "nobs",
         "clean" = "N",
         fmt = f),
    list("raw" = "logLik",
         "clean" = "Log Likelihood",
         fmt = f),
    list("raw" = "aic",
         "clean" = "AIC",
         fmt = f)
)

# model coefficients to include in output
all_coefs <- c(# `(Intercept)` = "Constant",  per alicia do not include
               DVP = "Democratic Vote Percentage",
               anyDec_fiveYears = "Had Any Declaration Last 5 Years",
               nDec_fiveYears = "Number of Declarations Last 5 Years",
               anyEpisode_fiveYears = "Had Any Episode Last 5 Years",
               nEpisode_fiveYears = "Number of Episodes Last 5 Years",
               anyDec_oneYear = "Any Declaration in the Last Year",
               anyEpisode_oneYear = "Had any Episode in the Last Year",
               nDec_oneYear = "Number of Declarations in the Last Year",
               nEpisode_oneYear = "Number of Episodes in the Last Year",
               anyDec_twoYears = "Any Declaration in the Last Two Years",
               anyEpisode_twoYears = "Any Episodes in the Last Two Years",
               nDec_twoYears = "Number of Declarations in the Last Two Years",
               nEpisode_twoYears = "Number of Episodes in the Last Two Years",
               rural_urban_3pt = "Rural Urban 3pt",
               `log(total_pop)` = "Log of Total Population",
               log_totalPop = "Log of Total Population",
               log_medhhic = "Log of Median Household Income",
               `log(med_hhic)` = "Log of Median Household Income",
               perc_white = "Percent White, Non-Hispanic",
               edu_percentPop = "Percent of Population with a College Degree",
               transcript_year = "Meeting Year",
               overall_cvi = "Overall Climate Vulnerability",
               `nDec_fiveYears:DVP` = "Number of Declarations x DVP",
               `anyDec_fiveYears:DVP` = "Any Declaration x DVP",
               `nEpisode_fiveYears:DVP` = "Number of Episodes x DVP",
               `anyEpisode_fiveYears:DVP` = "Any Episode x DVP",
               `transcript_year:DVP` = "DVP x Meeting Year",
               `anyDec_oneYear:DVP` = "DVP x Any Declaration in the Last Year",
               `anyEpisode_oneYear:DVP` = "DVP x Had any Episode in the Last Year",
               `nDec_oneYear:DVP` = "DVP x Number of Declarations in the Last Year",
               `nEpisode_oneYear:DVP` = "DVP x Number of Episodes in the Last Year",
               `anyDec_twoYears:DVP` = "DVP x Any Declaration in the Last Two Years",
               `anyEpisode_twoYears:DVP` = "DVP x Any Episodes in the Last Two Years",
               `nDec_twoYears:DVP` = "DVP x Number of Declarations in the Last Two Years",
               `nEpisode_twoYears:DVP` = "DVP x Number of Episodes in the Last Two Years",
               census_division1 = "Census Division 1",
               census_division2 = "Census Division 2",
               census_division3 = "Census Division 3",
               census_division4 = "Census Division 4",
               census_division5 = "Census Division 5",
               census_division6 = "Census Division 6",
               census_division7 = "Census Division 7",
               census_division8 = "Census Division 8",
               census_division9 = "Census Division 9",
               `as.factor(transcript_year)2011` = "2011",
               `as.factor(transcript_year)2012` = "2012",
               `as.factor(transcript_year)2013` = "2013",
               `as.factor(transcript_year)2014` = "2014",
               `as.factor(transcript_year)2015` = "2015",
               `as.factor(transcript_year)2016` = "2016",
               `as.factor(transcript_year)2017` = "2017",
               `as.factor(transcript_year)2018` = "2018",
               `as.factor(transcript_year)2019` = "2019",
               `as.factor(transcript_year)2020` = "2020",
               `as.factor(transcript_year)2021` = "2021",
               `as.factor(transcript_year)2022` = "2022",
               `as.factor(transcript_year)2023` = "2023",
               `DVP:as.factor(transcript_year)2011` = "DVP x 2011",
               `DVP:as.factor(transcript_year)2012` = "DVP x 2012",
               `DVP:as.factor(transcript_year)2013` = "DVP x 2013",
               `DVP:as.factor(transcript_year)2014` = "DVP x 2014",
               `DVP:as.factor(transcript_year)2015` = "DVP x 2015",
               `DVP:as.factor(transcript_year)2016` = "DVP x 2016",
               `DVP:as.factor(transcript_year)2017` = "DVP x 2017",
               `DVP:as.factor(transcript_year)2018` = "DVP x 2018",
               `DVP:as.factor(transcript_year)2019` = "DVP x 2019",
               `DVP:as.factor(transcript_year)2020` = "DVP x 2020",
               `DVP:as.factor(transcript_year)2021` = "DVP x 2021",
               `DVP:as.factor(transcript_year)2022` = "DVP x 2022",
               `DVP:as.factor(transcript_year)2023` = "DVP x 2023"
               
)

dv_header <- function(ft) {
    ft %>% 
        add_header_row(values = c("",
                                  "Dependent Variable: Climate Change/Global Warming Mention")) %>% 
        align(align = c("center"), part = "header") %>% 
        padding(padding = 0, part = "all")
}


fema_noaa_header <- function(ft) {
    ft %>% 
        add_header_row(values = c(" ", "FEMA", "NOAA"), colwidths = c(1,2,2)) %>% 
        add_header_row(values = c("",
                                  "Dependent Variable: Climate Change/Global Warming Mention"),
                       colwidths = c(1,4)) %>% 
        align(align = c("center"), part = "header") %>% 
        padding(padding = 0, part = "all")
}

fmt_me <- function(ft) {
    ft %>% 
        flextable() %>% 
        colformat_double(big.mark = ",", digits = 2, na_str = "N/A") %>% 
        labelizor(labels = c(edu_percentPop = "Percent of Population with a College Degree",
                    log_medhhic = "Log of Median Household Income",
                    log_totalPop = "Log of Total Population",
                    overall_cvi = "Overall Climate Vulnerability",
                    perc_white = "Percent White, Non-Hispanic",
                    rural_urban_3pt = "Rural Urban 3pt",
                    transcript_year = "Meeting Year")) %>% 
        set_header_labels(term = "Variable",
                          estimate = "Estimate",
                          std.error = "Standard Error",
                          statistic = "Statistic",
                          p.value = "p-value",
                          s.value = "s-value",
                          conf.low = "Confidence Low",
                          conf.high = "Confidence High")
}

