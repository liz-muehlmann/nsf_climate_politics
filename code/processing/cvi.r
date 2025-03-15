### file description ###########################################################
##                                                                            ##
## This file handles the processing steps necessary for the climate change    ##
##      vulnerability index                                                   ##
##      Data included:                                                        ##
##          Climate Change Vulnerability (2010-2023)                          ##
##              https://github.com/wachiuphd/CVI                              ##
##                                                                            ##
## Output:                                                                    ##
##     /LocalView/data/modified/cvi_county.rdata                              ##
##                                                                            ##
################################################################################

#   ____________________________________________________________________________
#   load libraries and custom functions                                     ####

source("./code/preliminaries/analysis_prelims.r")      
library(readxl)                                           # work with xlsx files

cvi <- read_xlsx("./data/original/202310_CVI.xlsx", 
                 sheet = "Domain CVI Values") %>%
  select(-County, -State) %>%
  rename(
    tract_fips = `FIPS Code`,
    overall_cvi = `Overall CVI Score`,
    baseline_all = `Baseline: All`,
    baseline_health = `Baseline: Health`,
    baseline_socioEcon = `Baseline: Social Economic`,
    baseline_infrastructure = `Baseline: Infrastructure`,
    baseline_environ = `Baseline: Environment`,
    climate_all = `Climate Change: All`,
    climate_health = `Climate Change: Health`,
    climate_socioEcon = `Climate Change: Social Economic`,
    climate_extreme = `Climate Change: Extreme Events`) %>%
  createFips() %>%
  excludeStates() %>%
  group_by(stcounty_fips) %>%
  mutate(n_counties = n()) %>%
  reframe(
    across(c(
      overall_cvi, baseline_all, baseline_health, baseline_socioEcon,
      baseline_infrastructure, baseline_environ, climate_all, climate_health,
      climate_socioEcon, climate_extreme
    ), ~ sum(.x) / n_counties)
  ) %>%
  distinct(stcounty_fips, .keep_all = TRUE) %>%
  filter(stcounty_fips != 51515) %>%
  fixCounties2020()

# save(cvi, file = "./data/modified/cvi.rdata")

##  ............................................................................
##  CVI Sample                                                              ####

sample_cvi <- cvi %>% sample_n(50)
# write.csv(sample_cvi, "./results/samples/cvi_sample.csv", row.names = FALSE)

