### file description ###########################################################
##                                                                            ##
## This file handles the processing steps necessary for the first street data ##
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://firststreet.org/                                      ##  
## Output:                                                                    ##
##                                                                            ##    
##                                                                            ##    
################################################################################ 

#   ____________________________________________________________________________
#   load packages                                                           ####
#       Note: the tidyverse() package is loaded below through the geographic_processing.r file

library(arrow)                                     # open and work with parquet format
library(readtext)                                  # read filepaths

#   ____________________________________________________________________________
#   state and county information is necessary for aggregating to            ####
#   the county-year level processing the geography data is done in the 
#   /processing_scripts/geography.r file

source("./LocalView/code/processing_scripts/geography.r")
fs <- open_dataset("./LocalView/data/original/first_street/score-48.parquet")
fs <- Scanner$create(fs)
fs <- fs$ToTable()
fs <- as.data.frame(fs)


#   ____________________________________________________________________________
#   save sample                                                             ####

fs_sample <- head(fs, n = 100)
# write.csv(fs_sample, "./LocalView/data/samples/fs_sample.csv", row.names = FALSE)
