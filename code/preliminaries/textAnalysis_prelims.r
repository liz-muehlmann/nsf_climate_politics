################################################################################ 
##                                                                            ##
## This file includes the custom functions used to do text analysis using     ##
##     the local view data                                                    ##
##                                                                            ##
## This file is the es the local view data for text analysis.                 ##
##      Data included:                                                        ##
##          Local View (2010-2023)                                            ##
##              https://doi.org/10.7910/DVN/NJTBEM                            ##  
##                                                                            ##
##  Output:                                                                   ##
##      None                                                                  ##
################################################################################ 

#   ____________________________________________________________________________
#   load source file, data, and increase heap space                         ####

library(readtext)                  # read filepaths
library(tidyverse)                 # data manipulation
library(tidytext)                  # corpus manipulation
library(broom)                     # make outputs tidy 
library(quanteda)                  # create dictionaries
library(textclean)                 # contractions list
library(stopwords)                 # stop word lists
library(ggplot2)                   # data visualization
library(ggraph)                    # network plots
library(patchwork)                 # plot composition 
library(SnowballC)                 # stemming words 
library(SemNetCleaner)             # singularize words   
library(tm)
library(topicmodels)
library(ggplot2)
library(RColorBrewer)
options(java.parameters = "-Xmx8000m")

load("./data/modified/local_view/lv_clean_transcript.rdata") 
load("./data/modified/all_data/allData_transcriptLevel.rdata") 

#   ____________________________________________________________________________
#   merge all data variables with transcripts                               ####

allData_transcript <- lvClean_transcript %>% 
    select(transcript_id, caption_text_clean) %>% 
    right_join(allData_transcriptLevel)

##  ............................................................................
##  define stop words                                                       ####

custom_stopwords <- c("pause",
                      "music",
                      "um",
                      "uh",
                      "okay",
                      "really",
                      "hi",
                      "hello",
                      "goodbye",
                      "bye",
                      "thanks",
                      "thank you",
                      "oh",
                      "please",
                      "mr",
                      "mrs",
                      "dr",
                      "sir",
                      "just",
                      "thank",
                      "like",
                      "alright",
                      "welcome",
                      "good bye",
                      "fellas",
                      "y'all",
                      "yeah")

#   ____________________________________________________________________________
#   define keywords of interest                                             ####

##  ............................................................................
##  disaster words                                                          ####

disasters <- quanteda::dictionary(list(
    storm = c("hurricane",
            "cyclone",
            "typhoon",
            "tropical storm",
            "tropical depression"),
    wind = c("tornado",
            "wind",
            "high wind"),
    fire = c("wildfire",
             "fire"),
    flood = c("flooding",
            "flood",
            "flooded",
            "floods"),
    heat = c("extreme heat",
             "heat wave",
             "heatwave",
             "heat",
             "drought"),
    climate = c("global warming",
                "climate change")))

## using wildcards
disasters_wildcards <- quanteda::dictionary(list(
    storm = c("hurricane*",
              "cyclone*",
              "typhoon*",
              "tropical storm*",
              "tropical depression*",
              "storm*"),
    wind = c("tornado*",
             "wind*",
             "high wind*"),
    fire = c("wildfire*",
             "fire*"),
    flood = c("flood*"),
    cold = c("ice storm",
             "sleet",
             "freeze",
             "ice*",
             "snow*",
             "hail*"),
    heat = c("extreme heat",
             "heat wave*",
             "heatwave*",
             "heat*",
             "drought*"),
    other_hazards = c("landslide*",
                      "temperature"),
    climate = c("global warming",
                "climate change")))


##  ............................................................................
##  government services words                                               ####

# gov_services = quanteda::dictionary(list(
#     services = c('drainage', 
#                  'storm water',
#                  'stormwater',
#                  'insufficient services',
#                  'sea level rise',
#                  'erosion'),
#     relief = c('relief',
#                'rebuild',
#                'relocation',
#                'recovery'),
#     prep = c('emergency management',
#              'emergency',
#              'mitigation')))


##  ............................................................................
##  hurricanes                                                              ####

# hurricanes = tolower(list('hurricane Ike',
#                     'hurricane Harvey',
#                     'hurricane Dolly',
#                     'hurricane Laura',
#                     'hurricane Rita',
#                     'hurricane Andrew',
#                     'hurricane Irma',
#                     'hurricane Imelda',
#                     'hurricane Ian',
#                     'hurricane Katrina'))

##  ............................................................................
##  relocation words                                                        ####

# relocation = tolower(list('relocation',
#                           'buyout',
#                           'buy out',
#                           'temporary relocation',
#                           'permanent relocation',
#                           'private buyout',
#                           'private buy out',
#                           'public buyout',
#                           'public buy out'))

##  ............................................................................
##  general words                                                           ####

# general_words <- tolower(list('adaptation',
#                               'insurance',
#                               'insurance rates',
#                               'gray infrastructure',
#                               'infrastructure',
#                               'blue infrastructure',
#                               'green infrastructure',
#                               'risk',
#                               'risk perception',
#                               'weather change',
#                               'preparedness',
#                               'prepare',
#                               'resilience',
#                               'resilient'))

##  ............................................................................
##  nbo words                                                               ####

# nbo <- tolower(list('residential voluntary association',
#                     'property owner associations',
#                     'home owners association',
#                     'HOA',
#                     'HOA Board',
#                     'Property Board',
#                     'NGO',
#                     'CBO',
#                     'mutual aid'))

##  ............................................................................
##  government words                                                        ####

# gov_words <- tolower(list('informal organizing',
#                           'organizing',
#                           'activism',
#                           'federal',
#                           'state',
#                           'state government',
#                           'county',
#                           'city',
#                           'special district',
#                           'incorporation',
#                           'incorporated',
#                           'unincorporated',
#                           'polycentric',
#                           'jurisdiction',
#                           'annexation',
#                           'deannexation',
#                           'funding',
#                           'tax credit',
#                           'subsidies',
#                           'grant',
#                           'grants',
#                           'zoning',
#                           'regulation',
#                           'regulations',
#                           'social services',
#                           'water',
#                           'sanitation',
#                           'parks',
#                           'trees',
#                           'streets',
#                           'public safety',
#                           'food',
#                           'shelter',
#                           'temporary shelter',
#                           'electric',
#                           'electric company',
#                           'power grid'))

##  ............................................................................
##  housing words                                                           ####

# housing <- tolower(list('property',
#                         'property values',
#                         'affordable housing',
#                         'public housing',
#                         'shelters',
#                         'trailer park',
#                         'trailer parks',
#                         'mobile homes',
#                         'mobile home',
#                         'short term rental',
#                         'short term rentals',
#                         'Air B&B',
#                         'air b and b',
#                         'airbandb',
#                         'airbnb',
#                         'air bnb',
#                         'unhoused',
#                         'homeless'))

##  ............................................................................
##  participation words                                                     ####

# participation <- tolower(list('city council',
#                               'county commission',
#                               'meeting',
#                               'community outreach',
#                               'political participation',
#                               'voting',
#                               'leadership',
#                               'activism',
#                               'protest',
#                               'turnout',
#                               'partisanship',
#                               'polarization'))

##  ............................................................................
##  communication words                                                     ####

# communication <- tolower(list('social media',
#                               'Facebook',
#                               'NextDoor',
#                               'Next Door',
#                               'Twitter',
#                               'TikTok',
#                               'Tik Tok',
#                               'BlueSky',
#                               'Blue Sky',
#                               'whatsapp',
#                               'whats app',
#                               'discord',
#                               'texting',
#                               'slack',
#                               'texted',
#                               'television',
#                               'TV',
#                               'newsletter',
#                               'newsletters',
#                               'news letters',
#                               'news'))

##  ............................................................................
##  community words                                                         ####

# community <- tolower(list('social trust',
#                           'trust',
#                           'community',
#                           'social capital',
#                           'inequality',
#                           'inequalities',
#                           'social vulnerability',
#                           'vulnerable',
#                           'vulnerability',
#                           'demographic change',
#                           'demographics',
#                           'age',
#                           'education',
#                           'technology',
#                           'language',
#                           'class',
#                           'income',
#                           'poverty',
#                           'ethnicity',
#                           'ethnic',
#                           'race',
#                           'immigration',
#                           'documentation',
#                           'dependents',
#                           'care workers',
#                           'care worker',
#                           'LGTBQIA+',
#                           'LGTBQIA',
#                           'homeowner',
#                           'home owner',
#                           'renter',
#                           'disability',
#                           'housing tenure'))

##  ............................................................................
##  economic words                                                          ####
# 
# economic <- tolower(list('economic development',
#                          'economy',
#                          'money',
#                          'space x',
#                          'nasa',
#                          'tourism'))
































