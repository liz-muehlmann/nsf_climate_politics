################################################################################
##                                                                            ##
## This file creates several visualizations regarding the Local View data     ##
##                                                                            ##
## geographic boundaries downloaded using the tidy census data                ##
##                                                                            ##
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##
## Political data was downloaded from the Harvard Dataverse for Algara &      ##
##    Sharif (2021) "Replication Data for: Partisanship & Nationalization     ##
##    in American Elections: Evidence from Presidential, Senatorial, &        ##
##    Gubernatorial Elections in the U.S. Counties, 1872-2020",               ##
##      https://doi.org/10.7910/DVN/DGUMFI                                    ##
##                                                                            ##
## ACS data was downloaded using the Tidy Census package                      ##
##      2006-2010 ACS > 2008                                                  ##
##      2011-2015 ACS > 2012 Election                                         ##
##      2016-2020 ACS > 2016 & 2020 Elections                                 ##
##                                                                            ##
## Election + ACS + County data were merged in                                ##
##      Code/Political/AlgaraACSPlaces.r                                      ##
##                                                                            ##
## ** Majority counties were determined using ArcGIS' Tabulate Intersection   ##
##    function and processed using Google Docs.                               ##
##    Methodology is available upon request.                                  ##
##                                                                            ##
################################################################################


#   ____________________________________________________________________________
#   load libraries and data                                                 ####

library(tidyverse)
library(ggplot2)
library(ggridges)
library(ggExtra)
library(sf)
library(tigris)
library(shiny)
library(sjPlot)
library(sjlabelled)
library(sjmisc)

load("./data/modified/all_data/allData_transcriptLevel.rdata")
load("./data/modified/all_data/allData_State.rdata")

## state abbreviations
states_abbr <- states() %>%
  filter(STATEFP <= "56" & STATEFP != "02") %>%
  select(NAME, STUSPS) %>%
  rename(
    state_name = NAME,
    state_abbr = STUSPS
  ) %>%
  st_drop_geometry()

## join state abbreviation with data
counties <- allData_transcriptLevel %>%
  left_join(s_abbr) %>%
  mutate(vp_factor = case_when((DVP*100) >= 0 & (DVP*100) <= 20.99 ~ "0-20",
                               (DVP*100) >= 21 & (DVP*100) <= 40.99 ~ "21-40",
                               (DVP*100) >= 41 & (DVP*100) <= 60.99 ~ "41-60",
                               (DVP*100) >= 61 & (DVP*100) <= 80.99 ~ "61-80", 
                               (DVP*100) >= 81 ~ "81-100"))

## state level data
states <- allData_state %>% 
  left_join(s_abbr) %>% 
  mutate(vp_factor = case_when((state_DVP*100) >= 0 & (state_DVP*100) <= 20.99 ~ "0-20",
                               (state_DVP*100) >= 21 & (state_DVP*100) <= 40.99 ~ "21-40",
                               (state_DVP*100) >= 41 & (state_DVP*100) <= 60.99 ~ "41-60",
                               (state_DVP*100) >= 61 & (state_DVP*100) <= 80.99 ~ "61-80", 
                               (state_DVP*100) >= 81 ~ "81-100"),
         vp_grey = ifelse(state_ccgwBinary != 0, vp_factor, "No CC Mention")) %>%
  select(
    transcript_year, state_name, state_abbr, state_n_transcripts,
    state_n_ccgwMentions, state_n_ccgwMentions, state_prop_scriptCCGW, 
    state_prop_ccgwMentions, state_DVP, state_RVP,
    vp_factor, vp_grey) %>%
  distinct(transcript_year, state_name, .keep_all = TRUE)

# write to csv
# write.csv(states, "./Data/LocalView/LVModifiedData/states_propsum.csv", row.names=FALSE)

################################################################################
##                                                                            ##
##                   density plot of household income                         ##
##                                                                            ##
################################################################################
dens <- ggplot(allData_transcriptLevel, aes(x = log(med_hhic))) +
  geom_density() +
  geom_vline(aes(xintercept = mean(log(med_hhic))),
    color = "blue", linetype = "dashed", size = 1
  )

################################################################################
##                                                                            ##
##               point graph, state cc mention, by year                       ##
##                                                                            ##
################################################################################
colors_points <- c("#EF4056", "#8B0015", "#3B75E9", "#0000ff", "#D4D4D4", "#FFFFFF")

## proportions
states %>%
  ggplot(aes(x = transcript_year, y = state_prop_ccgwMentions, 
             group = state_name, 
             color = vp_grey)) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_points, 
                     name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(aes(label = ifelse(state_prop_ccgwMentions >= 15, 
                               as.character(state_abbr), "")), hjust = 1.5, 
            vjust = 0, color = "black", size = 3) +
  labs(
    title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Labeled points indicate states with a proportion of 15% or more.")

## count
states %>%
  ggplot(aes(x = transcript_year, y = state_n_ccgwMentions, group = state_name, 
             color = vp_grey)) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_points, 
                     name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(aes(label = ifelse(state_n_ccgwMentions >= 5, 
                               as.character(state_abbr), "")), hjust = 1.5, 
            vjust = 0, color = "black", size = 3) +
  labs(
    title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Labeled points indicate states with 5 or more 
    climate change mentions"
  )


################################################################################
##                                                                            ##
##                             hexbin usa                                     ##
##                                                                            ##
################################################################################
s <- read_sf("./data/original/states_hexgrid.gpkg") %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>%
  left_join(states, by = c("google_name" = "state_name"))

hexbin <- ggplot(s) +
  geom_sf(aes(fill = state_n_ccgwMentions)) +
  geom_sf_text(aes(label = iso3166_2)) +
  theme_void()

################################################################################
##                                                                            ##
##                             blue state DVP                                 ##
##                                                                            ##
################################################################################
ggplot(states, aes(x=transcript_year, y=state_name, fill=state_DVP)) +
    scale_color_brewer(palette = "Blues") +
    geom_tile(color = "black") +
    geom_text(data = subset(states, state_n_ccgwMentions > 0), aes(label=state_n_ccgwMentions, color="white")) +
    theme_minimal(base_size = 8)


################################################################################
##                                                                            ##
##       heatmap of DVP, by state, year, prop transcript number & grey        ##
##                                                                            ##
################################################################################
hm_SProp <- ggplot(states, aes(x = transcript_year, y = state_name, fill = state_prop_ccgwMentions)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#c9eac6", high = "#505d4f") +
  geom_text(data = subset(states, state_prop_ccgwMentions > 0), 
            aes(label = round(state_prop_ccgwMentions, 2)), color = "black", size = 3) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Numbers included inside the boxes is the proportion of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention.")

################################################################################
##                                                                            ##
##               heatmap of DVP, by state, year, CC transcript number         ##
##                                                                            ##
################################################################################
colors <- c("#ff6e66", "#D885A0", "#b19cd9", "#939DD1", "#759ec9")

hm_SYVP <- ggplot(states, aes(x = transcript_year, y = state_name, fill = vp_factor)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = colors, name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(data = subset(states, state_n_ccgwMentions > 0), 
            aes(label = state_n_ccgwMentions), color = "black", size = 3) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Boxes without a value had no climate change mentions. \n Rows with no transcripts were dropped.")

################################################################################
##                                                                            ##
##               heatmap of DVP, by state, year, proportion of cc             ##
##                                                                            ##
################################################################################
hm_prop_SYVP <- ggplot(states, aes(x = transcript_year, y = state_name, fill = vp_factor)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = colors, name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(data = subset(states, state_n_ccgwMentions > 0), aes(label = round(state_prop_ccgwMentions, 2)), color = "black", size = 3) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Proportion of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Numbers included inside the boxes are the proportion of transcripts for the state that include at least one mention of climate change. \n Boxes without a value had no climate change mentions. \n Rows with no transcripts were dropped.")

################################################################################
##                                                                            ##
##               heatmap of DVP, by state, year, CC transcript number & grey  ##
##                                                                            ##
################################################################################
colors_grey <- c("#ff6e66", "#D885A0", "#b19cd9", "#939DD1", "#D4D4D4", "#FFFFFF")

hmGrey_countSYVP <- ggplot(states, aes(x = transcript_year, y = state_name, fill = vp_grey)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = colors_grey, name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(data = subset(states, state_n_ccgwMentions > 0), aes(label = state_n_ccgwMentions), color = "black", size = 3) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention.")

################################################################################
##                                                                            ##
##  heatmap of DVP, by state, year, proportion of transcript number & grey    ##
##                                                                            ##
################################################################################
hmGrey_propSYVP <- ggplot(states, aes(x = transcript_year, y = state_name, fill = vp_grey)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = colors_grey, name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(data = subset(states, state_prop_ccgwMentions > 0), aes(label = round(state_prop_ccgwMentions, 2)), color = "black", size = 3) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention.")

################################################################################
##                                                                            ##
##              group by state, map county level data and save                ##
##                                                                            ##
################################################################################
colors7_grey <- c("#ff6e66", "#D885A0", "#b19cd9", "#939DD1", "#769dcc", "#D4D4D4", "#FFFFFF")

counties %>%
  filter(state_name == "Texas") %>%
  ggplot(aes(x = transcript_year, 
             y = county_name, 
             fill = vp_factor)) +
  geom_tile(color = "black") +
  scale_fill_manual(
    name = "Democratic Vote Percentage",
    values = c(
      "0-20" = "#ff6e66",
      "21-40" = "#D885A0",
      "41-60" = "#b19cd9",
      "61-80" = "#939DD1",
      "81-100" = "#769dcc",
      "No CC Mention" = "#D4D4D4",
      labels = c("0-20", "21-40", "41-60", "61-80", "81-100", "No CC Mention"))) +
  theme_minimal() +
  ggtitle("Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(data = subset(counties, 
                          n_ccgwMentions > 0 & 
                          state_name == "Texas"), 
            aes(label = n_ccgwMentions), 
                color = "black", 
                size = 3) +
  labs(
    caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention.",
    x = "County Name",
    y = "Year of Transcript")
# ggsave("./LocalView/results/graphs/TEX.jpg", height = 20, width = 8.5, limitsize = FALSE)


for (s in unique(counties$state_name)) {
  p <- counties %>%
    filter(state_name == s) %>%
    ggplot(aes(x = transcript_year, y = county_name) +
    geom_tile(color = "black") +
    scale_fill_manual(
      name = "Democratic Vote Percentage",
      values = c(
        "0-20" = "#ff6e66",
        "21-40" = "#D885A0",
        "41-60" = "#b19cd9",
        "61-80" = "#939DD1",
        "81-100" = "#769dcc",
        "No CC Mention" = "#D4D4D4",
        labels = c("0-20", "21-40", "41-60", "61-80", "81-100", "No CC Mention")
      )
    ) +
    theme_minimal() +
    ggtitle("Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(data = subset(counties, sum_scriptCC > 0 & state_name == s), aes(label = sum_scriptCC), color = "black", size = 3) +
    labs(
      caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention.",
      x = "County Name",
      y = "Year of Transcript"
    )
  # ggsave(p, filename = paste0("./LocalView/results/graphs/240629_heatmap_", s, ".jpg"), height = 11, width = 8.5)
}


#   ____________________________________________________________________________
#   histograms                                                              ####

library(strcode) # easy code separators
options(strcode = list(
  insert_with_shiny = FALSE, # set options
  char_length = 80,
  hash_in_sep = TRUE
))

load("./LocalView/data/modified/lvFema_all.rdata")
load("./LocalView/data/modified/lvFema_allDeclarations.rdata")
load("./LocalView/data/modified/lvFema_transcript.rdata")


##  ............................................................................
##  number of days between declaration and meeting; all data all years      ####

lvFema %>%
  ggplot(aes(x = days_btwn_decMeeting)) +
  geom_histogram() +
  theme_minimal() +
  labs(
    x = "days between declaration and meeting",
    title = "Number of Days between Declaration and Meeting (all data all years)"
  )


##  ............................................................................
##  number of days between declaration and meeting; transcript level        ####

lvf_transcript %>%
  ggplot(aes(x = days_btwn_decMeeting)) +
  geom_histogram() +
  theme_minimal() +
  labs(
    x = "days between declaration and meeting",
    title = "Number of Days Between Declaration and Meeting (transcript level)"
  )

##  ............................................................................
##  number of declarations in the last five years; (transcript level)       ####

lvf_transcript %>%
  ggplot(aes(x = nDec_FiveYears)) +
  geom_histogram() +
  theme_minimal() +
  labs(
    x = "Number of Declarations in the last five years",
    title = "Number of Declarations in the last five years (Transcript Level)"
  )


##  ............................................................................
##  filtered for 0-2 years, days between declaration-meeting; (Transcript level)

lvf_transcript %>%
  filter(days_btwn_decMeeting <= 730) %>%
  ggplot(aes(x = days_btwn_decMeeting)) +
  geom_histogram() +
  theme_minimal() +
  labs(
    x = "Number of days between declaration and meeting",
    title = "Days between dec and meeting (disasters between 0-2 years)"
  )

################################################################################
##                                                                            ##
##                            cc mention by year                              ##
##                                                                            ##
################################################################################

# stacked bar chart                                                         ####
allData_transcriptLevel %>% 
  group_by(transcript_year, ccgwBinary) %>% 
  summarize(n_transcript_ccgwBinary = n()) %>% 
  # distinct(transcript_year, .keep_all = TRUE) %>% 
  ggplot(aes(fill=as.factor(ccgwBinary), y=n_transcript_ccgwBinary, x=transcript_year)) + 
  geom_bar(position="stack", stat="identity") +
  theme_minimal() +
  guides(fill = guide_legend(title = "Legend:")) +
  labs(title = "Number of Transcripts by Climate Change or Global Warming Mention by Year",
       x = "Transcript Year",
       y = "Number of Transcripts") +
  scale_fill_discrete(labels=c('No Mention', 'Climate Change or Global Warming Mention')) +
  theme(legend.position = "bottom",
        plot.title = element_text(size=11))



















