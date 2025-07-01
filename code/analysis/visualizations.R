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
library(viridis)

load("./data/modified/all_data/allData_transcriptLevel.rdata")
load("./data/modified/all_data/allData_State.rdata")
load("./data/modified/all_data/allData_countyLevel_noNA.rdata")
load("./data/modified/lvFema_transcriptLevel.rdata")

#   ____________________________________________________________________________
#   custom functions                                                        ####

factor_dvp <- function(df) {
  DVP <- readline("What is the DVP variable called? ")
  df <- df %>% 
    mutate(vp_factor = case_when((!!sym(DVP)*100) >= 0 & (!!sym(DVP)*100) <= 20.99 ~ "0-20",
                                 (!!sym(DVP)*100) >= 21 & (!!sym(DVP)*100) <= 40.99 ~ "21-40",
                                 (!!sym(DVP)*100) >= 41 & (!!sym(DVP)*100) <= 60.99 ~ "41-60",
                                 (!!sym(DVP)*100) >= 61 & (!!sym(DVP)*100) <= 80.99 ~ "61-80", 
                                 (!!sym(DVP)*100) >= 81 ~ "81-100"))
}


#   ____________________________________________________________________________
#   state abbreviations                                                     ####

states_abbr <- states() %>%
  filter(STATEFP <= "56" & STATEFP != "02") %>%
  select(NAME, STUSPS) %>%
  rename(
    state_name = NAME,
    state_abbr = STUSPS) %>%
  st_drop_geometry()

#   ____________________________________________________________________________
#   factor transcript level data                                            ####

# DVP
transcript_vpFactor <- allData_transcriptLevel %>%
  left_join(states_abbr) %>%
  factor_dvp()

#   ____________________________________________________________________________
#   state level data                                                        ####

# state_DVP
states <- allData_state %>% 
  left_join(states_abbr) %>%
  factor_dvp() %>%
  select(
    transcript_year, state_name, state_abbr, state_n_transcripts,
    state_n_ccgwMentions, state_prop_scriptCCGW, 
    state_prop_ccgwMentions, state_DVP, state_RVP,
    vp_factor) %>%
  distinct(transcript_year, state_name, .keep_all = TRUE)

# write.csv(states, "./data/modified/local_view/lv_states_prop.csv", row.names=FALSE)

#   ____________________________________________________________________________
#   county level data                                                       ####

# DVP
counties <- allData_countyLevel_noNA %>% 
  left_join(states_abbr) %>% 
  factor_dvp() %>% 
  group_by(stcounty_fips) %>% 
  mutate(sum_scriptCCGW = sum(ccgwBinary))

#   ____________________________________________________________________________
#   density of log median household income                                  ####

dens <- ggplot(allData_transcriptLevel, aes(x = log(med_hhic))) +
  geom_density() +
  geom_vline(aes(xintercept = mean(log(med_hhic))),
    color = "blue", linetype = "dashed", linewidth = 1) +
  labs(x = "Log of Median Household Income",
       y = "Density",
       title = "Graph showing the density of the log of median household income") +
  theme(plot.title = element_text(hjust = 0.5))

#   ____________________________________________________________________________
#   point graphs                                                            ####

colors_points <- c("#EF4056", "#8B0015", "#3B75E9", "#cccccc")
colors_grey <- c("#ff6e66", "#D885A0", "#b19cd9", "#939DD1", "#D4D4D4", "#FFFFFF")

##  ............................................................................
##  proportion of CC/GW mention                                             ####

states %>%
  ggplot(aes(x = transcript_year, 
             y = state_prop_ccgwMentions,
             color = vp_factor)) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_points, 
                     name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(aes(label = ifelse(state_prop_ccgwMentions >= 0.33, 
                               as.character(state_abbr), "")), hjust = 1.5, 
            vjust = 0, color = "black", size = 3) +
  labs(
    title = "Proportion of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Labeled points indicate states with a proportion of 15% or more.",
    x = "Transcript Year",
    y = "Proportion of Climate Change/Global Warming Mentions (State)") +
  theme(plot.title = element_text(hjust = 0.5))

##  ............................................................................
##  number of CC/GW mention                                                 ####

states %>%
  ggplot(aes(x = transcript_year, y = state_n_ccgwMentions, group = state_name, 
             color = vp_factor)) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_points, 
                     name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(aes(label = ifelse(state_n_ccgwMentions >= 50, 
                               as.character(state_abbr), "")), hjust = 1.5, 
            vjust = 0, color = "black", size = 3) +
  labs(
    title = "Number of transcripts with at least one mention of climate 
    change by state, year, and 2020 vote percentage",
    caption = "Labeled points indicate states with 5 or more climate change mentions",
    x = "Transcript Year",
    y = "Number of Climate Change/Global Warming Mentions (State)") +
  theme(plot.title = element_text(hjust = 0.5))

#   ____________________________________________________________________________
#   hexbin usa                                                              ####

s <- read_sf("./data/original/states_hexgrid.gpkg") %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>%
  left_join(states, by = c("google_name" = "state_name")) %>% 
  filter(iso3166_2 != "AK" &
         iso3166_2 != "HI")

s$bin <- cut(s$state_n_ccgwMentions,
                     breaks = c(seq(0, 25, 5), Inf),
                     labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26+"),
                     include.lowest = TRUE)

magma_palette <- rev(magma(7))
states_above_26mentions <- c("CA", "AZ", "WA","OR", "MN", "NM", "IL", "TX", "NC", "NY", "MA", "NJ")

ggplot(s) +
  geom_sf(aes(fill = bin)) +
  geom_sf_text(aes(label = iso3166_2), 
               color = ifelse(s$iso3166_2 %in% states_above_26mentions,
                                                      "#cccccc", "#000")) +
  theme_void() +
  scale_fill_manual(values = magma_palette,
    name = "Number CC/GW Mentions",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth = unit(12, units = "mm"),
      label.position = "bottom", title.position = "top", nrow = 1)) +
  theme_void() +
  ggtitle("Map of Number of CC/GW Mentions by State") +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#000"), 
    plot.title = element_text(
      size = 22, hjust = 0.5, color = "#000",
      margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))

#   ____________________________________________________________________________
#   heatmaps                                                                ####

ggplot(states, aes(x=transcript_year, y=state_name, fill=state_DVP)) +
    scale_color_brewer(palette = "Blues") +
    geom_tile(color = "black") +
    geom_text(data = subset(states, state_n_ccgwMentions > 0), aes(label=state_n_ccgwMentions), color="white") +
    theme_minimal(base_size = 8) +
  ggtitle("Number of transcripts with at least one mention of climate change by state and year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Transcript Year",
       y = "State Name") + 
  guides(fill = guide_legend(title = "Democratic Vote Percentage"))


##  ............................................................................
##  DVP, state, year, prop transcript                                       ####

ggplot(states, 
       aes(x = transcript_year, 
       y = state_name, 
       fill = state_prop_ccgwMentions)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#c9eac6", high = "#505d4f") +
  geom_text(data = subset(states, state_prop_ccgwMentions > 0), 
            aes(label = round(state_prop_ccgwMentions, 2)), color = "black", size = 3) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Numbers included inside the boxes is the proportion of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention.") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Transcript Year",
       y = "State Name") + 
  guides(fill = guide_legend(title = "Proportion of CC/GW Mentions"))


##  ............................................................................
##  DVP, state, year, number ccgw                                           ####

colors <- c("#ff6e66", "#D885A0", "#b19cd9", "#939DD1", "#759ec9")

ggplot(states, aes(x = transcript_year, y = state_name, fill = vp_factor)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = colors, name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(data = subset(states, state_n_ccgwMentions > 0), 
            aes(label = state_n_ccgwMentions), color = "black", size = 3) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Boxes without a value had no climate change mentions. \n Rows with no transcripts were dropped.") +
  labs(x = "Transcript Year",
       y = "State Name")


##  ............................................................................
##  DVP, state, year, prop ccgw                                             ####

ggplot(states, aes(x = transcript_year, y = state_name, fill = vp_factor)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = colors, name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(data = subset(states, state_n_ccgwMentions > 0), aes(label = round(state_prop_ccgwMentions, 2)), color = "black", size = 3) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Proportion of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Numbers included inside the boxes are the proportion of transcripts for the state that include at least one mention of climate change. \n Boxes without a value had no climate change mentions. \n Rows with no transcripts were dropped.") +
  labs(x = "Transcript Year",
       y = "State Name")


##  ............................................................................
##  DVP, state, year, number ccgw 

ggplot(states, aes(x = transcript_year, y = state_name, fill = vp_factor)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = colors_grey, name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(data = subset(states, state_n_ccgwMentions > 0), aes(label = state_n_ccgwMentions), color = "black", size = 3) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Number of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention.") +
  labs(x = "Transcript Year",
       y = "State Name")


##  ............................................................................
##  DVP, state, year, prop ccgw

ggplot(states, aes(x = transcript_year, y = state_name, fill = vp_factor)) +
  geom_tile(color = "black") +
  scale_fill_manual(values = colors_grey, name = "Democratic Vote Percentage") + # Apply manual colors
  geom_text(data = subset(states, state_prop_ccgwMentions > 0), aes(label = round(state_prop_ccgwMentions, 2)), color = "black", size = 3) +
  theme_minimal(base_size = 8) +
  labs(
    title = "Proportion of transcripts with at least one mention of climate change by state, year, and 2020 vote percentage",
    caption = "Numbers included inside the boxes are the number of transcripts for the state that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention.") +
  labs(x = "Transcript Year",
       y = "State Name")


##  ............................................................................
##  counties                                                                ####

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


for (s in unique(counties$state_name)){
  p <- counties %>%
    filter(state_name == s) %>%
    ggplot(aes(x = transcript_year, y = county_name)) +
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
             geom_text(data = subset(counties, sum_scriptCCGW > 0 & state_name == s), aes(label = sum_scriptCCGW), color = "black", size = 3) +
             labs(
               caption = "Numbers included inside the boxes are the number of transcripts for the county that include at least one mention of climate change. \n Grey boxes have transcripts but no CC mention.",
               x = "County Name",
               y = "Year of Transcript")
           
  # ggsave(p, filename = paste0("./results/visualizations/county_heatmaps/250627_heatmap_", s, ".jpg"), height = 11, width = 8.5)
}

#   ____________________________________________________________________________
#   LV-FEMA histograms                                                      ####

##  ............................................................................
##  days between dec/meeting all                                            ####

lvFema_transcriptLevel %>%
  ggplot(aes(x = months_btwn_decMeeting)) +
  geom_histogram() +
  theme_minimal() +
  labs(
    x = "Days Between Declaration and Meeting",
    y = "Number of Transcripts",
    title = "Number of Days Between Declaration and Meeting (transcript level)")

##  ............................................................................
##  months between dec/meeting five years                                   ####

lvFema_transcriptLevel %>%
  ggplot(aes(x = nDec_fiveYears)) +
  geom_histogram() +
  theme_minimal() +
  labs(
    x = "Number of Declarations in the last five years",
    y = "Number of Transcripts",
    title = "Number of Declarations in the last five years (transcript Level)")


##  ............................................................................
##  months between dec/meeting two years                                    ####

lvFema_transcriptLevel %>%
  filter(months_btwn_decMeeting <= 36) %>%
  ggplot(aes(x = months_btwn_decMeeting)) +
  geom_histogram() +
  theme_minimal() +
  labs(
    x = "Number of Months Between Declaration and Meeting",
    y = "Number of Transcripts",
    title = "Months Between Declaration and Meeting (disasters between 0-2 years)")


#   ____________________________________________________________________________
#   stacked bar chart                                                       ####

## ccgw mention by year
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



















