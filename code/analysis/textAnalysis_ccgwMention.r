############################################################################# ##
##                                                                            ##      
## This file uses quanteda and tidytext to analyze the Local View data        ## 
##                                                                            ##  
##          DOCUMENTS WITH A CLIMATE CHANGE OR GLOBAL WARMING MENTION         ##
##                                                                            ##  
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##     
############################################################################# ##

#   ____________________________________________________________________________
#   load libraries and data                                                 ####

source("./LocalView/code/processing_scripts/textAnalysis_prelims.r")

#   ____________________________________________________________________________
#   subset transcripts with climate mention                                 ####

## n = 4216
allData_mention <- allData_transcript %>% 
    group_by(transcript_year) %>% 
    mutate(n_transcripts_year = n()) %>% 
    filter(ccgwBinary == 1) %>% 
    mutate(n_ccgw_transcript_year = n(),
           prop_ccgw_transcripts_year = n_ccgw_transcript_year/n_transcripts_year) 

#   ____________________________________________________________________________
#   remove unnecessary data sets                                            ####

rm(allData_transcript, allData_transcriptLevel, lvClean_transcript)

#   ____________________________________________________________________________
#   create corpus                                                           ####

## n = 4216
corpus <- corpus(allData_mention, text_field = "caption_text_clean")

#   ____________________________________________________________________________
#   create tokens                                                           ####

tokens <- corpus %>% 
    quanteda::tokens(remove_punct = FALSE,
                     remove_url = TRUE,
                     remove_symbols = TRUE,
                     remove_numbers = FALSE) %>% 
    tokens_remove(c(stopwords("en"), 
                    custom_stopwords)) %>% 
    tokens_tolower()

##  ............................................................................
##  assign document variables                                               ####

docvars_df <- docvars(tokens)
docvars_df$docname <- paste("text", 1:nrow(docvars_df), sep = "")

#   ____________________________________________________________________________
#   create keywords in context                                              ####

## list of options for keywords available in the text analysis prelim file
## n = 60.246
kwic_disasters <- kwic(tokens, pattern = disasters_wildcards, window = 20)         

## make kwic searchable                  
kwicDisasters_df <- merge(kwic_disasters, docvars_df, by = "docname")             

## remove lines that mention windows instead of wind 
## remove lines mentioning the fire department instead of fire
## n = 55,538
kwicDisasters_df <- kwicDisasters_df %>% 
    filter(!keyword %in% c("wind-up" ,
                        "wind-ups" ,
                        "winda" ,
                        "windage" ,
                        "windale" ,
                        "windbreak" ,
                        "windbreaker" ,
                        "windbreakers" ,
                        "windbreaks" ,
                        "winded" ,
                        "windell" ,
                        "windenwood" ,
                        "winder" ,
                        "windermere" ,
                        "winders" ,
                        "windex" ,
                        "windfall" ,
                        "windfalls" ,
                        "windgren" ,
                        "windham" ,
                        "windhoek" ,
                        "winding" ,
                        "windish" ,
                        "windjammer" ,
                        "windlass" ,
                        "windle's" ,
                        "windler" ,
                        "windley" ,
                        "window's" ,
                        "window-based" ,
                        "windower" ,
                        "windowless" ,
                        "windows" ,
                        "windows-based" ,
                        "windowsdressers.org" ,
                        "windowsill" ,
                        "windowsills" ,
                        "windowski" ,
                        "windowsofunderstanding.org" ,
                        "windpipe" ,
                        "windridge" ,
                        "windrock" ,
                        "windrow" ,
                        "windrowing" ,
                        "windrows" ,
                        "windscreen" ,
                        "windscreen's" ,
                        "windscreens" ,
                        "windshield" ,
                        "windshields" ,
                        "windside" ,
                        "windsock" ,
                        "windsocks" ,
                        "windsong" ,
                        "windsor" ,
                        "windsor's" ,
                        "windsors" ,
                        "windstream" ,
                        "windsurfing" ,
                        "windus" ,
                        "windville" ,
                        "windville's"), 
           (!keyword %in% c("firewood" ,
                          "firework" ,
                          "fireworkers" ,
                          "fireworks" ,
                          "firewos" ,
                          "fireteam" ,
                          "firetrap" ,
                          "firetruck" ,
                          "firetrucks" ,
                          "firewalall" ,
                          "firewall" ,
                          "firewalled" ,
                          "firewalls" ,
                          "fireside" ,
                          "firestation" ,
                          "firestein" ,
                          "firestick" ,
                          "firestone" ,
                          "fireman" ,
                          "fireman's" ,
                          "firemanic" ,
                          "firematic" ,
                          "firemed" ,
                          "firemen" ,
                          "firemen's" ,
                          "firepay" ,
                          "firepeep" ,
                          "fireplace" ,
                          "fireplaces" ,
                          "fireplug" ,
                          "firepower" ,
                          "fireghters" ,
                          "firehawk" ,
                          "firehose" ,
                          "firehouse" ,
                          "firehouses" ,
                          "firefhting" ,
                          "firefighrs" ,
                          "firefight" ,
                          "firefighte" ,
                          "firefighter" ,
                          "firefighter's" ,
                          "firefighters" ,
                          "firefighthters" ,
                          "firefighting" ,
                          "firefights" ,
                          "fireflies" ,
                          "fireflow" ,
                          "firefly" ,
                          "firefox" ,
                          "firefoxian" ,
                          "firecracker" ,
                          "firecrackers" ,
                          "firecrate" ,
                          "fired" ,
                          "fire-pot" ,
                          "fireable" ,
                          "firearm" ,
                          "firearm-related" ,
                          "firearms" ,
                          "firebird" ,
                          "fireboat" ,
                          "firebond" ,
                          "firebraid" ,
                          "firebrand")),
           !(keyword == "fire" &
                 str_detect(word(post), "department")))
 

# storm <-  kwicDisasters_df %>% filter(keyword == "storm" &
#                                           str_detect(word(pre, -1), "\\bice\\b"))

#   ____________________________________________________________________________
#   get proportion of keyword use by year                                   ####

# total keywords in the year, proportion of each keyword

kwicDisasters_df <- kwicDisasters_df %>% 
    group_by(transcript_year, keyword) %>% 
    mutate(n_keyword_year = n()) %>% 
    ungroup() %>% 
    group_by(transcript_year, pattern) %>% 
    mutate(n_pattern_year = n()) %>% 
    ungroup() %>% 
    mutate(prop_keyword_year = n_keyword_year/n_ccgw_transcript_year,
           prop_pattern_year = n_pattern_year/n_ccgw_transcript_year)

##  ............................................................................
##  pull out all keywords and patterns                                      ####

pattern_keyword <- kwicDisasters_df %>% 
    select(transcript_year, pattern, keyword, n_ccgw_transcript_year, 
           prop_ccgw_transcripts_year, n_keyword_year, n_pattern_year, 
           prop_keyword_year, prop_pattern_year) %>% 
    group_by(keyword) %>% 
    distinct(keyword, .keep_all = TRUE)

# write.csv(pattern_keyword, "./LocalView/results/summaries/250226_patterns_summaries_all.csv", row.names = FALSE)

## subset kwic sample
kwicDisastersDF_sample <- kwicDisasters_df %>% 
    group_by(pattern) %>% 
    slice_sample(n = 50) %>% 
    ungroup()

## save kwic sample
# write.csv(kwicDisastersDF_sample,
#           "./LocalView/results/summaries/250228_ccgw_kwic_disasters.csv",
#           row.names = FALSE)

##  ............................................................................
##   create kwic pattern summary                                            ####

pattern_summary <- kwicDisasters_df %>% 
    group_by(transcript_year, pattern) %>% 
    summarize(n_pattern = n(),
              n_transcripts = n_distinct(transcript_id)) 

# write.csv(pattern_summary, 
#           "./LocalView/results/summaries/250228_ccgw_pattern_summary.csv",
#           row.names = FALSE)


##  ............................................................................
##  create kwic pattern summary graph                                       ####

pattern_graph <- pattern_summary %>% 
    ggplot(aes(
        x = transcript_year,
        y = prop_pattern_year,
        group = pattern,
        color = pattern)) +
    geom_line()


##  ............................................................................
##  separate kwic by pattern & save                                         ####          

patterns <- c("storm", "wind", "fire", "flood", "cold", "heat", "other_hazards", "climate")

for(p in patterns){
    pat <- kwicDisasters_df %>% 
        filter(pattern == p)
    assign(paste(p), pat)
}

# save kwic by patterns
for(p in patterns) {
    pat <- get(p) %>% 
        group_by(transcript_year, keyword) %>% 
        summarize(n_keyword = n(),
                  n_transcripts = n_distinct(transcript_id))
    
    assign(paste(p, "_summary", sep=""), pat)
    
    # write.csv(get(paste0(p, "_summary", sep="")),
    #           paste0("./LocalView/results/summaries/250228_",
    #                  p,
    #                  "_summary.csv"),
    #           row.names = FALSE)
}

##  ............................................................................
##  plot kwic pattern summaries & save                                      ####

for(p in patterns){
    g <- get(paste0(p, "_summary", sep = "")) %>% 
        ggplot(aes(x = transcript_year,
                   y = n_keyword,
                   group = keyword, 
                   color = keyword)) +
        geom_line()
    
    assign(paste(p, "_graph", sep = ""), g)
    
    # ggsave(g,
    #        filename = paste0("./LocalView/results/graphs/250228_", p, 
    #                          "_graph.jpg", sep = ""))
}

##  ............................................................................
##   pull sample kwic transcripts & save                                    ####

# transcript_pull <- kwic_sample %>% 
#     group_by(pattern) %>% 
#     slice_sample(n = 2) %>% 
#     ungroup() 
# 
# transcripts <- allData_transcriptLevel %>% 
#     filter(transcript_id %in% transcript_pull$transcript_id) %>% 
#     left_join(transcript_pull)
# 
# for (i in 1:nrow(transcripts)) {
#     doc <- read_docx()
#     
#     # Add a title
#     doc <- doc %>%
#         body_add_par("Meeting Details", style = "heading 1")
#     
#     # Add all values at the top
#     for (col in names(transcripts)) {
#         doc <- doc %>%
#             body_add_par(paste(col, ":", transcripts[i, col]), style = "Normal")
#     }
#     
#     # Add the caption text below
#     doc <- doc %>%
#         body_add_par("Caption Text:", style = "heading 2") %>%
#         body_add_par(transcripts[i, "caption_text_clean"], style = "Normal")
#     
#     transcript_year <- transcripts[i, "transcript_year"]
#     keyword <- transcripts[i, "keyword"]
#     transcript_id <- transcripts[i, "transcript_id"]
#     
#     # Save the document
#     doc_name <- paste0("./LocalView/results/sample_transcripts/", 
#                        transcript_year, "_", 
#                        keyword, "_", 
#                        transcript_id, ".docx")
#     print(doc, target = doc_name)
#     message("Created: ", doc_name)
# }


#   ____________________________________________________________________________
#   subset kwic transcripts                                                 ####

ccgw_disasters <- corpus_subset(corpus, docnames(corpus) %in% kwicDisasters_df$docname)

#   ____________________________________________________________________________
#   create document-feature matrix                                          ####

dfm <- dfm(
    tokens,
    tolower = TRUE,
    remove_padding = FALSE)

dfm_sorted <- dfm_sort(dfm, 
                       decreasing = TRUE, 
                       margin = "both")

#   ____________________________________________________________________________
#   create term-frequency inverse term-frequency tf-idf                     ####

tf_idf <- tf_idf(dfm)

