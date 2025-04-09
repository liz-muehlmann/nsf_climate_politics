############################################################################# ##
##                                                                            ##      
## This file uses quanteda and tidytext to analyze the Local View data        ## 
##                                                                            ##  
##                              ALL DOCUMENTS                                 ##  
##                                                                            ##  
## Local View data is available here:                                         ##
## https://doi.org/10.7910/DVN/NJTBEM                                         ##
##                                                                            ##     
############################################################################# ##

#   ____________________________________________________________________________
#   load libraries and data                                                 ####

source("./code/preliminaries/textAnalysis_prelims.r")

#   ____________________________________________________________________________
#   create corpus                                                           ####

corpus <- corpus(allData_transcript, text_field = "caption_text_clean")

#   ____________________________________________________________________________
#   create tokens                                                           ####

tokens <- corpus %>% 
    quanteda::tokens(remove_punct = FALSE,
                     remove_url = FALSE,
                     remove_symbols = FALSE,
                     remove_numbers = FALSE) %>% 
    tokens_remove(c(stopwords("en"), custom_stopwords)) %>% 
    tokens_tolower()

##  ............................................................................
##  assign document variables                                               ####

docvars_df <- docvars(tokens)
docvars_df$docname <- paste("text", 1:nrow(docvars_df), sep = "")

#   ____________________________________________________________________________
#   keywords in context                                                     ####

## list of options for keywords available in the text analysis prelim file
kwic_disasters <- kwic(tokens, pattern = disasters, window = 20)         

## make kwic searchable                  
kwicDisasters_df <- merge(kwic_disasters, docvars_df, by = "docname")             

# remove lines that mention windows instead of wind and fire department instead of fire
kwicDisasters_df <- kwicDisasters_df %>% 
    filter(keyword != "window", 
           !(keyword == "fire" &
           str_detect(word(post), "department"))) %>% 
    group_by(transcript_year) %>% 
    mutate(n_transcripts_year = n_distinct(transcript_id),
           n_unique_pattern_year = n_distinct(pattern)) %>% 
    group_by(transcript_year, pattern) %>% 
    mutate(n_pattern_mentions = n(),
           prop_unique_pattern_year = n_unique_pattern_year/n_transcripts_year,
           prop_pattern_mentions = n_pattern_mentions/n_transcripts_year) %>% 
    ungroup()

## subset kwic sample
kwicDisastersDF_sample <- kwicDisasters_df %>% 
    group_by(pattern) %>% 
    slice_sample(n = 50) %>% 
    ungroup()

## save kwic sample
# write.csv(kwicDisastersDF_sample,
#           "./results/samples/250325_ccgw_kwic_disasters.csv",
#           row.names = FALSE)

#   ____________________________________________________________________________
#   overall pattern summary                                                 ####

pattern_summary <- kwicDisasters_df %>%
    select(transcript_year, pattern, n_transcripts_year, n_unique_pattern_year, 
           n_pattern_mentions, prop_unique_pattern_year, prop_pattern_mentions)

# write.csv(pattern_summary, "./results/samples/250325_pattern_summary.csv", row.names = FALSE)

##  ............................................................................
##  overall pattern graph - count                                           ####

## pattern count
pattern_graph_count <- pattern_summary %>% 
    ggplot(aes(
        x = transcript_year,
        y = n_pattern,
        group = pattern,
        color = pattern)) +
    geom_line()

# ggsave("./results/visualizations/250325_pattern_summary_graph.jpg", pattern_graph)

##  ............................................................................
##  overall pattern graph - proportion                                      ####

## pattern count
pattern_graph_count <- pattern_summary %>% 
    ggplot(aes(
        x = transcript_year,
        y = prop_pattern_mentions,
        group = pattern,
        color = pattern)) +
    geom_line()

# ggsave("./results/visualizations/250325_pattern_summary_graph.jpg", pattern_graph)


##  ............................................................................
##  separate kwic by pattern & save                                         ####          

patterns <- c("storm", "wind", "fire", "flood", "heat", "climate")

for(p in patterns){
    pat <- kwicDisasters_df %>% 
        filter(pattern == p) 
    assign(paste(p), pat)
}


# save kwic by patterns
for(p in patterns) {
    pat <- get(p) %>% 
        group_by(transcript_year, keyword) %>% 
        summarize(n_keyword_year = n(),
                  n_transcripts_year = n_distinct(transcript_id))
    
    assign(paste(p, "_summary", sep=""), pat)
    
    # write.csv(get(paste0(p, "_summary", sep="")),
    #           paste0("./results/samples/250325_",
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
    #        filename = paste0("./results/visualizations/250325_", p, "_graph.jpg", sep = ""))
}

##  ............................................................................
##   pull sample kwic transcripts & save                                    ####

transcript_pull <- kwicDisastersDF_sample %>%
    group_by(pattern) %>%
    slice_sample(n = 2) %>%
    ungroup()

transcripts <- allData_transcriptLevel %>%
    filter(transcript_id %in% transcript_pull$transcript_id) %>%
    left_join(transcript_pull)

for (i in 1:nrow(transcripts)) {
    doc <- read_docx()

    # Add a title
    doc <- doc %>%
        body_add_par("Meeting Details", style = "heading 1")

    # Add all values at the top
    for (col in names(transcripts)) {
        doc <- doc %>%
            body_add_par(paste(col, ":", transcripts[i, col]), style = "Normal")
    }

    # Add the caption text below
    doc <- doc %>%
        body_add_par("Caption Text:", style = "heading 2") %>%
        body_add_par(transcripts[i, "caption_text_clean"], style = "Normal")

    transcript_year <- transcripts[i, "transcript_year"]
    keyword <- transcripts[i, "keyword"]
    transcript_id <- transcripts[i, "transcript_id"]

    # Save the document
    doc_name <- paste0("./results/samples/transcripts/",
                       transcript_year, "_",
                       keyword, "_",
                       transcript_id, ".docx")
    print(doc, target = doc_name)
    message("Created: ", doc_name)
}

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

