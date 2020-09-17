library(dplyr)

project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"

ingestion_results <- "working/SEC_Packaging/RUN1_INGESTION/"


ingestion_filings <- list.files(paste0(project_folder, ingestion_results))

ingestion_paths <- paste0(project_folder, ingestion_results, ingestion_filings)

#373 - this didn't process for some reason...
# i = 373
for (i in 1:length(ingestion_paths)) {
  print(i)
  
  filing <- readRDS(ingestion_paths[i])
  filing_words <- capture_words(path = ingestion_paths[i], txt_string = filing, 
                                brand_char_max = 5,
                                char_min = 3, char_max = 25, no_num = TRUE, camel = TRUE, camel_char_thres = 20, encoding = FALSE)
  
  saveRDS(filing_words, file = paste0(project_folder, "working/SEC_Packaging/RUN2_WORDCAPTURE/", ingestion_filings[i]))
  
  print(i/length(ingestion_paths))
}

# length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN2_WORDCAPTURE/")))
