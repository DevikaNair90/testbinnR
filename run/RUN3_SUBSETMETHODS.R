library(dplyr)
project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"
captures_folder <- "working/SEC_Packaging/RUN2_WORDCAPTURE/"

captures_path <- list.files(paste0(project_folder, captures_folder))

captures_paths <- paste0(project_folder, captures_folder, captures_path)
length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/")))

for (i in 1:length(captures_paths)) {
  print(i)
  
  allwords <- readRDS(captures_paths[i])
  
  allwords <-  allwords %>%
    mutate(Captures_ = ifelse(stringr::str_detect(Captures, "(®|™)$"), 
                              stringr::str_remove(Captures, "(®|™)$"), 
                              Captures), 
           low = stringr::str_to_lower(Captures_), 
           eng_C = hunspell::hunspell_check(Captures_), 
           eng_l = hunspell::hunspell_check(low)
           )
  
  allwords <- cbind(allwords, allwords$low %>% testbinnR::refine_captures())
  saveRDS(allwords, paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/", captures_path[i]))
  
  print(i/length(captures_paths))
  
}

# checkme <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/", captures_path[510]))

######################
######################
######################
######################

captures_paths2 <- paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/", list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/")))

#24 didn't work

for (i in 1:length(captures_paths2)) {
  print(i)
  
  allwords <- readRDS(captures_paths2[i])
  
  # allnoneng <- allwords %>% filter(eng_C == FALSE) %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_NON_ENGLISH/", captures_path[i]))
  # brand <- allwords %>% filter(!is.na(brand)) %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/BRAND/", captures_path[i]))
  # launch <- allwords %>% get_proximity_words(mode = "string", search = "launch", n = 20)  %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_LAUNCH/", captures_path[i]))
  
  new_loc <- allwords %>% get_proximity_words(mode = "string", search = "\\bnew(er|est)?\\b", n = 1) 
  
  if(is.data.frame(new_loc) == TRUE) {
    
    ## NEW PRODUCT
    new_product_loc <- new_loc %>%
      filter(pstn != "before") %>%
      mutate(orig_id = id) %>%
      get_proximity_words(mode = "string", search = "product", n = 1) 
    
    if(is.data.frame(new_product_loc) == TRUE) {
      new_product_loc <- new_product_loc %>% filter(pstn %in% c("before", "target"))
      
      new_product_words <- allwords %>%
        get_proximity_words(mode = "id", search = new_product_loc$orig_id, n = 20)
      
      # saveRDS(new_product_words, paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWPRODUCT/", captures_path[i]))
      saveRDS(new_product_words, paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWPRODUCT_RELAXED/", captures_path[i]))
    }
    
    ## NEW DRUG
    new_drug_loc <- new_loc %>%
      filter(pstn != "before") %>%
      mutate(orig_id = id) %>%
      get_proximity_words(mode = "string", search = "drug", n = 1) 
    
    if(is.data.frame(new_drug_loc) == TRUE) {
      new_drug_loc <- new_drug_loc %>% filter(pstn %in% c("before", "target"))
      
      new_drug_words <- allwords %>%
        get_proximity_words(mode = "id", search = new_drug_loc$orig_id, n = 20)
      
      # saveRDS(new_drug_words, paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWDRUG/", captures_path[i]))
      saveRDS(new_drug_words, paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWDRUG_RELAXED/", captures_path[i]))
      
    }
    
  }
  
  print(i/length(captures_paths))
}


##
b <- 616
checkme2 <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/", captures_path[b]))
checkme2_allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_NON_ENGLISH/", captures_path[b]))
checkme2_brand <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/BRAND/", captures_path[b]))
checkme2_launch <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_LAUNCH/", captures_path[b]))
checkme2_newprod <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWPRODUCT_RELAXED/", captures_path[b]))
checkme2_newdrug <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWDRUG_RELAXED/", captures_path[b]))



fold_res <- c(length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/"))),
length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_NON_ENGLISH/"))),
length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/BRAND/"))),
length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_LAUNCH/"))),
length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWPRODUCT/"))),
length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWDRUG/"))), 
length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWPRODUCT_RELAXED/"))),
length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWDRUG_RELAXED/"))))

results <- data.frame(RES = c("ALL WORDS ANALYZE", "ALL NON ENGLISH", "BRAND", "LAUNCH", "NEW PRODUCT 1", "NEW DRUG 1", "NEW PRODUCT 2", "NEW DRUG 2"),
           size = fold_res)

path <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/"
find <- list.files(path)
size <- file.size(paste0(path, find))
datetime <- file.mtime(paste0(path, find))
res <- data.frame("File" = find,
           "Size" = size/1024,
           "Date" = as.Date(datetime), 
           "Time" = as.factor(lubridate::hour(datetime)))


library(ggplot2)
ggplot(data = res, aes(x = Size)) + 
  geom_histogram() #+ facet_wrap(~Date) 


res %>% filter(Size < 300)
check_path <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN2_WORDCAPTURE/"
under200 <- "1385818_10-K_2013-11-26.RDS"
checkme_run2 <- readRDS(paste0(check_path, under200))
checkme_run3 <- readRDS(paste0(path, under200))

tail(checkme_run2, 100) %>% View()
