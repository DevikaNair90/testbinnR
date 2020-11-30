library(dplyr)
library(testbinnR)
project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"
captures_folder <- "working/SEC_Packaging/RUN2_WORDCAPTURE/"

run2_files <- list.files(paste0(project_folder, captures_folder))
run2_paths <- paste0(project_folder, captures_folder, run2_files)

run3_files <- list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/"))
run3_paths <- paste0(project_folder ,"working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/", run3_files )
run3_dates <- file.mtime(run3_paths)

allfiles <- left_join(data.frame(run2_files, run2_paths), data.frame(run3_files, run3_paths, run3_dates), by = c("run2_files" = "run3_files")) 

allfiles <- allfiles %>%  mutate(Company = stringr::str_extract(run2_files, "\\d*(?=_)"),
                                   Year = stringr::str_remove_all(stringr::str_extract(run2_files, "_\\d{4}-"), "[^\\d]"),
                                 Month = stringr::str_remove_all(stringr::str_extract(run2_files, "-\\d\\d-"), "[^\\d]"))
##

allfilings_allwords_analyze <- readRDS("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV162020v3.RDS")

allfilings_allwords_analyze <-  allfilings_allwords_analyze %>% 
  mutate(Captures_ = ifelse(grepl(x = Captures, pattern =  "(®|™)$"), 
                            gsub(x = Captures, pattern = "(®|™)$", replacement = ""), 
                            Captures), 
         low = tolower(Captures_), 
         eng_C = hunspell::hunspell_check(Captures_), 
         eng_l = hunspell::hunspell_check(low)
  )

# allfilings_allwords_analyze %>% saveRDS("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV172020v4.RDS")
allfilings_allwords_analyze <- readRDS("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV172020v4.RDS")

allfilings_allwords_analyze %>% filter(grepl(low, pattern = "[^a-zA-Z]"))

filing_wc <- allfilings_allwords_analyze %>% count(Company, Year, Month)

##

allfiles <- allfiles %>% left_join(filing_wc, by = c("Company", "Year", "Month")) 

##############
##############
##############


allnoneng <- allfilings_allwords_analyze %>% 
  mutate(nonalpha_v2 = ifelse(is.na(brand), nonalpha, FALSE)) %>%
  filter(eng_C == FALSE & 
           xml == FALSE & 
           fin == FALSE & 
           dates == FALSE & 
           pharma == FALSE & 
           comp_name == FALSE & 
           tick == FALSE & 
           digits == FALSE &
           nonalpha_v2 == FALSE &
           is.na(Status)
  ) 

# allnoneng %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "allnoneng", "_17NOV2020.RDS"))
allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "allnoneng", "_17NOV2020.RDS"))

######################
######################
######################
######################

brand <- allfilings_allwords_analyze %>% 
  mutate(nonalpha_v2 = ifelse(is.na(brand), nonalpha, FALSE)) %>%
  filter(!is.na(brand)
  ) 

# brand %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "brand", "_17NOV2020.RDS"))
brand <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "brand", "_17NOV2020.RDS"))

######################
######################
######################
######################

launch20 <- allfilings_allwords_analyze %>% 
  get_proximity_words(mode = "string", search = "launch", n = 20)

# launch20 %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "launch20", "_17NOV2020.RDS"))

launch5 <- allfilings_allwords_analyze %>% 
  get_proximity_words(mode = "string", search = "launch", n = 5)

# launch5 %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "launch5", "_17NOV2020.RDS"))

launch20_noneng <- launch20 %>%
  mutate(nonalpha_v2 = ifelse(is.na(brand), nonalpha, FALSE)) %>%
  filter(eng_C == FALSE & 
           xml == FALSE & 
           fin == FALSE & 
           dates == FALSE & 
           pharma == FALSE & 
           comp_name == FALSE & 
           tick == FALSE & 
           digits == FALSE &
           nonalpha_v2 == FALSE &
           is.na(Status)
  ) 

# launch20_noneng %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "launch20_noneng", "_17NOV2020.RDS"))


launch5_noneng <- launch5 %>%
  mutate(nonalpha_v2 = ifelse(is.na(brand), nonalpha, FALSE)) %>%
  filter(eng_C == FALSE & 
           xml == FALSE & 
           fin == FALSE & 
           dates == FALSE & 
           pharma == FALSE & 
           comp_name == FALSE & 
           tick == FALSE & 
           digits == FALSE &
           nonalpha_v2 == FALSE &
           is.na(Status)
  ) 

# launch5_noneng %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "launch5_noneng", "_17NOV2020.RDS"))

######################
######################
######################
######################

new_loc <- allfilings_allwords_analyze %>% get_proximity_words(mode = "string", search = "\\bnew(er|est)?\\b", n = 1) 

######################
######################

new_product_loc <- new_loc %>%
  filter(pstn != "before") %>%
  mutate(orig_id = id) %>%
  get_proximity_words(mode = "string", search = "product", n = 1) 

new_product_loc <- new_product_loc %>% filter(pstn %in% c("before", "target"))

newproduct_20 <- allfilings_allwords_analyze %>%
  get_proximity_words(mode = "id", search = new_product_loc$orig_id, n = 20)

# newproduct_20 %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "newproduct20", "_17NOV2020.RDS"))

newproduct_20_noneng <- newproduct_20 %>%
  mutate(nonalpha_v2 = ifelse(is.na(brand), nonalpha, FALSE)) %>%
  filter(eng_C == FALSE & 
           xml == FALSE & 
           fin == FALSE & 
           dates == FALSE & 
           pharma == FALSE & 
           comp_name == FALSE & 
           tick == FALSE & 
           digits == FALSE &
           nonalpha_v2 == FALSE &
           is.na(Status)
  ) 

# newproduct_20_noneng %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "newproduct20_noneng", "_17NOV2020.RDS"))

newproduct_5 <- allfilings_allwords_analyze %>%
  get_proximity_words(mode = "id", search = new_product_loc$orig_id, n = 5)

# newproduct_5 %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "newproduct5", "_17NOV2020.RDS"))

newproduct_5_allnoneng <- newproduct_5 %>%
  mutate(nonalpha_v2 = ifelse(is.na(brand), nonalpha, FALSE)) %>%
  filter(eng_C == FALSE & 
           xml == FALSE & 
           fin == FALSE & 
           dates == FALSE & 
           pharma == FALSE & 
           comp_name == FALSE & 
           tick == FALSE & 
           digits == FALSE &
           nonalpha_v2 == FALSE &
           is.na(Status)
  ) 

# newproduct_5_allnoneng %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "newproduct5_noneng", "_17NOV2020.RDS"))

######################
######################

new_drug_loc <- new_loc %>%
  filter(pstn != "before") %>%
  mutate(orig_id = id) %>%
  get_proximity_words(mode = "string", search = "drug", n = 1) 

new_drug_loc <- new_drug_loc %>% filter(pstn %in% c("before", "target"))

newdrug_20 <- allfilings_allwords_analyze %>%
  get_proximity_words(mode = "id", search = new_drug_loc$orig_id, n = 20)

# newdrug_20 %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "newdrug20", "_176NOV2020.RDS"))

newdrug_20_allnoneng <- newdrug_20 %>%
  mutate(nonalpha_v2 = ifelse(is.na(brand), nonalpha, FALSE)) %>%
  filter(eng_C == FALSE & 
           xml == FALSE & 
           fin == FALSE & 
           dates == FALSE & 
           pharma == FALSE & 
           comp_name == FALSE & 
           tick == FALSE & 
           digits == FALSE &
           nonalpha_v2 == FALSE &
           is.na(Status)
  ) 

# newdrug_20_allnoneng %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "newdrug20_noneng", "_17NOV2020.RDS"))

newdrug_5 <- allfilings_allwords_analyze %>%
  get_proximity_words(mode = "id", search = new_drug_loc$orig_id, n = 5)

newdrug_5_allnoneng <- newdrug_5 %>%
  mutate(nonalpha_v2 = ifelse(is.na(brand), nonalpha, FALSE)) %>%
  filter(eng_C == FALSE & 
           xml == FALSE & 
           fin == FALSE & 
           dates == FALSE & 
           pharma == FALSE & 
           comp_name == FALSE & 
           tick == FALSE & 
           digits == FALSE &
           nonalpha_v2 == FALSE &
           is.na(Status) ) 

# newdrug_5_allnoneng %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/", "newdrug_5_noneng", "_17NOV2020.RDS"))


######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################

# OLD CODE

######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################
######################



# captures_paths2 <- paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/", list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/")))
# 
# modtimes <- file.mtime(captures_paths2)
# times_Df <- data.frame(dates = as.Date(modtimes), files = list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/")), paths = captures_paths2)
# times_Df[,1:2]
# table(lubridate::date(times_Df$dates))
# captures_paths_2 <- times_Df  %>% .$paths #%>% filter(dates < "2020-09-29" )
# captures_path_2 <- times_Df  %>% .$files #%>% filter(dates < "2020-09-29" )
# 
# # for (i in 1:length(captures_paths2)) {
# #   allwords <- readRDS(captures_paths2[i])
# #   brand <- allwords %>% filter(!is.na(brand)) %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/BRAND/", captures_path[i]))
# # }
# 
# for (i in 1:length(captures_paths_2)) {
#   print(i)
#   
#   allwords <- readRDS(captures_paths2[i])
#   
#   allnoneng <- allwords %>% filter(eng_C == FALSE) %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_NON_ENGLISH/", captures_path_2[i]))
#   brand <- allwords %>% filter(!is.na(brand)) %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/BRAND/", captures_path_2[i]))
#   launch <- allwords %>% get_proximity_words(mode = "string", search = "launch", n = 20)  %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_LAUNCH/", captures_path_2[i]))
#   
#   new_loc <- allwords %>% get_proximity_words(mode = "string", search = "\\bnew(er|est)?\\b", n = 1) 
#   
#   if(is.data.frame(new_loc) == TRUE) {
#     
#     ## NEW PRODUCT
#     new_product_loc <- new_loc %>%
#       filter(pstn != "before") %>%
#       mutate(orig_id = id) %>%
#       get_proximity_words(mode = "string", search = "product", n = 1) 
#     
#     if(is.data.frame(new_product_loc) == TRUE) {
#       new_product_loc <- new_product_loc %>% filter(pstn %in% c("before", "target"))
#       
#       new_product_words <- allwords %>%
#         get_proximity_words(mode = "id", search = new_product_loc$orig_id, n = 20)
#       
#       # saveRDS(new_product_words, paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWPRODUCT/", captures_path[i]))
#       saveRDS(new_product_words, paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWPRODUCT_RELAXED/", captures_path_2[i]))
#     }
#     
#     ## NEW DRUG
#     new_drug_loc <- new_loc %>%
#       filter(pstn != "before") %>%
#       mutate(orig_id = id) %>%
#       get_proximity_words(mode = "string", search = "drug", n = 1) 
#     
#     if(is.data.frame(new_drug_loc) == TRUE) {
#       new_drug_loc <- new_drug_loc %>% filter(pstn %in% c("before", "target"))
#       
#       new_drug_words <- allwords %>%
#         get_proximity_words(mode = "id", search = new_drug_loc$orig_id, n = 20)
#       
#       # saveRDS(new_drug_words, paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWDRUG/", captures_path[i]))
#       saveRDS(new_drug_words, paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWDRUG_RELAXED/", captures_path_2[i]))
#       
#     }
#     
#   }
#   
#   print(i/length(captures_paths_2))
# }
# 
# 
# ##
# b <- 616
# checkme2 <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/", captures_path[b]))
# checkme2_allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_NON_ENGLISH/", captures_path[b]))
# checkme2_brand <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/BRAND/", captures_path[b]))
# checkme2_launch <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_LAUNCH/", captures_path[b]))
# checkme2_newprod <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWPRODUCT_RELAXED/", captures_path[b]))
# checkme2_newdrug <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWDRUG_RELAXED/", captures_path[b]))
# 
# 
# 
# fold_res <- c(length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/"))),
#               length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_NON_ENGLISH/"))),
#               length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/BRAND/"))),
#               length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_LAUNCH/"))),
#               length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWPRODUCT/"))),
#               length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWDRUG/"))), 
#               length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWPRODUCT_RELAXED/"))),
#               length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/PROXIMITY_NEWDRUG_RELAXED/"))))
# 
# results <- data.frame(RES = c("ALL WORDS ANALYZE", "ALL NON ENGLISH", "BRAND", "LAUNCH", "NEW PRODUCT 1", "NEW DRUG 1", "NEW PRODUCT 2", "NEW DRUG 2"),
#                       size = fold_res)
# 
# path <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/"
# find <- list.files(path)
# size <- file.size(paste0(path, find))
# datetime <- file.mtime(paste0(path, find))
# res <- data.frame("File" = find,
#                   "Size" = size/1024,
#                   "Date" = as.Date(datetime), 
#                   "Time" = as.factor(lubridate::hour(datetime)))
# 
# 
# library(ggplot2)
# ggplot(data = res, aes(x = Size)) + 
#   geom_histogram() #+ facet_wrap(~Date) 
# 
# 
# res %>% filter(Size < 300)
# check_path <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN2_WORDCAPTURE/"
# under200 <- "1385818_10-K_2013-11-26.RDS"
# checkme_run2 <- readRDS(paste0(check_path, under200))
# checkme_run3 <- readRDS(paste0(path, under200))
# 
# tail(checkme_run2, 100) %>% View()
# 
# 


