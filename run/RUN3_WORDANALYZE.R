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

table(as.Date(allfiles$run3_dates))

# allfiles_ <- allfiles %>% filter(run3_dates < "2020-11-09")

# allfiles_ <- allfiles_[rev(rownames(allfiles_)),]

# allfiles_ <- allfiles_ %>% slice(200:250)

# captures_path <- list.files(paste0(project_folder, captures_folder))

# #
# allwords_run2 <- readRDS(paste0(project_folder, captures_folder, captures_path[1]))
# allwords_run2 %>% filter(stringr::str_detect(Captures, "(®|™)$"))
# #
# captures_path <- list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/BRAND/"))
# allwords_run3 <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/BRAND/", captures_path[1]))
# allwords_run3 %>% filter(stringr::str_detect(Captures, "(®|™)$"))

# captures_paths <- paste0(project_folder, captures_folder, captures_path)
# length(list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/")))

# captures_path <- rev(captures_path)
# captures_paths <- rev(captures_paths)
# 
# #work on files not modified on sept 29 
# files_in_allwords <- list.files(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/"))
# paths_in_allwords <- paste0(project_folder ,"working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/" ,files_in_allwords )
# modtimes <- file.mtime(paths_in_allwords)
# times_Df <- data.frame(dates = as.Date(modtimes), files = files_in_allwords, paths = paths_in_allwords)
# times_Df[,1:2]
# table(lubridate::date(times_Df$dates))
# captures_paths <- times_Df %>% filter(dates < "2020-11-09" ) %>% .$paths
# captures_path <- times_Df %>% filter(dates < "2020-11-09") %>% .$files
# # 1178711 -- 1
# # 1427925 -- 149

for (i in 1:nrow(allfiles_)) {
  print(i)
  
  allwords <- readRDS(allfiles_$run2_paths[i])
  
  allwords <-  allwords %>% 
    mutate(Captures_ = ifelse(grepl(x = Captures, pattern =  "(®|™)$"), 
                              gsub(x = Captures, pattern = "(®|™)$", replacement = ""), 
                              Captures), 
           low = tolower(Captures_), 
           eng_C = hunspell::hunspell_check(Captures_), 
           eng_l = hunspell::hunspell_check(low)
           )
  print(Sys.time())
  allwords <- cbind(allwords, allwords$Captures %>% testbinnR::refine_captures("TF"))
  print(Sys.time())
  saveRDS(allwords, paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/", allfiles_$run2_files[i]))
  
  print(i/nrow(allfiles_))
  print(Sys.time())
  
}

checkme <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/", allfiles_$run2_files[i]))
checkme %>% colnames


datalist = list()

for (i in 1:nrow(allfiles)) {
  dat <- readRDS(allfiles$run3_paths[i])
  dat$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat # add it to your list
}

columns <- lapply(X = datalist, FUN = ncol) %>%  unlist()
allfiles <- allfiles %>% mutate(colnum = columns)

# allfiles_ <- allfiles %>% filter(colnum > 17)  # if there are some with more than 17 columns 

checkme <- readRDS(paste0(project_folder, "working/SEC_Packaging/RUN3_SUBSETMETHODS/ALL_WORDS_ANALYZE/", allfiles_$run2_files[1]))

allfilings_allwords_analyze = do.call(rbind, datalist)

# saveRDS(allfilings_allwords_analyze, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV132020.RDS")
allfilings_allwords_analyze <- readRDS("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV132020.RDS")

######################
######################
######################
######################

  
allnoneng_by_freq  <- allfilings_allwords_analyze %>% 
    filter(eng_C == FALSE & xml == FALSE & fin == FALSE & dates == FALSE & pharma == FALSE & comp_name == FALSE & tick == FALSE) %>% 
    count(low) %>% 
    arrange(desc(n))

write.csv(allnoneng_by_freq, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allnoneng_cleanupcsv_NOV132020.csv")

allfilings_allwords_analyze <- allfilings_allwords_analyze %>% 
  mutate(digits = grepl(low, pattern = "\\d")) %>%
  mutate(nonalpha = grepl(low, pattern = "[^a-zA-Z]")) 

# saveRDS(allfilings_allwords_analyze, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV132020v2.RDS")
allfilings_allwords_analyze <- readRDS("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV132020v2.RDS")

######################
######################
######################
######################

words_annotated <- read.csv("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allnoneng_cleanupcsv_NOV132020_annotated.csv")
table(words_annotated$Status, useNA = "always")

remove_words <- words_annotated %>% 
  filter(Status %in% c("xml", "pharma", "fin", "company")) %>%
  select(-n, -Question)



allfilings_allwords_analyze <- allfilings_allwords_analyze %>% left_join(remove_words, by = "low")
#  73,993,508 - no change in rows
# table(allfilings_allwords_analyze$Status)

# saveRDS(allfilings_allwords_analyze, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV162020v3.RDS")
allfilings_allwords_analyze <- readRDS("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV162020v3.RDS")


