library(dplyr)
library(testbinnR)

get_unqs <- function(df) {
  df %>% select(low) %>% distinct() %>% nrow()
}


project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"

# allfilings_allwords_analyze <- readRDS("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV162020v3.RDS")
# allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "allnoneng", "_16NOV2020.RDS"))
# brand <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "brand", "_16NOV2020.RDS"))
# launch20_noneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "launch20_noneng", "_16NOV2020.RDS"))
# launch5_noneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "launch5_noneng", "_16NOV2020.RDS"))
# newproduct_20_noneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "newproduct20_noneng", "_16NOV2020.RDS"))
# newproduct_5_allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "newproduct5_noneng", "_16NOV2020.RDS"))
# newdrug_20_allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "newdrug20_noneng", "_16NOV2020.RDS"))
# newdrug_5_allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "newdrug_5_noneng", "_16NOV2020.RDS"))

##############
allfilings_allwords_analyze <- readRDS("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/allfilings_allwordsanalyze_NOV172020v4.RDS")
allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "allnoneng", "_17NOV2020.RDS"))
brand <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "brand", "_17NOV2020.RDS"))
launch20_noneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "launch20_noneng", "_17NOV2020.RDS"))
launch5_noneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "launch5_noneng", "_17NOV2020.RDS"))
newproduct_20_noneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "newproduct20_noneng", "_17NOV2020.RDS"))
newproduct_5_allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "newproduct5_noneng", "_17NOV2020.RDS"))
newdrug_20_allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "newdrug20_noneng", "_17NOV2020.RDS"))
newdrug_5_allnoneng <- readRDS(paste0(project_folder, "working/SEC_Packaging/", "newdrug_5_noneng", "_17NOV2020.RDS"))

##############
##############

datasets <- c("All words", "Non-English", "Brand", "Launch - 20", "Launch - 5", 
              "New Product - 20", "New Product - 5", "New Drug - 20", "New Drug - 5"
              )
datalist <- list(allfilings_allwords_analyze, allnoneng, brand, launch20_noneng, launch5_noneng,
                 newproduct_20_noneng, newproduct_5_allnoneng, newdrug_20_allnoneng, newdrug_5_allnoneng)

# saveRDS(datalist, file = paste0(project_folder, "working/SEC_Packaging/allresults_list_NOV172020v2.RDS"))
datalist <- readRDS(file = paste0(project_folder, "working/SEC_Packaging/allresults_list_NOV172020v2.RDS"))

# rm(allfilings_allwords_analyze, allnoneng, brand, launch20_noneng, launch5_noneng,
   # newproduct_20_noneng, newproduct_5_allnoneng, newdrug_20_allnoneng, newdrug_5_allnoneng)

##############
##############
##############

datasets <- c("All words", "Non-English", "Brand", "Launch - 20", "Launch - 5", 
              "New Product - 20", "New Product - 5", "New Drug - 20", "New Drug - 5")

total_captures <- lapply(datalist, FUN = nrow)
unq_captures <- lapply(datalist, FUN = get_unqs)

SummaryCaptures <- data.frame("Set" = datasets) %>% 
  mutate(Proximity = recode_factor(Set, 
                                   `All words` = "None", 
                                   `Non-English` = "Method 1", 
                                   `Brand` = "Method 3",
                                   `Launch - 20` = "Proximity - 20",
                                   `New Product - 20` = "Proximity - 20",
                                   `New Drug - 20` = "Proximity - 20",
                                   `Launch - 5` = "Proximity - 5",
                                   `New Product - 5` = "Proximity - 5",
                                   `New Drug - 5` = "Proximity - 5"
                                   ), 
         Method = recode_factor(Proximity, 
                                `Proximity - 20` = "Method 2A",
                                `Proximity - 5` = "Method 2B"
                                ), 
         "TotalCaptures" = unlist(total_captures), "UniqueCaptures" = unlist(unq_captures)
         )

# SummaryCaptures %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/SummaryCaptures_NOV172020.RDS"))
summary_captures <- readRDS(paste0(project_folder, "working/SEC_Packaging/SummaryCaptures_NOV172020.RDS"))








