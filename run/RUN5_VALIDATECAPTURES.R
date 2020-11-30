library(dplyr)
library(testbinnR)

# PATH
project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"
# LOAD DATA
summary_captures <- readRDS(paste0(project_folder, "working/SEC_Packaging/SummaryCaptures_NOV172020.RDS"))
datalist <- readRDS(file = paste0(project_folder, "working/SEC_Packaging/allresults_list_NOV172020v2.RDS"))


# GET VALIDATIONS 
datalist_valid <- list()
datalist_valid <- lapply(datalist, FUN = validate_captures)
# saveRDS(datalist_valid, paste0(project_folder, "working/SEC_Packaging/methodresults_valid_list_NOV182020v1.RDS"))
datalist_valid <- readRDS(paste0(project_folder, "working/SEC_Packaging/methodresults_valid_list_NOV182020v1.RDS"))

# COUNT VALIDATIONS
valids <- vector()
valids_unq <- vector()
invalids_unq <- vector()
for (i in 1:length(datalist_valid)) {
  valids[i] <- sum(datalist_valid[[i]]$VALID)
  valids_unq[i] <- datalist_valid[[i]] %>% filter(VALID == TRUE) %>% count(low) %>% select(-n) %>% nrow()
  invalids_unq[i] <- datalist_valid[[i]] %>% filter(VALID == FALSE) %>% count(low) %>% select(-n) %>% nrow()
}

# ADD TO SUMMARY
valids <- c(NA, valids)
valids_unq <- c(NA, valids_unq)
invalids_unq <- c(NA, invalids_unq)
summary_captures <- summary_captures %>% mutate(ValidCaptures = valids, 
                            ValidUniqCapts = valids_unq, 
                            InvalidUniqCapts = invalids_unq) 

summary_captures %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/SummaryCaptures_NOV192020v3.RDS"))
