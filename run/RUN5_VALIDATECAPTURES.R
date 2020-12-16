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

# summary_captures %>% saveRDS(paste0(project_folder, "working/SEC_Packaging/SummaryCaptures_NOV192020v3.RDS"))

summary_captures_ <- readRDS(paste0(project_folder, "working/SEC_Packaging/SummaryCaptures_NOV192020v3.RDS"))

################
################
################

ndc_validateset <- readRDS("data/ndc_validateset2.RDS") %>% tidyr::unnest("orig_ID")

datalist <- readRDS(file = paste0(project_folder, "working/SEC_Packaging/allresults_list_NOV172020v2.RDS"))
datalist_valid_ndc <- list()
datalist_valid_ndc <- lapply(datalist[2:9], FUN = validate_ndc)
# saveRDS(datalist_valid_ndc, paste0(project_folder, "working/SEC_Packaging/allndcresults_list_DEC102020.RDS"))
datalist_valid_ndc <- readRDS(paste0(project_folder, "working/SEC_Packaging/allndcresults_list_DEC102020.RDS"))

test1 <- validate_ndc(datalist[[1]]) # ???

datalist_valid_ndc[[1]] %>% head(10) %>% tidyr::unnest("Index_PROP") %>% inner_join()


# COUNT VALIDATIONS
valids <- vector()
valids_unq <- vector()
invalids_unq <- vector()
for (i in 1:length(datalist_valid_ndc)) {
  valids[i] <- sum(datalist_valid_ndc[[i]]$VALID)
  valids_unq[i] <- datalist_valid_ndc[[i]] %>% filter(VALID == TRUE) %>% count(low) %>% select(-n) %>% nrow()
  invalids_unq[i] <- datalist_valid_ndc[[i]] %>% filter(VALID == FALSE) %>% count(low) %>% select(-n) %>% nrow()
}

# ADD TO SUMMARY
valids <- c(NA, valids)
valids_unq <- c(NA, valids_unq)
invalids_unq <- c(NA, invalids_unq)
summary_captures <- summary_captures_ %>% mutate(ValidNDC = valids, 
                                                ValidUniqNDC = valids_unq, 
                                                InvalidUniqNDC = invalids_unq) 



launch_5_valid <- datalist_valid[[4]]
newproduct_5_valid <- datalist_valid[[6]]
newdrug_5_valid <- datalist_valid[[8]]

launch_20_valid <- datalist_valid[[3]]
newproduct_20_valid <- datalist_valid[[5]]
newdrug_20_valid <- datalist_valid[[7]]



## PROXIMITY 5 

proximity_5_COMBINED <- rbind(launch_5_valid, newproduct_5_valid, newdrug_5_valid) %>% distinct() 
proximity_5_COMBINED <- proximity_5_COMBINED[!duplicated(proximity_5_COMBINED$id),] 

# COUNT VALIDATIONS
valids <- sum(proximity_5_COMBINED$VALID)
valids_unq <- proximity_5_COMBINED %>% filter(VALID == TRUE) %>% count(low) %>% select(-n) %>% nrow()
invalids_unq <- proximity_5_COMBINED %>% filter(VALID == FALSE) %>% count(low) %>% select(-n) %>% nrow()

prox_5_combined_summary_row <- c("Combined - P5", "Proximity - 5", "Method 2B", nrow(proximity_5_COMBINED), length(unique(proximity_5_COMBINED$UniqueCaptures)), valids, valids_unq, invalids_unq)

## PROXIMITY 20

proximity_20_COMBINED <- rbind(launch_20_valid, newproduct_20_valid, newdrug_20_valid) %>% distinct() 
proximity_20_COMBINED <- proximity_20_COMBINED[!duplicated(proximity_20_COMBINED$id),] 

# COUNT VALIDATIONS
valids <- sum(proximity_20_COMBINED$VALID)
valids_unq <- proximity_20_COMBINED %>% filter(VALID == TRUE) %>% count(low) %>% select(-n) %>% nrow()
invalids_unq <- proximity_20_COMBINED %>% filter(VALID == FALSE) %>% count(low) %>% select(-n) %>% nrow()

prox_20_combined_summary_row <- c("Combined - P20", "Proximity - 20", "Method 2A", nrow(proximity_20_COMBINED), length(unique(proximity_20_COMBINED$UniqueCaptures)), valids, valids_unq, invalids_unq)


summary_captures_ <- summary_captures_ %>% rbind(prox_5_combined_summary_row, prox_20_combined_summary_row)

summary_captures_ 



# ndc_validateset
# 
# test <- ndc_validateset %>% 
#   # mutate(id = as.numeric(id)) %>% 
#   left_join(prop_low_unq %>% tidyr::unnest("id"), by =  "id")
# 
# datalist_valid_ndc[[8]] %>% tidyr::unnest("Index_PROP")
# 
#   left_join(, by = c("pos_id" = "Index_PROP"))
#   
# 
# View(head(test, 100))
# 
# datalist_valid_ndc[[8]] %>% filter(stringr::str_detect(Word, "diluent")) %>% tidyr::unnest("Index_PROP") %>% filter(Index_PROP == 5)
# 
# # datalist_valid_ndc <- lapply(datalist, FUN = )
# # saveRDS(datalist_valid, paste0(project_folder, "working/SEC_Packaging/methodresults_valid_list_NOV182020v1.RDS"))
# # datalist_valid <- readRDS(paste0(project_folder, "working/SEC_Packaging/methodresults_valid_list_NOV182020v1.RDS"))
# 
# prop_low_ids <- ndc_validateset %>% select(orig_ID, id, Prop_low) %>% mutate(total_listings = unlist(lapply(orig_ID, FUN = nrow))) %>% transmute(id = as.numeric(id), Prop_low, total_listings, orig_ID) %>% tidyr::unnest("orig_ID") %>% mutate(orig_id = as.numeric(orig_id))
# 
# thing <- datalist[[9]] %>% head %>% validate_ndc()
# ## Better to match to ndc_validateset (which has column for total)
# symbicort_PROPTEST <- validate_ndc(data.frame("Word" = c("symbicort"))) %>% 
#   tidyr::unnest(c("Index_PROP")) %>% 
#   left_join(prop_low_unq %>% tidyr::unnest(c("id")) %>% mutate(id = as.numeric(id)), by = c("Index_PROP" = "pos_id")) %>% 
#   left_join(ndc_validateset %>% mutate(id = as.numeric(id)), by = c( "id"))
# 
# acetominophen_NONPROPTEST <- validate_ndc(data.frame("Word" = c("acetominophen"))) %>% 
#   tidyr::unnest(c("Index_NONPROP")) %>% 
#   left_join(nonprop_low_unq %>% tidyr::unnest(c("id")) %>% mutate(id = as.numeric(id)), by = c("Index_NONPROP" = "pos_id")) %>% 
#   left_join(ndc_validateset %>% mutate(id = as.numeric(id)), by = c("id"))
# 
# 
# #  inner_join(ndc_validateset %>% mutate(id = as.integer(id)) %>% select(id, family, LABELERNAME, PROPRIETARYNAME), by = c("Index_PROP" = "id"))
# datalist_valid_ndc[[8]] %>% count(VALID)
