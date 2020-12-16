# martha_devika_join <- read.csv("data/dspg_ndc_dna_corpfam_to_sec_corpfam.csv")
# new_corp_fam <- martha_devika_join %>% select(original.NDC.company, new_corp_fam) 

## PRECISION
project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"
datalist_valid <- readRDS(paste0(project_folder, "working/SEC_Packaging/methodresults_valid_list_NOV182020v1.RDS"))

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



### RECALL 

datalist_valid_ndc_2 <- readRDS(paste0(project_folder, "working/SEC_Packaging/allndcresults_list_DEC132020.RDS"))

data_colnames <- colnames(datalist_valid_ndc_2[[1]]$data[[1]])[2:20]

launch_5_valid <- datalist_valid_ndc_2[[4]] %>% tidyr::unnest(data)  %>% tidyr::nest(orig_id = c(orig_id), data = data_colnames)
newproduct_5_valid <- datalist_valid_ndc_2[[6]]  %>% tidyr::unnest(data)  %>% tidyr::nest(orig_id = c(orig_id), data = data_colnames)
newdrug_5_valid <- datalist_valid_ndc_2[[8]] %>% tidyr::unnest(data)  %>% tidyr::nest(orig_id = c(orig_id), data = data_colnames)

launch_20_valid <- datalist_valid_ndc_2[[3]] %>% tidyr::unnest(data)  %>% tidyr::nest(orig_id = c(orig_id), data = data_colnames)
newproduct_20_valid <- datalist_valid_ndc_2[[5]]  %>% tidyr::unnest(data)  %>% tidyr::nest(orig_id = c(orig_id), data = data_colnames)
newdrug_20_valid <- datalist_valid_ndc_2[[7]] %>% tidyr::unnest(data)  %>% tidyr::nest(orig_id = c(orig_id), data = data_colnames)

## PROXIMITY 5 

proximity_5_COMBINED <- rbind(launch_5_valid, newproduct_5_valid, newdrug_5_valid) %>% distinct() 

proximity_5_COMBINED <- proximity_5_COMBINED %>% 
  tidyr::nest(valid = c("valid"), results = c("Word.y", "sec_capture_id.y", "TOT_SEC_NONPROPCAPTURES", "valid_p", "valid_n")) %>% 
  mutate(results_valid_summary = ifelse(unlist(lapply(valid, FUN = sum)) >0, 1, 0)  )

proximity_5_COMBINED_ <- proximity_5_COMBINED[!duplicated(proximity_5_COMBINED$orig_id),] 
proximity_5_COMBINED_

## PROXIMITY 20

proximity_20_COMBINED <- rbind(launch_20_valid, newproduct_20_valid, newdrug_20_valid) %>% distinct() 

proximity_20_COMBINED <- proximity_20_COMBINED %>% 
  tidyr::nest(valid = c("valid"), results = c("Word.y", "sec_capture_id.y", "TOT_SEC_NONPROPCAPTURES", "valid_p", "valid_n")) %>% 
  mutate(results_valid_summary = ifelse(unlist(lapply(valid, FUN = sum)) >0, 1, 0)  )

proximity_20_COMBINED_ <- proximity_20_COMBINED[!duplicated(proximity_20_COMBINED$orig_id),] 
proximity_20_COMBINED_


# COUNT VALIDATIONS
calc_ndc_recallvals <- function(df) {
  NDC_DRUG_CT_all = df %>% filter(results_valid_summary >0 & !is.na(family)) %>% nrow
  NDC_DRUG_PROP_all = NDC_DRUG_CT_all/nrow(df)
  NDC_DRUG_CT_compset = df %>% filter(results_valid_summary >0 & !is.na(family)) %>% nrow
  NDC_DRUG_PROP_compset = NDC_DRUG_CT_compset/nrow(df %>% filter(!is.na(family)))
  
  NDC_LISTING_CT_all = df %>% filter(results_valid_summary >0 & !is.na(family)) %>% .$total_listings %>% sum
  NDC_LISTING_PROP_all = NDC_LISTING_CT_all/sum(df$total_listings)
  NDC_LISTING_CT_compset = df %>% filter(results_valid_summary >0 & !is.na(family)) %>% .$total_listings %>% sum
  NDC_LISTING_PROP_compset = NDC_LISTING_CT_compset/(df %>% filter(!is.na(family)) %>% .$total_listings %>% sum) 
  
  data.frame(NDC_DRUG_CT_all, 
             NDC_DRUG_PROP_all, 
             NDC_DRUG_CT_compset, 
             NDC_DRUG_PROP_compset, 
             NDC_LISTING_CT_all, 
             NDC_LISTING_PROP_all, 
             NDC_LISTING_CT_compset, 
             NDC_LISTING_PROP_compset)
}

###
ndc_recalls_prox_5 <- calc_ndc_recallvals(proximity_5_COMBINED_)
ndc_recalls_prox_20 <- calc_ndc_recallvals(proximity_20_COMBINED_)

final_res <- read.csv("data/final_res/summary_methods_precision_recall_overall.csv")
prox_5_combined_res<- data.frame("Set" = "Combined - P5", "Proximity" = "Proximity - 5", "Method" = "Method 2B") %>% cbind(ndc_recalls_prox_5) 
prox_20_combined_res<- data.frame("Set" = "Combined - P20", "Proximity" = "Proximity - 20", "Method" = "Method 2A") %>% cbind(ndc_recalls_prox_20) 

final_res_RECALLONLY <- final_res %>% select(Set, Proximity, Method, c(12:19)) %>% rbind(prox_5_combined_res, prox_20_combined_res)

# write.csv(final_res_RECALLONLY, "data/final_res/summary_methods_recall_overall_15DEC2020.csv")
final_res_RECALLONLY <- read.csv("data/final_res/summary_methods_recall_overall_15DEC2020.csv")


calc_ndc_recallvals_percomp <- function(df) {
  df %>% 
    filter(!is.na(family)) %>% 
    count(family, valid = valid > 0) %>% 
    reshape2::dcast(family ~valid,fun.aggregate =  sum) %>%
    `colnames<-`(c("family", "Undetected", "Detected")) %>% 
    mutate(comp_recall = Detected/(Undetected + Detected)) 
}

calc_ndc_precvals_percomp <- function(df) {
  df %>% 
    left_join(ndc_sec_ref %>% transmute(Company = as.character(cik), family) %>% distinct(), by = c("Company" )) %>%
    filter(!is.na(family)) %>% 
    count(family, VALID) %>% 
    reshape2::dcast(family~VALID, sum, value.var = "n") %>%
    `colnames<-`(c("family", "Invalid", "Valid")) %>% 
    mutate(comp_prec = Valid/(Invalid + Valid)) 
}

comb_prox <- list(proximity_20_COMBINED_, proximity_5_COMBINED_)

ndc_comp_recalls_combprox <- lapply(, FUN = calc_ndc_recallvals_percomp)
ndc_comp_prec_combprox <- lapply(list(proximity_20_COMBINED_, proximity_5_COMBINED_), FUN = calc_ndc_precvals_percomp)


company_prec_recall <- final_res %>% select(1:3) %>% slice(2:9) %>% mutate(COMP_NDC_RECALL_MEAN = lapply(ndc_comp_recalls, function(x) {mean(x$comp_recall)}) %>% unlist, 
                                                                           COMP_NDC_RECALL_MEDIAN = lapply(ndc_comp_recalls, function(x) {median(x$comp_recall)}) %>% unlist,
                                                                           COMP_NDC_RECALL_MAX = lapply(ndc_comp_recalls, function(x) {max(x$comp_recall)}) %>% unlist,
                                                                           
                                                                           COMP_NDC_PREC_MEAN = lapply(ndc_comp_prec, function(x) {mean(x$comp_prec)}) %>% unlist, 
                                                                           COMP_NDC_PREC_MEDIAN = lapply(ndc_comp_prec, function(x) {median(x$comp_prec)}) %>% unlist,
                                                                           COMP_NDC_PREC_MAX = lapply(ndc_comp_prec, function(x) {max(x$comp_prec)}) %>% unlist)
