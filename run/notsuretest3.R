library(dplyr)
library(stringr)

datapath1 <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/" 
datapath <- "/project/biocomplexity/sdad/projects_data/volume_nyc1_01/business_innovation/" 

## ORIGINAL NDC DATA
ndc_product <-  readr::read_tsv(paste0(datapath1, "original/NDC/product.txt")) %>% mutate(year = str_extract(string = STARTMARKETINGDATE, pattern = "^\\d{4}"))

## NDC - SEC COMPANY MAPPING - 279 matches
ndc_sec_ref <- readxl::read_excel(paste0(datapath1, "working/NDC_SEC_CompNames/final_ndc_sec_companies.xlsx"), sheet = "finalset") %>% select(-12,-13, -14)
colnames(ndc_sec_ref) <- dataplumbr::name.standard_col_names(colnames(ndc_sec_ref))
ndc_sec_ref <- ndc_sec_ref %>% filter(is.na(dupe))

#####
marthas_corpfam <- readxl::read_excel("data/ndc_dna_matching.xlsx")
ndc_sec_ref 

martha_devika_join <- marthas_corpfam %>% full_join(ndc_sec_ref %>% select(family, ndc_labeler) %>% distinct(), by = c("original NDC company" = "ndc_labeler"))
martha_devika_join <- martha_devika_join %>% mutate(new_corp_fam = ifelse(is.na(`corporate family`), family, `corporate family`)) 

write.csv(martha_devika_join, "data/dspg_ndc_dna_corpfam_to_sec_corpfam.csv")


####

## NDC Data - give an id and a corp family 
ndc_product_ <- ndc_product %>% 
  mutate(orig_id = rownames(.)) %>% 
  # left_join(martha_devika_join %>% select(new_corp_fam, `original NDC company`) %>% distinct, by = c("LABELERNAME" = "original NDC company")) %>%
  left_join(ndc_sec_ref %>% select(ndc_labeler, family) %>% distinct(), by = c("LABELERNAME" = "ndc_labeler")) %>%
  select(orig_id, family, colnames(ndc_product)) 


ndc_product_ %>% head(2) %>% as.data.frame() %>% t

ndc_drugs_nestedlistings <- ndc_product_ %>% 
  mutate(Prop_low = stringr::str_to_lower(PROPRIETARYNAME), Nonrop_low = stringr::str_to_lower(NONPROPRIETARYNAME)) %>%
  tidyr::nest(data = c("orig_id", "PRODUCTID", "PRODUCTNDC", "PRODUCTTYPENAME", 
                       "PROPRIETARYNAMESUFFIX", "DOSAGEFORMNAME", "ROUTENAME", "STARTMARKETINGDATE", 
                       "ENDMARKETINGDATE", "MARKETINGCATEGORYNAME", "APPLICATIONNUMBER", "LABELERNAME", 
                       "SUBSTANCENAME", "ACTIVE_NUMERATOR_STRENGTH", "ACTIVE_INGRED_UNIT", "PHARM_CLASSES", 
                       "DEASCHEDULE", "NDC_EXCLUDE_FLAG", "LISTING_RECORD_CERTIFIED_THROUGH", "year")) %>% 
  mutate(total_listings = unlist(lapply(data, FUN = nrow)), 
         drug_id = as.numeric(rownames(.)) )

## Martha's version

ndc_drugs_nestedlistings <- ndc_product_ %>% 
  mutate(Prop_low = stringr::str_to_lower(PROPRIETARYNAME), Nonrop_low = stringr::str_to_lower(NONPROPRIETARYNAME)) %>%
  tidyr::nest(data = c("orig_id", "PRODUCTID", "PRODUCTNDC", "PRODUCTTYPENAME", 
                       "PROPRIETARYNAMESUFFIX", "DOSAGEFORMNAME", "ROUTENAME", "STARTMARKETINGDATE", 
                       "ENDMARKETINGDATE", "MARKETINGCATEGORYNAME", "APPLICATIONNUMBER", "LABELERNAME", 
                       "SUBSTANCENAME", "ACTIVE_NUMERATOR_STRENGTH", "ACTIVE_INGRED_UNIT", "PHARM_CLASSES", 
                       "DEASCHEDULE", "NDC_EXCLUDE_FLAG", "LISTING_RECORD_CERTIFIED_THROUGH")) %>% 
  mutate(total_listings = unlist(lapply(data, FUN = nrow)), 
         drug_id = as.numeric(rownames(.)) )

ndc_drugs_nestedlistings_MC <- ndc_drugs_nestedlistings %>% 
  group_by(new_corp_fam, year) %>% 
  summarise(total_listings = sum(total_listings), total_drugs = n()) 
write.csv(ndc_drugs_nestedlistings_MC, "data/ndc_drugs_nestedlistings_MC.csv")

# saveRDS(ndc_drugs_nestedlistings, "data/ndc_drugs_nestedlistings.RDS")
ndc_drugs_nestedlistings <- readRDS("data/ndc_drugs_nestedlistings.RDS")

prop_matchset_drugid <- ndc_drugs_nestedlistings %>% 
  select(Prop_low, drug_id) %>% 
  tidyr::nest(drug_id = c("drug_id")) %>%
  mutate(PROP_ID = as.numeric(row.names(.)))

# saveRDS(prop_matchset_drugid, "data/ndc_valid_calculation/prop_matchset_drugid.RDS")

nonprop_matchset_drugid <- ndc_drugs_nestedlistings %>% 
  select(Nonrop_low, drug_id) %>% 
  tidyr::nest(drug_id = c("drug_id")) %>%
  mutate(NONPROP_ID = as.numeric(row.names(.)))

# saveRDS(nonprop_matchset_drugid, "data/ndc_valid_calculation/nonprop_matchset_drugid.RDS")

df <- datalist_valid_ndc[[8]]

validate_ndc <- function(df) {
  # test_words <- unique(df[,"Word"])
  test_word_df <- df %>% mutate(sec_capture_id = as.numeric(rownames(.))) %>% select(Word, sec_capture_id) %>% tidyr::nest(sec_capture_id = sec_capture_id)
  
  # READ ndc_drugs_nestedlistings IN!!
  ndc_drugs_nestedlistings <- readRDS("data/ndc_drugs_nestedlistings.RDS")
  
  # READ prop_matchset_drugid and nonprop IN!!
  prop_matchset_drugid <- readRDS("data/ndc_valid_calculation/prop_matchset_drugid.RDS")
  nonprop_matchset_drugid <- readRDS("data/ndc_valid_calculation/nonprop_matchset_drugid.RDS")
  
  # PROP
  pos_PROP <- list()
  for (i in 1:length(test_word_df$Word)) {
    pos_PROP[[i]] <- grep(x = prop_matchset_drugid$Prop_low, pattern =  paste0("\\b", test_word_df$Word[i], "\\b"))
  } # pos_prop is the length of test_word_df
  
  prop_results =  cbind(test_word_df, Index_PROP = I(pos_PROP)) %>% mutate(TOT_PROP_MATCHES = lengths(Index_PROP)) 
    # no_prop_matches <- tot_prop_results %>% filter(TOT_PROP_MATCHES == 0) # do i need this? these are SEC captures without an NDC - to calculate NDC, don't need these? 
    some_prop_matches <- prop_results %>% 
      filter(TOT_PROP_MATCHES > 0) %>% 
      tidyr::unnest(Index_PROP) %>% 
      left_join(prop_matchset_drugid, by = c("Index_PROP" = "PROP_ID")) %>% 
      mutate(TOT_DRUG_MATCHES = unlist(lapply(drug_id, FUN = nrow))) %>% 
      tidyr::unnest("drug_id")
    
    res_pt1 <- ndc_drugs_nestedlistings %>% 
      left_join(some_prop_matches %>% select(-Index_PROP, -Prop_low), by = c("drug_id")) %>% 
      select(-TOT_PROP_MATCHES, -TOT_DRUG_MATCHES) %>% 
      mutate(TOT_SEC_PROPCAPTURES = ifelse(lengths(sec_capture_id) == 0, 0, unlist(lapply(sec_capture_id, FUN = nrow)))) 
    
    res_pt1
    
    # NON PROP
    pos_NONPROP <- list()
    for (i in 1:length(test_word_df$Word)) {
      pos_NONPROP[[i]] <- grep(x = nonprop_matchset_drugid$Nonrop_low, pattern =  paste0("\\b", test_word_df$Word[i], "\\b"))
    } # pos_prop is the length of test_word_df
    
    nonprop_results =  cbind(test_word_df, Index_NONPROP = I(pos_NONPROP)) %>% mutate(TOT_NONPROP_MATCHES = lengths(Index_NONPROP)) 
    
    some_nonprop_matches <- nonprop_results %>% 
      filter(TOT_NONPROP_MATCHES > 0) %>% 
      tidyr::unnest(Index_NONPROP) %>% 
      left_join(nonprop_matchset_drugid, by = c("Index_NONPROP" = "NONPROP_ID")) %>% 
      mutate(TOT_DRUG_MATCHES = unlist(lapply(drug_id, FUN = nrow))) %>% 
      tidyr::unnest("drug_id")
    
    res_pt2 <- res_pt1 %>% 
      left_join(some_nonprop_matches %>% select(-Index_NONPROP, -Nonrop_low), by = c("drug_id")) %>% 
      select(-TOT_NONPROP_MATCHES, -TOT_DRUG_MATCHES) %>% 
      mutate(TOT_SEC_NONPROPCAPTURES = ifelse(lengths(sec_capture_id.y) == 0, 0, unlist(lapply(sec_capture_id.y, FUN = nrow)))) 
    
    res_pt2 <- res_pt2 %>% mutate(valid_p = TOT_SEC_PROPCAPTURES > 0, valid_n =  TOT_SEC_NONPROPCAPTURES > 0, valid = valid_p + valid_n)
}

## EXPLAIN LISTINGS -> DRUGS 
listings_vs_drugs <- data.frame("Unit" = c("Listings", "Drugs"), "Size" = c(nrow(ndc_product_), nrow(ndc_drugs_nestedlistings)), 
                                "Company_Set_Size" = c(ndc_product_ %>% filter(!is.na(family)) %>% nrow, ndc_drugs_nestedlistings %>% filter(!is.na(family)) %>% nrow )) %>%
  mutate(Company_Prop = round(Company_Set_Size/Size, 3))


## TEST 
project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"
datalist_valid_ndc <- readRDS(paste0(project_folder, "working/SEC_Packaging/allndcresults_list_DEC102020.RDS"))

test <- validate_ndc(datalist_valid_ndc[[7]])

# GET VALIDATIONS 
datalist_valid_ndc_2 <- list()
datalist_valid_ndc_2 <- lapply(datalist_valid_ndc, FUN = validate_ndc)

datalist_valid_ndc_2[[3]]
# saveRDS(datalist_valid_ndc_2, paste0(project_folder, "working/SEC_Packaging/allndcresults_list_DEC132020.RDS"))
datalist_valid_ndc_2 <- readRDS(paste0(project_folder, "working/SEC_Packaging/allndcresults_list_DEC132020.RDS"))


## Calc recall??

summary_captures_ <- readRDS(paste0(project_folder, "working/SEC_Packaging/SummaryCaptures_NOV192020v3.RDS"))

calc_ndc_recallvals <- function(df) {
  NDC_DRUG_CT_all = df %>% filter(valid >0 & !is.na(family)) %>% nrow
  NDC_DRUG_PROP_all = NDC_DRUG_CT_all/nrow(df)
  NDC_DRUG_CT_compset = df %>% filter(valid >0 & !is.na(family)) %>% nrow
  NDC_DRUG_PROP_compset = NDC_DRUG_CT_compset/nrow(df %>% filter(!is.na(family)))
  
  NDC_LISTING_CT_all = df %>% filter(valid >0 & !is.na(family)) %>% .$total_listings %>% sum
  NDC_LISTING_PROP_all = NDC_LISTING_CT_all/sum(df$total_listings)
  NDC_LISTING_CT_compset = df %>% filter(valid >0 & !is.na(family)) %>% .$total_listings %>% sum
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


ndc_recalls <- lapply(datalist_valid_ndc_2, FUN = calc_ndc_recallvals)
ndc_recalls <- do.call(args = ndc_recalls,what =  rbind)

summary_ndc 
summary_ndc_1 <- summary_captures_ %>% 
  select(1:3) %>% 
  slice(2:9) %>%
  cbind(ndc_recalls)

final_res <- summary_captures_ %>% 
  mutate(ValidALLCapsProp = ValidCaptures/TotalCaptures, 
         ValidUniqCaptsProp = ValidUniqCapts/UniqueCaptures) %>% 
  left_join(summary_ndc_1, by = c("Set", "Proximity", "Method"))

# write.csv(final_res, "data/final_res/summary_methods_precision_recall_overall.csv")
final_res <- read.csv("data/final_res/summary_methods_precision_recall_overall.csv")

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


ndc_comp_recalls <- lapply(datalist_valid_ndc_2, FUN = calc_ndc_recallvals_percomp)
ndc_comp_prec <- lapply(datalist_valid_ndc, FUN = calc_ndc_precvals_percomp)


company_prec_recall <- final_res %>% select(1:3) %>% slice(2:9) %>% mutate(COMP_NDC_RECALL_MEAN = lapply(ndc_comp_recalls, function(x) {mean(x$comp_recall)}) %>% unlist, 
                                                    COMP_NDC_RECALL_MEDIAN = lapply(ndc_comp_recalls, function(x) {median(x$comp_recall)}) %>% unlist,
                                                    COMP_NDC_RECALL_MAX = lapply(ndc_comp_recalls, function(x) {max(x$comp_recall)}) %>% unlist,
                                                    
                                                    COMP_NDC_PREC_MEAN = lapply(ndc_comp_prec, function(x) {mean(x$comp_prec)}) %>% unlist, 
                                                    COMP_NDC_PREC_MEDIAN = lapply(ndc_comp_prec, function(x) {median(x$comp_prec)}) %>% unlist,
                                                    COMP_NDC_PREC_MAX = lapply(ndc_comp_prec, function(x) {max(x$comp_prec)}) %>% unlist)

# write.csv(company_prec_recall, "data/final_res/summary_methods_precision_recall_bycomp.csv")

portfolio_size <- ndc_drugs_nestedlistings %>% filter(!is.na(family)) %>% group_by(family) %>% summarise(total_listings = sum(total_listings), total_drugs = n())

plot_8 <- left_join(ndc_comp_recalls[[8]], ndc_comp_prec[[8]], by = "family") %>% left_join(portfolio_size, by = "family")

plot_8 <- list()

for (i in 1:length(ndc_comp_prec)) {
  plot_8[[i]] <- ndc_comp_prec[[i]] %>% left_join(ndc_comp_recalls[[i]], by = "family")  %>% left_join(portfolio_size, by = "family")
}

library(ggplot2)

ggplot(plot_8 %>% filter(!is.na(comp_prec)), aes(x = comp_prec, y = total_listings)) + 
  geom_count() +
  geom_label(data = plot_8 %>% filter(!is.na(comp_prec) & total_listings > 200), aes( label = family), vjust = -1)

ggplot(plot_8 %>% filter(!is.na(comp_recall)), aes(x = comp_recall, y = total_listings)) + 
  geom_count() +
  geom_label(data = plot_8 %>% filter(!is.na(comp_recall) & total_listings > 200), aes( label = family), vjust = -1)


the_hundos <- list()

for (i in 1:length(ndc_comp_prec)) {
  the_hundos[[i]] <- plot_8[[i]] %>% filter(comp_prec == 1 & comp_recall == 1)
}

ggplot(plot_8 %>% filter(!is.na(comp_prec)), aes(x = comp_prec, y = total_drugs)) + 
  geom_count() +
  geom_label(data = plot_8 %>% filter(!is.na(comp_prec) & total_drugs > 100), aes( label = family), vjust = -1)

ggplot(plot_8 %>% filter(!is.na(comp_recall)), aes(x = comp_recall, y = total_drugs)) + 
  geom_count() +
  geom_label(data = plot_8 %>% filter(!is.na(comp_recall) & total_drugs > 100), aes( label = family), vjust = -1)


ggplot(plot_8, aes(x = comp_prec, y = comp_recall, colour = total_listings)) + 
  geom_count() + 
  geom_label(data = plot_8 %>% filter(total_listings <10), aes( label = family), vjust = -1)

ggplot(plot_8, aes(x = comp_prec, y = comp_recall, colour = total_drugs)) + 
  geom_count() + 
  geom_label(data = plot_8 %>% filter(total_drugs >100), aes( label = family), vjust = -1)

set <- summary_captures_ %>% slice(2:9) %>% .$Set

for (i in 1:length(plot_8)) {
  recall_plot <- ggplot(plot_8[[i]], aes(x = set[i], y = comp_recall)) +
    geom_violin() 
  
  print(recall_plot)
}

for (i in 1:length(plot_8)) {
  prec_plot <- ggplot(plot_8[[i]], aes(x = set[i], y = comp_prec)) +
    geom_violin() 
  
  print(prec_plot)
}

for (i in 1:length(plot_8)) {
  scatter_plot <- ggplot(plot_8[[i]], aes(x = comp_prec, y = comp_recall, colour = total_listings)) + 
    geom_count() + 
    geom_label(data = plot_8[[i]] %>% filter(total_listings > 200 ), aes( label = family), vjust = -1)
  
  
  print(scatter_plot)
}




