library(dplyr)
library(testbinnR)

project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"

datapath1 <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/" 
datapath <- "/project/biocomplexity/sdad/projects_data/volume_nyc1_01/business_innovation/" 

ndc_sec_ref <- readxl::read_excel(paste0(datapath1, "working/NDC_SEC_CompNames/final_ndc_sec_companies.xlsx"), sheet = "finalset") %>% select(-12,-13, -14)
colnames(ndc_sec_ref) <- dataplumbr::name.standard_col_names(colnames(ndc_sec_ref))
ndc_sec_ref <- ndc_sec_ref %>% filter(is.na(dupe))
CORPFAM_CIK <- ndc_sec_ref %>% transmute(family, cik = as.character(cik)) %>% distinct()

summary_captures <- readRDS(paste0(project_folder, "working/SEC_Packaging/SummaryCaptures_NOV192020v3.RDS"))

datalist_valid <- readRDS(paste0(project_folder, "working/SEC_Packaging/methodresults_valid_list_NOV182020v1.RDS"))

## START COUNTING AT COMPANY LEVEL

datasets <- c("Non-English", "Brand", "Launch - 20", "Launch - 5", 
              "New Product - 20", "New Product - 5", "New Drug - 20", "New Drug - 5")

datalist_valid_mentions <- list()
datalist_valid_innovations <- list()

totalmentionsperfamily <- list()
totalproductsperfamily <- list()
valinval_productsperfamily <- list()

for (i in 1:length(datalist_valid)) {
  # COUNT ALL MENTIONS PER COMPANY REGARDLESS OF TIME - broken out by validity 
  # FAMILY, VALID, CAPTURE, # MENTIONS
 datalist_valid_mentions[[i]] <- datalist_valid[[i]]  %>% left_join(CORPFAM_CIK, by = c("Company" = "cik")) %>% count_valid_mentions %>% mutate(Method = datasets[i])
  
  # COUNT ALL POTENTIAL INNOVATIONS PER COMPANY REGARDLESS OF TIME - broken out by validity 
  # FAMILY, VALID #, INVALID #
 datalist_valid_innovations[[i]] <- datalist_valid[[i]]  %>% left_join(CORPFAM_CIK, by = c("Company" = "cik")) %>% count_valid_innovations %>% mutate(Method = datasets[i])
  
  ####
  
  # SUM ALL MENTIONS PER COMPANY REGARDLESS OF TIME - with new METHOD column
  # FAMILY, TOTAL CAPTURES (AKA MENTIONS), METHOD
 totalmentionsperfamily[[i]] <- datalist_valid_mentions[[i]] %>% group_by(Family) %>% summarise(total_captures = sum(Mentions)) %>% mutate(Method = datasets[i])
  
  # SUM ALL PRODUCTS PER COMPANY REGARDLESS OF TIME - with new METHOD column
  # FAMILY, TOTAL PRODS, METHOD
 totalproductsperfamily[[i]] <- datalist_valid_innovations[[i]] %>% reshape2::melt() %>% group_by(Family) %>% summarise(total_prods = sum(value)) %>% mutate(Method = datasets[i])
  
  # SUM ALL PRODUCTS PER COMPANY REGARDLESS OF TIME - broken out by VALID/INVALID
  # FAMILY, VALID #, INVALID #,  METHOD
  valinval_productsperfamily[[i]] <- datalist_valid_innovations[[i]] %>% group_by(Family) %>% summarise(Valid = sum(Valid), Invalid = sum(Invalid)) %>% mutate(Method = datasets[i])
  
}

# saveRDS(datalist_valid_mentions, paste0(project_folder, "working/SEC_Packaging/methodresults_valid_MENTIONct_NOV182020v1.RDS"))
# saveRDS(datalist_valid_innovations, paste0(project_folder, "working/SEC_Packaging/methodresults_valid_INNOVct_NOV182020v1.RDS"))



### COLLAPSE THE 6 PROXIMITY METHODS INTO 2

proximity_20 <- datalist_valid_mentions[c(3,5,7)]
proximity_5 <- datalist_valid_mentions[c(4,6,8)]

for (i in 1:3) {
  proximity_20[[i]] <- proximity_20[[i]] %>% mutate(Method = datasets[c(3,5,7)][i])
  proximity_5[[i]] <- proximity_5[[i]] %>% mutate(Method = datasets[c(4,6,8)][i])
}

proximity_20 <- do.call(rbind, proximity_20)
proximity_5 <- do.call(rbind, proximity_5)

### 1) TOTAL CAPTURES for PROXIMITY METHODS

total_captures_proximity_20 <- proximity_20 %>% 
  reshape2::dcast(Family + Valid + PotentialInnovation ~ Method, value.var = "Mentions") %>%
  mutate(total_captures = ifelse(!is.na(`Launch - 20`), `Launch - 20`, 0) + 
                      ifelse(!is.na(`New Drug - 20`), `New Drug - 20`, 0) + 
                          ifelse(!is.na(`New Product - 20`), `New Product - 20`, 0),
         MethodCt = ifelse(!is.na(`Launch - 20`), 1, 0) + 
                        ifelse(!is.na(`New Drug - 20`), 1, 0) + 
                            ifelse(!is.na(`New Product - 20`), 1, 0)) %>% 
  mutate(Method = "Proximity - 20")

total_captures_proximity_5 <- proximity_5 %>% 
  reshape2::dcast(Family + Valid + PotentialInnovation ~ Method, value.var = "Mentions") %>%
  mutate(total_captures = ifelse(!is.na(`Launch - 5`), `Launch - 5`, 0) + 
           ifelse(!is.na(`New Drug - 5`), `New Drug - 5`, 0) + 
           ifelse(!is.na(`New Product - 5`), `New Product - 5`, 0),
         MethodCt = ifelse(!is.na(`Launch - 5`), 1, 0) + 
           ifelse(!is.na(`New Drug - 5`), 1, 0) + 
           ifelse(!is.na(`New Product - 5`), 1, 0)) %>% 
  mutate(Method = "Proximity - 5")

total_captures_proximity_20_ <- total_captures_proximity_20 %>% group_by(Family, Method) %>% summarise(total_captures = sum(total_captures))
total_captures_proximity_5_ <- total_captures_proximity_5 %>% group_by(Family, Method) %>% summarise(total_captures = sum(total_captures))

### 2) TOTAL PRODUCTS for PROXIMITY METHODS

total_products_proximity_20 <- proximity_20 %>% 
  select(Family, PotentialInnovation) %>% 
  distinct() %>%
  count(Family) %>% 
  transmute(Family, total_prods = n, Method = "Proximity - 20")
  
total_products_proximity_5 <- proximity_5 %>% 
  select(Family, PotentialInnovation) %>% 
  distinct() %>%
  count(Family) %>% 
  transmute(Family, total_prods = n, Method = "Proximity - 5")

### 3) TOTAL PRODUCTS for PROXIMITY METHODS - broken out by valid/invalid

total_products_proximity_20_valinval <- proximity_20 %>% 
  select(Family, Valid, PotentialInnovation) %>% 
  distinct() %>%
  count(Family, Valid) %>% 
  reshape2::dcast(Family ~Valid, value.var = "n") %>%
  transmute(Family, Invalid = `FALSE`, Valid = `TRUE`, Method = "Proximity - 20")

total_products_proximity_5_valinval <- proximity_5 %>% 
  select(Family, Valid, PotentialInnovation) %>% 
  distinct() %>%
  count(Family, Valid) %>% 
  reshape2::dcast(Family ~Valid, value.var = "n") %>%
  transmute(Family, Invalid = `FALSE`, Valid = `TRUE`, Method = "Proximity - 5")


#################

# RESULTS TABLES

total_captures_ <- rbind(do.call(rbind, totalmentionsperfamily), total_captures_proximity_20_, total_captures_proximity_5_)
total_products_ <- rbind(do.call(rbind, totalproductsperfamily), total_products_proximity_20, total_products_proximity_5)
total_valinval_products_ <- rbind(do.call(rbind, valinval_productsperfamily), total_products_proximity_20_valinval, total_products_proximity_5_valinval)

company_results <- total_captures_ %>% 
  left_join(total_products_, by = c("Family", "Method")) %>% 
  left_join(total_valinval_products_, by = c("Family", "Method")) %>%
  transmute(Family, Method, Total_Capts = total_captures, Total_POTINN = total_prods, Valid, Invalid)

PERCVAL_COMPANY <- company_results %>% 
  select(Family, Method, Total_Capts, Total_POTINN, Valid, Invalid) %>% 
  mutate(Perc_Val = Valid/Total_POTINN) 

PERCVAL_METHOD <- PERCVAL_COMPANY %>%
  group_by(`Method`) %>%
  summarise(Mean = mean(Perc_Val, na.rm = TRUE), Median = median(Perc_Val, na.rm = TRUE), Max = max(Perc_Val, na.rm = TRUE), Min = min(Perc_Val, na.rm = TRUE), StDev = sd(Perc_Val, na.rm = TRUE))

### NDC CALCULATION

datalist_valid_mentions_all <- do.call("rbind", datalist_valid_mentions) 

test_set_words <- datalist_valid_mentions_all  %>% filter(VALID == TRUE) %>% head(20) %>% select(family, Company, Month, Year, Captures, Word, Index_PROP, Index_NONPROP)

prop_test_set <- test_set_words %>% tidyr::unnest(Index_PROP) %>% select(-Index_NONPROP)
nonprop_test_set <- test_set_words %>% tidyr::unnest(Index_NONPROP) %>% select(-Index_PROP)

prop_test_set  %>% left_join(ndc_val_set %>% mutate(id = as.integer(id)), by = c("Index_PROP" = "id")) %>% View()

#
prop_low_ids <- readRDS("data/ndc_validateset_proplowids.RDS")
ndc_val_set <- readRDS("data/ndc_validateset.RDS")

ndc_product <-  readr::read_tsv(paste0(datapath1, "original/NDC/product.txt")) %>% mutate(year = stringr::str_extract(string = STARTMARKETINGDATE, pattern = "^\\d{4}"))

ndc_product %>% head()







