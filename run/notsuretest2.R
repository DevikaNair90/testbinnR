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

## NDC Data - give an id and a corp family 
ndc_product_ <- ndc_product %>% 
  mutate(orig_id = rownames(.)) %>% 
  left_join(ndc_sec_ref %>% select(family, ndc_labeler) %>% distinct, by = c("LABELERNAME" = "ndc_labeler")) %>%
  select(orig_id, family, colnames(ndc_product)) 

# saveRDS(ndc_product_, "data/ndc_orig_listings.RDS")

## clean strings of product names, add in corporate families, and rownames

# ndc_validateset <- ndc_product_ %>% 
#   transmute(orig_id = list(orig_id), family, LABELERNAME, PROPRIETARYNAME, NONPROPRIETARYNAME, SUBSTANCENAME) %>%
#   distinct() %>%
#   mutate(Prop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(PROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")),
#          Nonprop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(NONPROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), 
#          Sub_low = str_trim(str_replace_all(str_remove_all(str_to_lower(SUBSTANCENAME), "\\(|\\)"), "\\\u0097|\\\u0093", " "))) %>%
#   # left_join(ndc_sec_ref %>% select(1,11), by = c("LABELERNAME" = "ndc_labeler")) %>%
#   mutate(id = rownames(.)) %>%
#   select(orig_id, id, family, LABELERNAME, PROPRIETARYNAME, NONPROPRIETARYNAME, SUBSTANCENAME, Prop_low, Nonprop_low, Sub_low)

# oldversion <- readRDS("data/ndc_validateset.RDS")

ndc_validateset <- ndc_product_ %>% 
  tidyr::nest(orig_ID = c("orig_id"), data = c("PRODUCTID", "PRODUCTNDC", "PRODUCTTYPENAME", "PROPRIETARYNAMESUFFIX", "DOSAGEFORMNAME", "ROUTENAME", 
                                                              "STARTMARKETINGDATE", "ENDMARKETINGDATE", "MARKETINGCATEGORYNAME", "APPLICATIONNUMBER", "ACTIVE_NUMERATOR_STRENGTH",
                                                              "ACTIVE_INGRED_UNIT", "PHARM_CLASSES", "DEASCHEDULE", "NDC_EXCLUDE_FLAG", "LISTING_RECORD_CERTIFIED_THROUGH")) %>%
  mutate(Prop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(PROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")),
         Nonprop_low = str_trim(str_replace_all(str_remove_all(str_to_lower(NONPROPRIETARYNAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")), 
         Sub_low = str_trim(str_replace_all(str_remove_all(str_to_lower(SUBSTANCENAME), "\\(|\\)"), "\\\u0097|\\\u0093", " ")),
         id = rownames(.)) %>%
  select(orig_ID, id, family, LABELERNAME, PROPRIETARYNAME, NONPROPRIETARYNAME, SUBSTANCENAME, Prop_low, Nonprop_low, Sub_low, year, data)

# saveRDS(ndc_validateset, "data/ndc_validateset2.RDS")


prop_low_ids <- ndc_validateset %>% select(orig_ID, id, Prop_low) %>% mutate(total_listings = unlist(lapply(orig_ID, FUN = nrow))) %>% select(id, Prop_low, total_listings, orig_ID)
prop_low_unq <- prop_low_ids %>% count(Prop_low) %>% select(-n)
# saveRDS(prop_low_ids, "data/ndc_validateset_proplowids.RDS")
# saveRDS(prop_low_unq, "data/ndc_validateset_proplownames.RDS")
nonprop_low_ids <- ndc_validateset %>% select(orig_ID, id, Nonprop_low) %>% mutate(total_listings = unlist(lapply(orig_ID, FUN = nrow))) %>% select(id, Nonprop_low, total_listings, orig_ID)
nonprop_low_unq <- nonprop_low_ids %>% count(Nonprop_low) %>% select(-n)
# saveRDS(nonprop_low_ids, "data/ndc_validateset_nonproplowids.RDS")
# saveRDS(nonprop_low_unq, "data/ndc_validateset_nonproplownames.RDS")


# 
# 
# testbrand <- brand %>% select(Captures, Word) %>% head(100) # mutate(val_PROP = grep(x = Word, pattern = ndc_validateset$Prop_low))
# 
# testbrand <- testbrand %>% distinct()
# 
# test_words <- unique(newdrug_5_allnoneng[,"low"])
# props <- unique(ndc_validateset$Prop_low)
# 
# validate_captures <- function(df) {
#   test_words <- unique(df[,"Word"])
#   # PROP
#   prop_low_unq <- readRDS("data/ndc_validateset_proplownames.RDS")
#   pos_PROP <- list()
#   for (i in 1:length(test_words)) {
#     pos_PROP[[i]] <- grep(x = prop_low_unq$Prop_low, pattern =  test_words[i])
#   }
#   # NONPROP
#   nonprop_low_unq <- readRDS("data/ndc_validateset_nonproplownames.RDS")
#   pos_NONPROP <- list()
#   for (i in 1:length(test_words)) {
#     pos_NONPROP[[i]] <- grep(x = nonprop_low_unq$Nonprop_low, pattern =  test_words[i])
#   }
#   # RES? 
#   
#   res <- data.frame(Words = test_words,  Index_PROP = I(pos_PROP), Index_NONPROP = I(pos_NONPROP)) %>% 
#     mutate(Matches_PROP = lengths(Index_PROP), Matches_NONPROP = lengths(Index_NONPROP), 
#            Valid_PROP = Matches_PROP > 0, Valid_NONPROP = Matches_NONPROP > 0)
#   
#   res <- df %>% left_join(res, by = c("Word" = "Words")) %>% mutate(VALID = (Valid_PROP + Valid_NONPROP) > 0)
#   
# }
# 
# launch5_noneng___res <- validate_captures(launch5_noneng)
# table(launch5_noneng___res$VALID) 
# head(launch5_noneng___res, 1)
# 
# newproduct_5_allnoneng___res %>% filter(VALID == FALSE) %>% .$low %>% unique
# 
# # brand %>% filter(stringr::str_detect(Word, "a-"))
# # brand %>% filter(stringr::str_detect(Word, paste0("\\b","a-", "\\b")))
# # brand %>% filter(Word == "a-") %>% filter(stringr::str_detect(Word, pattern = "a-"))
# # ndc_validateset %>% filter(stringr::str_detect(Prop_low, "a-")) %>% .$Prop_low
# # res %>% filter(stringr::str_detect(Words, pattern = paste0("\\b","a-",)))
# # res %>% arrange(desc(matches))
# 
# sec_percorpfamily <- ndc_sec_ref %>% count(family, cik) %>% transmute(filer_family = family, cik = as.character(cik)) # %>% count(family)
# 
# brand_prop_res <- sec_percorpfamily %>% 
#   full_join(brand, by = c("cik" = "Company")) %>% 
#   # left_join(ndc_sec_ref %>% transmute(cik = as.character(cik), family) %>% distinct(), by = c("Company" = "cik")) %>% 
#   left_join(res, by = c("Word" = "Words")) 
# 
# brand_prop_res %>%
#   head() %>% 
#   tidyr::unnest(Index) %>% 
#   left_join(ndc_validateset %>% transmute(id = as.integer(id), family, LABELERNAME), by = c("Index" = "id")) %>% 
#   View()
# 
# brand %>% left_join(res, by = c("Word" = "Words")) %>% head() %>% tidyr::unnest(Index) %>% left_join(ndc_validateset %>% transmute(id = as.integer(id), family, LABELERNAME), by = c("Index" = "id")) %>% View()
# 
# 
