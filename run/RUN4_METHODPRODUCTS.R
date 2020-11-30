library(dplyr)

folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/"
allnoneng <- "ALL_NON_ENGLISH/"
brand <- "BRAND/"
launch <- "PROXIMITY_LAUNCH/"
newprod <- "PROXIMITY_NEWPRODUCT_RELAXED/"
newdrug <-  "PROXIMITY_NEWDRUG_RELAXED/"

iterateme <- brand # finished allnoneng
captures_ <- list.files(paste0(folder, iterateme))
capture_paths <- paste0(folder, iterateme, list.files(paste0(folder, iterateme)))


example <- readRDS(capture_paths[4])

example_products <- example %>% 
  filter(eng_C == FALSE) %>% 
  mutate(tick = testbinnR::check_ticker(Word, mode = "TF"), 
         comp_name = testbinnR::check_company_name(Word, case = "low", mode = "TF")) %>%
  testbinnR::possible_products()

example_products %>% head()


for (i in 1:length(captures_)) {
  print(i)
  captures <- readRDS(capture_paths[i])
  
  products <- captures %>% 
    filter(eng_C == FALSE) %>% 
    mutate(tick = testbinnR::check_ticker(Word, mode = "TF"), 
           comp_name = testbinnR::check_company_name(Word, case = "low", mode = "TF")) %>%
    testbinnR::possible_products()
  
  saveRDS(products,
           paste0("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN4_METHODPRODUCTS/", 
                  brand,
                  captures_[i]))
  
  print(i/length(capture_paths))
}

prod_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN4_METHODPRODUCTS/"
allnoneng_capts <- paste0(prod_folder, allnoneng, list.files(paste0(prod_folder, allnoneng)))
brand_capts <-  paste0(prod_folder, brand, list.files(paste0(prod_folder, brand)))
brand_capts <-  paste0(folder, brand, list.files(paste0(prod_folder, brand)))

brands <- brand_capts %>% 
  purrr::map(readRDS)
