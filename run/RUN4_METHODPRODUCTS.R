library(dplyr)

folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/working/SEC_Packaging/RUN3_SUBSETMETHODS/"
allnoneng <- "ALL_NON_ENGLISH/"
brand <- "BRAND/"
launch <- "PROXIMITY_LAUNCH/"
newprod <- "PROXIMITY_NEWPRODUCT_RELAXED/"
newdrug <-  "PROXIMITY_NEWDRUG_RELAXED/"

captures <- list.files(paste0(folder, launch))
capture_paths <- paste0(folder, launch, list.files(paste0(folder, launch)))
example <- readRDS(capture_paths[2])

example_products <- example %>% 
  filter(eng_C == FALSE) %>% 
  mutate(tick = check_ticker(Word, mode = "TF"), comp_name = check_company_name(Word, case = "low", mode = "TF"), fin = check_financial(Word, "TF")) %>%
  possible_products()

example_products


for (i in 1:length(captures)) {
  captures <- readRDS(capture_paths[i])
  
}

