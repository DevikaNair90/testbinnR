library(stringr)
library(dplyr)

project_folder <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/"

# GET NDC COMPANIES
ndc_sec_ref <- readxl::read_excel(paste0(project_folder, "working/NDC_SEC_CompNames/final_ndc_sec_companies.xlsx"), sheet = "finalset") %>% select(-12,-13, -14)
colnames(ndc_sec_ref) <- dataplumbr::name.standard_col_names(colnames(ndc_sec_ref))
ndc_sec_ref <- ndc_sec_ref %>% filter(is.na(dupe)) %>% select(-dupe)

refciks <- unique(ndc_sec_ref$cik)

## GRAB ALL PATHS
paths_file <- paste0(project_folder, "original/edgar_filings/ALL_SEC_files.txt")
file_headers <- readr::read_tsv(paths_file, col_names = FALSE)

patt <- paste0("^(", paste0(refciks, collapse = "|"), ")" )

ndc_file_headers <- file_headers[str_detect(file_headers$X1, pattern = patt),][[1]]


## GET ALL FILINGS PATHS
paths <- paste0(project_folder, "original/edgar_filings/Edgar_filings_folders/", file_headers$X1)
paths[9]
file_names <- unique(list.files(paths, full.names = TRUE))
file_names[9]

## GET NDC FILINGS PATHS
ndc_paths <- paste0(project_folder, "original/edgar_filings/Edgar_filings_folders/", ndc_file_headers)
ndc_file_names <- unique(list.files(ndc_paths, full.names = TRUE))

## LOOP THROUGH INGESTION

for (i in 1:length(ndc_file_names)) {
  print(filing_meta(ndc_file_names[i]))
  filing <- ingest_filing(path = ndc_file_names[i], ascii = FALSE)
  print(nchar(filing))
  saveRDS(filing, file = paste0(project_folder, "working/SEC_Packaging/RUN1_INGESTION/", str_remove(basename(ndc_file_names[i]), ".txt$"), ".RDS"))
}

