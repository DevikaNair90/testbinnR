# TEST ENCODING

library(dplyr)
loadme <- paste0("./R/", list.files("./R/"))
sapply(X = loadme, FUN = source)

file_1 <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/1003642_10-K_2013/1003642_10-K_2013-02-26.txt"

file_2 <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/944809_10-K_2013/944809_10-K_2013-03-18.txt"

text_1 <- ingest_filing(file_1, brand_ascii = TRUE)
text_1_F <- ingest_filing(file_1, brand_ascii = FALSE)
text_2 <- ingest_filing(file_2, brand_ascii = TRUE)
text_2_F <- ingest_filing(file_2, brand_ascii = FALSE)
stringr::str_count(text_1, "®|™") #307
stringr::str_count(text_1_F, "®|™") #0
stringr::str_count(text_2, "®|™") #0
stringr::str_count(text_2_F, "®|™") #0

text_1_F_1 <- ingest_filing(file_1, ascii = TRUE)
text_1_F_2 <- ingest_filing(file_1, ascii = FALSE)
# text_2_F_2 <- ingest_filing(file_2, ascii = FALSE)

stringr::str_count(text_1_F_1, "®|™") #307
stringr::str_count(text_1_F_2, "®|™") #307



