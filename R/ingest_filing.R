

ingest_filing <- function(path, ascii) {
  raw_txt_of_xml <- path %>%
    readr::read_file() %>%
    remove_doc_types() %>%
    handle_encoding() %>%
    parse_xml(ascii)
  raw_txt_of_xml
}


# test <- ingest_filing("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/1003642_10-K_2013/1003642_10-K_2013-02-26.txt")



