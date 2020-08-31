
filing_meta <- function(path) {
  company <- stringr::str_match(basename(path), "(^.*?)_")[, 2]
  date <- stringr::str_match(basename(path), "([0-9][0-9][0-9][0-9])-([0-9][0-9])-[0-9][0-9]")
  month <- date[, 3]
  year <- date[, 2]

  filing_meta <- tibble::tibble(
    "Company" = company,
    "Month" = month,
    "Year" = year
  )

  filing_meta
}

# filing_meta("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/1003642_10-K_2013/1003642_10-K_2013-02-26.txt")



