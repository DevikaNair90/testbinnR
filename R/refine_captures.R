
refine_captures <- function(word_vec, types = c("xml", "financial", "dates", "pharma")) {
  if(any(stringr::str_detect(word_vec, "[:upper:]"))) {
    word_vec <- stringr::str_to_lower(word_vec)
  }

  data.frame(Word = word_vec) %>%
    mutate(brand = check_brand(Word),
    Word = ifelse(!is.na(brand), stringr::str_remove(string = Word, pattern = brand), Word),
    xml = check_xmlhtml(Word, mode = "TF"),
    fin = check_financial(Word, mode = "TF"),
    dates = check_dates(Word),
    pharma = check_pharma(Word, mode = "TF"),
    comp_name = check_company_name(Word, mode = "TF", case = "low"), 
    tick = check_ticker(Word, mode = "TF")
    )

}

