
check_company_name <- function(word_vec, mode, case) {
  new_ref_companies <- readRDS("data/new_ref_companies.RDS")
  # patthun = paste0("^", new_ref_companies$token_hun, "$", collapse = "|")

  if(case == "none") {
    patt = paste0("^", new_ref_companies$Tokens, "$", collapse = "|")
  }

  if(case == "low") {
    patt = paste0("^", new_ref_companies$token_low, "$", collapse = "|")
  }

  if(mode == "TF") {
    comp_YN <- stringr::str_detect(word_vec, patt)
  }

  if(mode == "extract") {
    comp_YN <- stringr::str_extract_all(word_vec, patt)
  }

  comp_YN

}
