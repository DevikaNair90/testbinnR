

check_ticker <- function(word_vec, mode) {
  cik_ticker <- readRDS("~/git/testbinnR/data/cik_ticker_dataset.RDS")
  patt_ticker = paste0("^", stringr::str_to_lower(cik_ticker$Ticker), "$", collapse = "|")

  if(mode == "TF") {
    ticker_YN <- stringr::str_detect(word_vec, patt_ticker)
  }

  if(mode == "extract") {
    ticker_YN <- stringr::str_extract_all(word_vec, patt_ticker)
  }

  ticker_YN
}



