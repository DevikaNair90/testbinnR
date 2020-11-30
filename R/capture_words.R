
capture_words <- function(path, txt_string, brand_char_max, char_min, char_max, no_num, camel, camel_char_thres, encoding) {

  words <- txt_string %>%
    extract_all_words() %>%
    clean_brand_extractions(brand_char_max) %>%
    clean_word_extractions(char_min, char_max, no_num, camel, camel_char_thres, encoding)

  filing_word_capture <- tibble::tibble(filing_meta(path), Captures = words)
}

