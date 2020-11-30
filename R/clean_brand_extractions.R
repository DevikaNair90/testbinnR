
clean_brand_extractions <- function(word_vec, brand_char_max, types = c("®", "™")) {
  # COMBINE SHORT BRAND-PROTECTED STRINGS WITH PRIOR WORD, DELETE PRIOR WORD
  for (i in 1:length(word_vec)) {
    if(stringr::str_detect(word_vec[i], pattern = paste0(types, collapse = "|")) & nchar(word_vec[i]) < brand_char_max) {
      word_vec[i] <- paste0(word_vec[i-1], "-", word_vec[i])
      word_vec[i-1] <- ""
    }
  }
  word_vec
}

