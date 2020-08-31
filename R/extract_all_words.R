
extract_all_words <- function(txt_string) {
  words <- txt_string %>%
    stringr::str_split("\\s", simplify = FALSE) %>%
    unlist() %>%
    stringr::str_replace_all(pattern = "[[:punct:]]", replacement = " ") %>%
    stringr::str_split("\\s") %>%
    unlist() %>%
    stringr::str_squish()
  words
}


