
check_brand <- function(word_vec) {
  brand = stringr::str_extract(word_vec, "®|™")
  brand
}
