

clean_word_extractions <- function(word_vec, char_min, char_max, no_num, camel, camel_char_thres, encoding) {
  # REMOVE BLANKS
  word_vec <- word_vec[dataplumbr::var.is_blank(word_vec) == FALSE]
  # ACCOUNT FOR ENCODING (?)
  if (encoding == TRUE) {
    word_vec <- word_vec %>%
      iconv(to = "latin1") %>%
      strsplit(split = "[[:punct:]]") %>%
      unlist()
  }

  # CHARACTER COUNT MIN
  if (missing(char_min) == FALSE) {
    word_vec <- word_vec[nchar(word_vec) >= char_min]
  }
  # CHARACTER COUNT MAX
  if (missing(char_max) == FALSE) {
    word_vec <- word_vec[nchar(word_vec) <= char_max]
  }
  # REMOVE NUMBER ONLY STRINGS
  if (no_num == TRUE) {
    word_vec <- word_vec[grepl(x = word_vec, pattern =  "[[:alpha:]]") == TRUE]
  }

  if (camel == TRUE) {
    word_vec_ncc <- word_vec[nchar(word_vec) <= camel_char_thres]

    word_vec_cc <- word_vec[stringr::str_detect(word_vec, "[a-z][A-Z][a-z]") & nchar(word_vec) > camel_char_thres] %>%
      stringr::str_split("(?=[A-Z])", simplify = FALSE) %>%
      unlist() %>%
      stringr::str_squish()

    word_vec_cc <- word_vec_cc[dataplumbr::var.is_blank(word_vec_cc) == FALSE]

    # REMOVE BLANKS
    word_vec_cc <- word_vec_cc[dataplumbr::var.is_blank(word_vec_cc) == FALSE]
    # CHARACTER COUNT MIN
    if (missing(char_min) == FALSE) {
      word_vec_cc <- word_vec_cc[nchar(word_vec_cc) >= char_min]
    }
    # CHARACTER COUNT MAX
    if (missing(char_max) == FALSE) {
      word_vec_cc <- word_vec_cc[nchar(word_vec_cc) <= char_max]
    }
    # REMOVE NUMBER ONLY STRINGS
    if (no_num == TRUE) {
      word_vec_cc <- word_vec_cc[grepl(x = word_vec_cc, pattern =  "[[:alpha:]]") == TRUE]
    }


    word_vec <- c(word_vec_ncc, word_vec_cc)
  }

  word_vec

}




