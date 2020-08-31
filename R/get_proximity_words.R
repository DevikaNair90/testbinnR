
get_proximity_words <- function(word_df, mode, search, n) {

  wordlist <- word_df %>%
    mutate(low = stringr::str_to_lower(Captures),
           id = row_number())

  if(mode == "string") {
    wordlist <- wordlist %>%
      mutate(target = ifelse(stringr::str_detect(stringr::str_to_lower(Captures), pattern = search), 1, 0))
  }

  if(mode == "id") {
    wordlist <- wordlist %>%
      mutate(target = ifelse(id %in% search, 1, 0))
  }

  target_rows <- wordlist %>% filter(target == 1)

  if(nrow(target_rows) > 0) {
    target_rows$seq_up <- NA
    target_rows$seq_down <- NA

    for (i in 1:length(target_rows$id)) {
      target_rows$seq_up[i] <- list(seq(from = target_rows$id[i] - 1, to = target_rows$id[i] - n))
      target_rows$seq_down[i] <- list(seq(from = target_rows$id[i] + 1, to = target_rows$id[i] + n))
    }
    before_target <- unlist(target_rows$seq_up)
    after_target <- unlist(target_rows$seq_down)

    context_words <- wordlist %>%
      filter(id %in% c(target_rows$id, before_target, after_target)) %>%
      arrange(Company, Year, Month, id) %>%
      mutate(pstn = ifelse(id %in% before_target, "before", ifelse(id %in% after_target, "after", "target")))

    context_words
  }

  else { print("Search pattern not found.")}
}

