
possible_products <- function(word_df) {
  word_df_prod <- word_df %>% filter(
    xml == FALSE &
      fin == FALSE &
      dates == FALSE &
      pharma == FALSE &
      eng_C == FALSE &
      tick == FALSE &
      comp_name == FALSE
  )

  if("pstn" %in% colnames(word_df)) {
    word_df_prod <- word_df_prod %>% select(Company, Month, Year, Captures, low, brand, id, target, pstn)
  }

  word_df_prod
}
