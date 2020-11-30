
count_valid_mentions <- function(df) {
  res_table_MENTIONS <- df %>% count(family, VALID, Captures) %>% transmute(Family = family, Valid = VALID, PotentialInnovation = Captures, Mentions = n)
  res_table_MENTIONS
}
