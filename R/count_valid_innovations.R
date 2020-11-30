count_valid_innovations <- function(df) {
  res_table_INNOVATIONS  <-  df %>% 
    count(family, VALID, Captures) %>% 
    transmute(Family = family, Valid = VALID, PotentialInnovation = Captures, Mentions = n) %>% 
    count(Family, Valid) %>% 
    transmute(Family, Valid, PotentialInnovations = n) %>% 
    reshape2::dcast(Family ~ Valid, sum) %>% 
    transmute(Family, Valid = `TRUE`, Invalid = `FALSE`)
  
  res_table_INNOVATIONS
}
