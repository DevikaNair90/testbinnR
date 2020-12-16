validate_ndc <- function(df) {
  test_words <- unique(df[,"Word"])
  # PROP
  pos_PROP <- list()
  for (i in 1:length(test_words)) {
    pos_PROP[[i]] <- grep(x = prop_matchset_drugid$Prop_low, pattern =  test_words[i])
  }
  # NONPROP
  pos_NONPROP <- list()
  for (i in 1:length(test_words)) {
    pos_NONPROP[[i]] <- grep(x = nonprop_matchset_drugid$Nonrop_low, pattern =  test_words[i])
  }
  # RES? 
  # ndc_drugs_nestedlistings
  intermediate <- data.frame(Words = test_words,  Index_PROP = I(pos_PROP), Index_NONPROP = I(pos_NONPROP)) %>% 
    mutate(Matches_PROP = lengths(Index_PROP), Matches_NONPROP = lengths(Index_NONPROP), 
           Valid_PROP = Matches_PROP > 0, Valid_NONPROP = Matches_NONPROP > 0)
  
  intermediate <- df %>% left_join(intermediate, by = c("Word" = "Words")) %>% mutate(VALID = (Valid_PROP + Valid_NONPROP) > 0)
  
  intermediate
  
}
