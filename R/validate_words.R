

validate_captures <- function(df) {
  test_words <- unique(df[,"Word"])
  # PROP
  prop_low_unq <- readRDS("data/ndc_validateset_proplownames.RDS")
  pos_PROP <- list()
  for (i in 1:length(test_words)) {
    pos_PROP[[i]] <- grep(x = prop_low_unq$Prop_low, pattern =  test_words[i])
  }
  # NONPROP
  nonprop_low_unq <- readRDS("data/ndc_validateset_nonproplownames.RDS")
  pos_NONPROP <- list()
  for (i in 1:length(test_words)) {
    pos_NONPROP[[i]] <- grep(x = nonprop_low_unq$Nonprop_low, pattern =  test_words[i])
  }
  # RES? 
  
  res <- data.frame(Words = test_words,  Index_PROP = I(pos_PROP), Index_NONPROP = I(pos_NONPROP)) %>% 
    mutate(Matches_PROP = lengths(Index_PROP), Matches_NONPROP = lengths(Index_NONPROP), 
           Valid_PROP = Matches_PROP > 0, Valid_NONPROP = Matches_NONPROP > 0)
  
  res <- df %>% left_join(res, by = c("Word" = "Words")) %>% mutate(VALID = (Valid_PROP + Valid_NONPROP) > 0)
  
}
