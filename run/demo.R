library(dplyr)
loadme <- paste0("./R/", list.files("./R/"))
sapply(X = loadme, FUN = source)

file = "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/1003642_10-K_2013/1003642_10-K_2013-02-26.txt"

text <- file %>% ingest_filing(ascii = FALSE)


words_no_encoding <- capture_words(path = file, txt_string = text,
                brand_char_max = 5,
                char_min = 3, char_max = 25, no_num = TRUE, camel = TRUE, camel_char_thres = 20, encoding = FALSE)


##

launch_proximity_words <- get_proximity_words(word_df = words_no_encoding, mode = "string", search = "launch", n =  20)


##

new_prod_locations <- words_no_encoding %>%
  get_proximity_words(mode = "string", search = "\\bnew(er|est|\\b)", n = 1) %>%
  mutate(orig_id = id) %>%
  get_proximity_words(mode = "string", search = "product", n = 1) %>%
  filter(pstn %in% c("before", "target"))

new_product_words <- words_no_encoding %>%
  get_proximity_words(mode = "id", search = new_prod_locations$orig_id, n = 20) ## note - target column is good, but before/after loses target category


###

launch_proximity_refine <- refine_captures(word_vec = launch_proximity_words$low)
launch_proximity_words <- cbind(launch_proximity_words, launch_proximity_refine) %>%
  select(-Word) %>% mutate(eng_C = hunspell::hunspell_check(Captures, hunspell::dictionary("en_US")),
                           eng_l = hunspell::hunspell_check(low, hunspell::dictionary("en_US")))


launch_proximity_words %>% filter(eng_C == FALSE &  xml == FALSE & dates == FALSE & pharma == FALSE)

##

new_product_refine <- refine_captures(word_vec = new_product_words$low)
new_product_words <- cbind(new_product_words, new_product_refine) %>%
  select(-Word) %>% mutate(eng_C = hunspell::hunspell_check(Captures, hunspell::dictionary("en_US")),
                           eng_l = hunspell::hunspell_check(low, hunspell::dictionary("en_US")))

new_product_words %>% filter(eng_C == FALSE &  xml == FALSE & dates == FALSE & pharma == FALSE)
