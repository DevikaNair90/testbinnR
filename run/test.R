library(dplyr)
loadme <- paste0("./R/", list.files("./R/"))

sapply(X = loadme, FUN = source)

file <- "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/1003642_10-K_2013/1003642_10-K_2013-02-26.txt"
filing_files <- c("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/944809_10-K_2013/944809_10-K_2013-03-18.txt"
, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/944809_10-K_2014/944809_10-K_2014-03-03.txt"
, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/944809_10-K_2015/944809_10-K_2015-02-27.txt"
, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/944809_10-K_2016/944809_10-K_2016-02-29.txt"
, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/944809_10-K_2017/944809_10-K_2017-03-01.txt"
, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/946840_10-K_2013/946840_10-K_2013-02-27.txt"
, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/949699_10-K_2013/949699_10-K_2013-02-26.txt"
, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/949699_10-K_2014/949699_10-K_2014-02-26.txt"
, "/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/edgar_filings/Edgar_filings_folders/949699_10-K_2015/949699_10-K_2015-02-18.txt")



text <- ingest_filing(filing_files[2], ascii = FALSE) #filing_files[1]

# the way this is written, it requires all arguments
## NOTE - if brand_ascii is true, then can't use with latin1 encoding conversion
# words_encoding <- capture_words(path = file, txt_string = text,
#                                 brand_char_max = 5,
#                                 char_min = 3, char_max = 25, no_num = TRUE, camel = TRUE, camel_char_thres = 20, encoding = TRUE)

words_no_encoding <- capture_words(path = filing_files[2], txt_string = text,
                                   brand_char_max = 5,
                                   char_min = 4, char_max = 25, no_num = TRUE, camel = TRUE, camel_char_thres = 20, encoding = FALSE)

#########
res <- get_proximity_words(word_df = words_no_encoding, mode = "string", search = "launch", n =  10)
head(res)
res <- get_proximity_words(word_df = words_no_encoding, mode = "id", search = c(100,  200, 300), n =  10)
head(res)
res <- get_proximity_words(word_df = words_no_encoding %>% tail(1000), mode = "string", search = "launch", n =  10)
head(res)

#########

launch_proximity_words <- get_proximity_words(word_df = words_no_encoding, mode = "string", search = "launch", n =  20)

new_prod_locations <- words_no_encoding %>%
  get_proximity_words(mode = "string", search = "\\bnew(er|est|\\b)", n = 1) %>%
  mutate(orig_id = id) %>%
  get_proximity_words(mode = "string", search = "product", n = 1) %>%
  filter(pstn %in% c("before", "target"))

new_product_words <- words_no_encoding %>%
  get_proximity_words(mode = "id", search = new_prod_locations$orig_id, n = 20) ## note - target column is good, but before/after loses target category

###

all_noneng_words_refine <- refine_captures(words_no_encoding$Captures)
all_noneng_words <- cbind(words_no_encoding, all_noneng_words_refine) %>%
  mutate(eng_C = hunspell::hunspell_check(Captures, hunspell::dictionary("en_US")),
         eng_l = hunspell::hunspell_check(Word, hunspell::dictionary("en_US")))

new_product_refine <- refine_captures(word_vec = new_product_words$low)
new_product_words <- cbind(new_product_words, new_product_refine) %>%
  select(-Word) %>% mutate(eng_C = hunspell::hunspell_check(Captures, hunspell::dictionary("en_US")),
                             eng_l = hunspell::hunspell_check(low, hunspell::dictionary("en_US")))

launch_proximity_refine <- refine_captures(word_vec = launch_proximity_words$low)
launch_proximity_words <- cbind(launch_proximity_words, launch_proximity_refine) %>%
  select(-Word) %>% mutate(eng_C = hunspell::hunspell_check(Captures, hunspell::dictionary("en_US")),
                           eng_l = hunspell::hunspell_check(low, hunspell::dictionary("en_US")))

all_noneng_words %>% filter(eng_C == FALSE &  xml == FALSE & dates == FALSE & pharma == FALSE) %>% .$Word %>% unique()
launch_proximity_words %>% filter(eng_C == FALSE &  xml == FALSE & dates == FALSE & pharma == FALSE) %>% .$low %>% unique()
new_product_words %>% filter(eng_C == FALSE &  xml == FALSE & dates == FALSE & pharma == FALSE) %>% .$low %>% unique()

####
####
all_noneng_words <- all_noneng_words %>% mutate(tick = check_ticker(Word, "TF"), comp_name = check_company_name(Word, "TF", "low"))
launch_proximity_words <- launch_proximity_words %>% mutate(tick = check_ticker(low, "TF"), comp_name = check_company_name(low, "TF", "low"))
new_product_words <- new_product_words %>% mutate(tick = check_ticker(low, "TF"), comp_name = check_company_name(low, "TF", "low"))

all_noneng_prods <- possible_products(all_noneng_words)
launch_proximity_prods <- possible_products(launch_proximity_words)
launch_proximity_prods %>% .$Captures %>% unique
all_noneng_prods %>% select(-Company, -Month, -Year) %>% distinct()

####
####
####
####
####

new_product_words <- new_product_words %>%
  mutate(english = hunspell::hunspell_check(low, hunspell::dictionary("en_US")),
         tickerYN = check_ticker(word_vec = low, mode = "TF"),
         compYN = check_company_name(low, mode = "TF", case = "low"),
         brand = stringr::str_extract(Captures, "®|™"))

new_product_words <- new_product_words %>% mutate(finYN = check_financial(word_vec = low, mode = "TF"))
new_product_words <- new_product_words %>% mutate(xmlYN = check_xmlhtml(word_vec = low, mode = "TF"))


new_product_words %>% filter(xmlYN == TRUE) %>% .$low %>% unique

refine_captures("Devika")
refine_captures("cashback")
refine_captures("bioequivalent")
refine_captures("nonvested")


