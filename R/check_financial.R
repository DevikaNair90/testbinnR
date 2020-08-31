# fin_terms <- readxl::read_excel("/project/biocomplexity/sdad/projects_data/ncses/bi/binn/original/investopedia_financial_terms_march122020.xlsx")
#
# fin <- unlist(strsplit(fin_terms$Term, "\\s"))
# fin <- str_replace_all(fin, pattern = "(?<=\\d\\d)-(?=[:alpha:])", replacement = "")
# fin <- str_replace_all(fin, pattern = "[[:punct:]]", replacement = " ")
# fin <- str_squish(unlist(strsplit(fin, "\\s")))
# fin <- str_to_lower(fin[nchar(fin) >2 & str_detect(fin, "[[:alpha:]]") == TRUE & hunspell::hunspell_check(fin) == FALSE])
#
# fin_manual <- c("nonvested", "unvested", "undiscounted", "payor", "nonoperating", "noncash", "indemnitee", "(non)?cancelable", "cancelable", "joinder", "uspto", "nonemployee", "recordkeeping", "incurrence", "iso4217", "level[123]", "unaudited", "chargeback", "commercialisation", "enforceability", "expensed", "collectability", "expirations", "depreciable", "sublicense", "saleable", "forecasted", "amortised", "exercisable", "deliverables", "ratably", "annum", "issuable", "extinguishment", "forma", "issuance", "financing", "licensor", "reportable", "authorisation", "reclassification", "counterparties", "payable", "counterparty", "tolerability", "depositary", "sublicensee", "volatilities", "remeasurement", "realizability", "investee", "programme", "rentable", "licence", "acquiree", "allocable", "withholding", "litigation", "unremitted", "transferee", "unpatented", "sublicensed", "resubmission", "evaluable", "bona", "patentable", "registrable", "judgement", "redemption", "exclusivity", "exclusivities", "amortisation", "deductibility", "cyber", "reorganisation", "ratable", "exercisability", "participations", "undesignated", "overpayment", "anticompetitive", "favour", "favourable", "monetization", "dilutive", "accreted", "authorised", "assignee", "certiorari", "transferor", "capitalised", "noteholder", "longstop", "tradable", "commercialise", "appealable", "noninfringement", "translational", "externalisation", "transactional", "patentee", "authorise", "reacquisition", "betterment")
#
# fin_eng_terms <- c("cancelled", "recognised", "endeavours", "cancellable", "premarket", "centre", "undrawn", "acknowledgement", "preliminarily", "amongst", "transferability", "durations", "onwards", "scrollbars", "minimise", "memoranda", "recognises", "totalling", "fulfil", "centres", "underperformance", "realised")
#
# fin <- c(fin, fin_manual, fin_eng_terms, fin_plural)
# fin_plural <- paste0(fin, "s")
# saveRDS(fin , "data/financial_terms.RDS")

check_financial <- function(word_vec, mode) {
  fin <- readRDS("data/financial_terms.RDS")

  if(mode == "TF") {
    fin_TF <- isTRUE(word_vec %in% fin|word_vec %in% paste0(fin, "s"))
  }

  fin_TF

}

