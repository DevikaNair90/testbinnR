# html_entities <- readxl::read_excel("~/W3 plu comments.xlsx", sheet = 1) %>% mutate(string = stringr::str_remove(stringr::str_to_lower(Name), ";")) %>% mutate(eng = hunspell::hunspell_check(string, hunspell::dictionary("en_US"))) %>% filter(eng == FALSE)
# parse_issue_manual <- c("extlink", "hidear", "helvetica", "nowrap", "sgml", "extlink",  "nump", "XBRL", "Arial", "Helvetica", "P10Y", "x2014",  "nbsp", "x2019", "x201...", "CFF0FC", "FFFFFF", "f0f0f0", "D9D9D9", "f0f9ee","ffffff", "P15Y", "P12M", "P30D", "x201C", "x201D", "x2022", "P24M", "innerHTML", "P24M", "P6Y3M", "P5Y6M", "P2Y6M", "xmltruetrueXML", "sgml", "xbrl", "xbrli","html", "cceeff", "D9D9D9", "f0f9ee", "CCEEFF", "Html", "Namespace", "dddddd", "d8d8d8", "CCECFF", "E6E7E8", "ccffcc", "B7DEE8", "cccccc", "c0c0c0", "CCFFCC", "BDD7EE", "bfe4ff", "webkit", "windowtext", "ARIAL", "ROWSHADECOLOR", "xbrldi","nowrap", "Calibri", "nonnum", "http")
# xml <- c(stringr::str_to_lower(html_entities$string), stringr::str_to_lower(parse_issue_manual))
# saveRDS(xml, "data/xmlhtml_terms.RDS")


check_xmlhtml <- function(word_vec, mode){
  xml <- readRDS("data/xmlhtml_terms.RDS")
  xml_patt <- paste0("\\b(", paste0(xml, collapse = "|"), ")\\b")

  if(mode == "TF"){
    xml_TF <- stringr::str_detect(word_vec, xml_patt)
  }

  if(mode == "extract"){
    xml_TF <- stringr::str_extract_all(word_vec, xml_patt)
  }

  xml_TF

}

