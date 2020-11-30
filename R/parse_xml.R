

html_text_collapse <- function(x, trim = FALSE, collapse = "\n"){
  UseMethod("html_text_collapse")
}

html_text_collapse.xml_nodeset <- function(x, trim = FALSE, collapse = "\n"){
  vapply(x, html_text_collapse.xml_node, character(1), trim = trim, collapse = collapse)
}

html_text_collapse.xml_node <- function(x, trim = FALSE, collapse = "\n"){
  paste(xml2::xml_find_all(x, ".//text()"), collapse = collapse)
}

replace_non_ascii2 <- function (x, replacement = '',  ...) {
  stringi::stri_replace_all_regex(x, '[^ -~®™]+', replacement = replacement)
}

parse_xml <- function(xml_string, ascii) {
  xml_string <- xml_string %>%
    xml2::read_html() %>%
    html_text_collapse()

  if(ascii == FALSE) {
    xml_string <- xml_string %>% replace_non_ascii2(replacement = " ")
  }

  xml_string

}

