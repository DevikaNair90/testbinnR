
# symbol_dict <- rbind(data.frame(String = c("\u00AE", "®", "&#174;", "&#xae;", "&reg;"),
#                                 Brand = rep("®", 5)),
#                      data.frame(String = c("\u0099", "™", "&#8482;", "&#x2122", "&#153;", "&#x99;", "&trade;", "\u2122"),
#                                 Brand = rep("™", 8)))
#
# symbol_dict$String <- as.character(symbol_dict$String)
# symbol_dict$Brand <- as.character(symbol_dict$Brand)


handle_encoding <- function(xml_string) {
  xml_string <- xml_string %>%
    gsub(pattern = "&nbsp;?", rep = " ")  %>%
    gsub(pattern = "&#60;|&#x3c;|&lt;|\u003C", replacement = "<") %>%
    gsub(pattern = "&#62;|&#x3e;|&gt;|\u003E", replacement = ">") %>%
    gsub(pattern = paste0("<tr> ?",  ".*?</tr>"), replacement = " ") %>%
    gsub(pattern = paste0("<MyReports> ?",  ".*?</MyReports>"), replacement = " ") %>%
    gsub(pattern = "\\b[\\w-]+(\u00AE|®|&#174;|&#xae;|&reg;)|\\b[A-Z][a-z]+\\s[\\w-]+(\u00AE|®|&#174;|&#xae;|&reg;)", replacement = "®") %>%
    gsub(pattern = "\\b[\\w-]+(\u0099|™|&#8482;|&#x2122|&#153;|&#x99;|&trade;|\u2122)|\\b[A-Z][a-z]+\\s[\\w-]+(\u0099|™|&#8482;|&#x2122|&#153;|&#x99;|&trade;|\u2122)",
         replacement = "™")

  xml_string
}
