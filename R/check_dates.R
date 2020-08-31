

check_dates <- function(word_vec) {
  year = "(19|20)[0-9][0-9]"
  month = "[0-1]?[0-9]"
  date = "[0-3]?[0-9]"
  separators = "(\\s|-|\\/)?"

  number_date_patt = paste(paste0(year, separators,  month, separators, date),
                            paste0(year, separators,  date,separators, month),
                            paste0(month, separators, date, separators, year),
                            paste0(month, separators, year, separators, date),
                            paste0(date, separators,  month, separators, year),
                            paste0(date, separators, year, separators, month),
                            sep = "|")

  month_abbr <- "(jan|feb|mar|apr|may|jun|jul|aug|sept?|oct|nov|dec)"
  abbr_patt <- paste(paste0(year, separators, month_abbr, separators, date),
                      paste0(year, separators, date, separators, month_abbr),
                      paste0(month_abbr,separators, date, separators, year),
                      paste0(month_abbr,separators, year, separators, date),
                      paste0(date, separators,  month_abbr, separators, year),
                      paste0(date, separators, year, separators, month_abbr),
                      paste0(month_abbr, separators, date),
                      paste0(date, separators, month_abbr),
                      sep = "|"
                      )

  month_patt <- "january|february|april|june|july|august|september|october|november|december"
  month_exception_patt <-  "\\bmay\\s?\\d|\\bmar[^A-Za-z]"

  # date_patt <- paste0(number_date_patt, abbr_patt, month_patt, month_exception_patt, collapse = "|")
  # dates_YN <- stringr::str_detect(word_vec, pattern = date_patt)

  dates_YN <- vector(length = length(word_vec))

  for (i in 1:length(word_vec)) {
    dates_YN[i] <- stringr::str_detect(word_vec[i], number_date_patt)

    if(dates_YN[i] == FALSE){
      dates_YN[i] <- stringr::str_detect(word_vec[i], abbr_patt)
    }

    if(dates_YN[i] == FALSE){
      dates_YN[i] <- stringr::str_detect(word_vec[i], month_patt)
    }

    if(dates_YN[i] == FALSE){
      dates_YN[i] <- stringr::str_detect(word_vec[i], month_exception_patt)
    }

  }

  dates_YN

}


# check_dates("06191990")
# check_dates("06-19-1990")
# check_dates("1990 06 19")
# check_dates("jan 07")
#
# check_dates("MAR-19-1990") # it thinks 1 = month, 9 = day and year = 1990
# check_dates("XXX-19-1990")
#
# check_dates("january")
# check_dates("march on washington")

