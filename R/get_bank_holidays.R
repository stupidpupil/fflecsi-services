get_bank_holidays <- function(){
  url <- "https://www.gov.uk/bank-holidays/england-and-wales.ics"
  calendar::ic_read(url)
}