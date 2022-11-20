at_least <- function(x, n) {
  tb <- table(x)
  names(tb)[tb >= n]
}


slug <- function(x) {
  x <- tolower(x)
  x <- gsub("'", "", x)
  x <- gsub("[^A-Za-z0-9]+", "-", x)
  x <- gsub("-+", "-", x)
  x <- gsub("^-|-$", "", x)
  x
}

stop_invalid <- function(x) {
  abort(c("Ill-formed ingredient", utils::capture.output(utils::str(x))))
}
html_escape <- function(x) htmltools::htmlEscape(x)
HTML <- function(...) htmltools::HTML(paste0(...))

first_upper <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
