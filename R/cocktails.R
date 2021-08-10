ingredients <- function(x) {
  x$ingredients %>%
    keep(~ length(.x) >= 2) %>%
    map_chr(~ .x[[2]]) %>%
    unique()
}

quantity <- function(x) {
  if (is.character(x)) {
    return(x)
  }

  int <- as.integer(x)
  dec <- sprintf("%.2f", x - int)

  frac <- switch(dec,
    "0.25" = "\u00bc",
    "0.50" = "\u00bd",
    "0.75" = "\u00be",
    NULL
  )
  if (is.null(frac)) {
    paste0(x, " oz")
  } else {
    if (int > 0) {
      paste0(int, frac, " oz")
    } else {
      paste0(frac, " oz")
    }
  }
}
