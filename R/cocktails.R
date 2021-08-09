ingredients <- function(x) {
  x$ingredients %>%
    keep(~ is.list(.x)) %>%
    map_chr(~ .x[[2]]) %>%
    unique()
}

quantity <- function(x) {
  int <- as.integer(x)
  dec <- sprintf("%.2f", x - int)

  if (int == 0 && dec == "0.10") {
    return("1 bar spoon")
  }

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
