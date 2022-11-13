ingredients <- function(x) {
  x$ingredients %>%
    keep(~ length(.x) >= 2) %>%
    map_chr(~ .x[[2]]) %>%
    unique()
}

quantity <- function(x, fraction_type = "unicode") {
  if (is.character(x)) {
    return(x)
  }

  int <- as.integer(x)
  dec <- sprintf("%.2f", x - int)
  frac <- fraction(dec, fraction_type)

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

fraction <- function(x, type = c("unicode", "vulgar")) {
  type <- arg_match(type)

  if (type == "unicode") {
    switch(x,
      "0.25" = "\u00bc",
      "0.50" = "\u00bd",
      "0.75" = "\u00be",
      NULL
    )
  } else {
    switch(x,
      "0.25" = " 1/4",
      "0.50" = " 1/2",
      "0.75" = " 3/4",
      NULL
    )
  }
}
