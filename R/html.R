html_cocktail <- function(x) {
  html$div(
    class = "cocktail",
    id = slug(x$title),

    # bar_ingredients(x$ingredients),
    html$h1(x$title),
    html_tags(x$tags),
    html_ingredients(x$ingredients),
    #
    if (!is.null(x$notes)) html$p(class = "notes", x$notes),
    html_source(x$source),
  )

}
html_ingredients <- function(x) {
  x %>%
    map(html_ingredient) %>%
    map(html$li) %>%
    html$ul(.)
}

html_source <- function(x) {
  if (is.null(x)) return()

  if (grepl("^http", x)) {
    html$p(class = "source", html$a(href = x, x))
  } else {
    html$p(class = "source", x)
  }
}

html_ingredient <- function(x) {
  if (is_string(x)) {
    html_escape(x)
  } else if (is.list(x) && length(x) %in% c(2, 3)) {
    quantity <- html_quantity(x[[1]])
    primary <- link_ingredient(x[[2]])
    modifier <- if (length(x) == 3) html_escape(x[[3]])

    HTML(paste0(quantity, " oz ", primary, if (!is.null(modifier)) ", ", modifier))
  } else {
    abort(c("Ill-formed ingredient", capture.output(str(x))))
  }
}

html_ingredient <- function(x) {
  if (is_string(x)) {
    html_escape(x)
  } else if (is.list(x) && length(x) %in% c(2, 3)) {
    quantity <- html_quantity(x[[1]])
    primary <- link_page(x[[2]], "ingredient")
    modifier <- if (length(x) == 3) html_escape(x[[3]])

    HTML(paste0(quantity, " oz ", primary, if (!is.null(modifier)) ", ", modifier))
  } else {
    stop_invalid(x)
  }
}

html_tags <- function(x) {
  if (is.null(x)) return()
  if (!is.character(x)) {
    stop_invalid(x)
  }
  links <- map(x, link_page, prefix = "tag")
  block <- HTML("[", paste0(map_chr(links, as.character), collapse = ", "), "]")
  html$p(class = "tags", block)
}

link_page <- function(x, prefix) {
  html$a(href = paste0("/", prefix, "/", slug(x)), x)
}

html_quantity <- function(x) {
  int <- as.integer(x)
  dec <- sprintf("%.2f", x - int)

  frac <- switch(dec,
    "0.25" = "\u00bc",
    "0.50" = "\u00bd",
    "0.75" = "\u00be",
    NULL
  )
  if (is.null(frac)) {
    as.character(x)
  } else {
    if (int > 0) {
      paste0(int, frac)
    } else {
      frac
    }
  }
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
  abort(c("Ill-formed ingredient", capture.output(str(x))))
}
html_escape <- function(x) htmltools::htmlEscape(x)
HTML <- function(...) htmltools::HTML(paste0(...))
