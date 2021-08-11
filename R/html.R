html_cocktail <- function(x, tags = character(), ingredients = character(), classes = character()) {
  html$article(
    class = paste0(c("cocktail", classes), collapse = " "),
    id = slug(x$title),

    # bar_ingredients(x$ingredients),
    html$div(class = "title",
      html$h2(html$a(x$title, href = paste0("#", slug(x$title)))),
      html_tags(x$tags, tags)
    ),
    html_ingredients(x$ingredients, ingredients),

    if (!is.null(x$notes)) html$p(class = "notes", x$notes),
    html_source(x$source),
  )

}
html_ingredients <- function(x, ingredients) {
  x %>%
    map(html_ingredient, ingredients) %>%
    map(html$li) %>%
    html$ul(.)
}

html_source <- function(x) {
  if (is.null(x)) return()

  if (grepl("^http", x)) {
    host <- httr::parse_url(x)$hostname
    html$p(class = "source", "Source:", html$a(href = x, paste0("<", host, ">")))
  } else {
    html$p(class = "source", "Source:", x)
  }
}

html_ingredient <- function(x, ingredients) {
  if (is_string(x)) {
    html_escape(x)
  } else if (length(x) %in% c(2, 3)) {
    quantity <- quantity(x[[1]])
    primary <- link_page(x[[2]], "ingredient", ingredients)
    modifier <- if (length(x) == 3) html_escape(x[[3]])

    HTML(paste0(quantity, " ", primary, if (!is.null(modifier)) ", ", modifier))
  } else {
    stop_invalid(x)
  }
}

html_tags <- function(x, tags) {
  if (!is.character(x) && !is.null(x)) {
    stop_invalid(x)
  }
  x <- intersect(x, tags)
  if (length(x) == 0) {
    return()
  }

  links <- map(x, link_page, prefix = "tag", valid = tags)
  block <- HTML(paste0(map_chr(links, as.character), collapse = ", "))
  html$p(class = "tags", block)
}

link_page <- function(x, prefix, valid) {
  if (x %in% valid) {
    html$a(href = paste0(prefix, "-", slug(x), ".html"), x)
  } else {
    html_escape(x)
  }
}

