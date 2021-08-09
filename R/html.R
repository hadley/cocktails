html_site <- function(dir = "~/desktop/test") {
  dir.create(dir, showWarnings = FALSE)

  tags <- at_least(unlist(map(cocktails, ~ .x$tags)), 2)
  ingredients <- at_least(unlist(map(cocktails, ingredients)), 2)

  # Home page (all cocktails for now)
  write_page(cocktails, file.path(dir, "index.html"),
    tags = tags,
    ingredients = ingredients
  )

  # Tag pages
  for (tag in tags) {
    filtered <- cocktails %>% keep(~ tag %in% .x$tags)
    write_page(filtered, file.path(dir, paste0("tag-", slug(tag), ".html")),
      tags = setdiff(tags, tag),
      ingredients = ingredients
    )
  }

  # Ingredient pages
  for (ingredient in ingredients) {
    filtered <- cocktails %>% keep(~ ingredient %in% ingredients(.x))
    write_page(filtered, file.path(dir, paste0("ingredient-", slug(ingredient), ".html")),
      tags = tags,
      ingredients = setdiff(ingredients, ingredient)
    )
  }

}

at_least <- function(x, n) {
  tb <- table(x)
  names(tb)[tb >= n]
}


ingredients <- function(x) {
  x$ingredients %>%
    keep(~ is.list(.x)) %>%
    map_chr(~ .x[[2]])
}

write_page <- function(x, path, title = "Cocktails", tags = character(), ingredients = character()) {
  template <- readLines(system.file("templates/page.html", package = "cocktails"))

  titles <- map_chr(x, ~ .x$title)
  cocktails <- map(x[order(titles)], html_cocktail, tags = tags, ingredients = ingredients)
  body <- html$div(class = "cocktails", cocktails)

  rendered <- whisker::whisker.render(template, list(title = title, body = body))
  writeLines(rendered, path)
  invisible(path)
}

html_cocktail <- function(x, tags = character(), ingredients = character()) {
  html$article(
    class = "cocktail",
    id = slug(x$title),

    # bar_ingredients(x$ingredients),
    html$h1(html$a(x$title, href = paste0("#", slug(x$title)))),
    html_tags(x$tags, tags),
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
    html$p(class = "source", html$a(href = x, x))
  } else {
    html$p(class = "source", x)
  }
}

html_ingredient <- function(x, ingredients) {
  if (is_string(x)) {
    html_escape(x)
  } else if (is.list(x) && length(x) %in% c(2, 3)) {
    quantity <- html_quantity(x[[1]])
    primary <- link_page(x[[2]], "ingredient", ingredients)
    modifier <- if (length(x) == 3) html_escape(x[[3]])

    HTML(paste0(quantity, " ", primary, if (!is.null(modifier)) ", ", modifier))
  } else {
    stop_invalid(x)
  }
}

html_tags <- function(x, tags) {
  if (is.null(x)) return()
  if (!is.character(x)) {
    stop_invalid(x)
  }
  links <- map(x, link_page, prefix = "tag", valid = tags)
  block <- HTML("[", paste0(map_chr(links, as.character), collapse = ", "), "]")
  html$p(class = "tags", block)
}

link_page <- function(x, prefix, valid) {
  if (x %in% valid) {
    html$a(href = paste0(prefix, "-", slug(x), ".html"), x)
  } else {
    html_escape(x)
  }
}

html_quantity <- function(x) {
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
