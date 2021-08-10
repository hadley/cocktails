preview_site <- function() {
  build_site()
  if (is.null(getOption("cocktails_server"))) {
    options(cocktails_server = servr::httw(system.file("docs", package = "cocktails")))
  }
}

build_site <- function(dir = "docs") {
  dir.create(dir, showWarnings = FALSE)
  file.copy(system.file("templates/cocktails.css", package = "cocktails"), dir, overwrite = TRUE)

  tags <- sort(at_least(unlist(map(cocktails, ~ .x$tags)), 2))
  ingredients <- sort(at_least(unlist(map(cocktails, ingredients)), 2))

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
      ingredients = ingredients,
      title = first_upper(tag)
    )
  }

  # Ingredient pages
  for (ingredient in ingredients) {
    filtered <- cocktails %>% keep(~ ingredient %in% ingredients(.x))
    write_page(filtered, file.path(dir, paste0("ingredient-", slug(ingredient), ".html")),
      tags = tags,
      ingredients = setdiff(ingredients, ingredient),
      title = first_upper(ingredient)
    )
  }
}

write_page <- function(x, path, title = "Cocktails", tags = character(), ingredients = character()) {
  heading <- html$header(
    html$h1(title),
    html$p(html$small(html$a(href = "/", "home")))
  )
  titles <- map_chr(x, ~ .x$title)
  cocktails <- map(x[order(titles)], html_cocktail, tags = tags, ingredients = ingredients)

  blocks <- list(heading, cocktails)
  write_page_blocks(blocks, path, title)
}

write_page_blocks <- function(blocks, path, title) {
  body <- html$div(class = "cocktails", blocks)

  template <- readLines(system.file("templates/page.html", package = "cocktails"))
  rendered <- whisker::whisker.render(template, list(title = title, body = body))
  writeLines(rendered, path)
  invisible(path)
}
