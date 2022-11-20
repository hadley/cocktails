#' Format cocktail as markdown
#'
#' @export
#' @keywords internal
md_cocktail <- function(cocktail) {
  ingredients <- map_chr(cocktail$ingredients, md_ingredient, title = cocktail$title)

  paste0(
    "## ", cocktail$title, " \\index[names]{", cocktail$title, "}\n",
    "\n",
    paste0("* ", ingredients, "\n", collapse = ""),
    "\n"
  )
}
md_ingredient <- function(x, title) {
  if (is_string(x)) {
    paste0("_", x, "_")
  } else if (length(x) %in% c(2, 3)) {
    quantity <- quantity(x[[1]], "vulgar")
    primary <- x[[2]]
    modifier <- if (length(x) == 3) x[[3]]

    needs_index <- !primary %in%
      c("Angostura", "lemon juice", "lime juice", "simple syrup",
        "sweet vermouth")

    paste0(
      quantity, " ", primary, if (!is.null(modifier)) ", ", modifier,
      if (needs_index) paste0(" \\index{", primary, "!", title, "}")
    )
  } else {
    stop_invalid(x)
  }
}
