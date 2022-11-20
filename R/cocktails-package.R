## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
## usethis namespace: end
NULL

#' Cocktails data
#'
#' @export
cocktails <- NULL
html <- NULL

.onLoad <- function(...) {
  html <<- htmltools::tags
  cocktails <<- yaml::read_yaml(system.file("extdata", "cocktails.yml", package = "cocktails"))
}
