#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom glue glue
#' @importFrom magrittr %>%
## usethis namespace: end
NULL

cocktails <- yaml::read_yaml(system.file("extdata", "cocktails.yml", package = "cocktails"))
book22 <- yaml::read_yaml(system.file("extdata", "book-22.yml", package = "cocktails"))
