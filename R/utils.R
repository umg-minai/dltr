#' Extract units
#'
#' Extract units of values from column names.
#' Column names have the form of "FOO BAR [UNIT]".
#'
#' @param x `character`, column names
#' @return `character`, units
#' @noRd
.extract_units_from_column_name <- function(x) {
    x[!grepl("[", x, fixed = TRUE)] <- NA
    sub("^.*\\[([^]]*)\\]$", "\\1", x)
}

#' Drop units
#'
#' Drop units of values from column names.
#' Column names have the form of "FOO BAR [UNIT]".
#'
#' @param x `character`, column names
#' @return `character`, units
#' @noRd
.drop_units_from_column_name<- function(x) {
    sub(" *\\[.*$", "", x)
}
