#' Read Draeger Logbook and Trend data.
#'
#' @param file `character(1)`, file path to logbook or trend data.
#'
#' @import data.table
#' @export
#' @examples
#' f <- system.file("logbook.txt", package = "dltr")
#' lb <- read_logbook(f)
#' lb
read_dltr <- function(file) {
    if (!file.exists(file))
        stop(file, " doesn't exist")

    if (!is_logbook_file(file))
        stop("Currently only logbook files are supported")

    .reshape_logbook(file)
}

#' @export
#' @rdname read_dltr
read_logbook <- read_dltr

#' Test Logbook and Trend file format
#'
#' @inheritParams read_dltr
#' @return `logical(1)`, `TRUE` if the value is readable and has the correct
#' header (first row).
#'
#' @examples
#' is_logbook_file(system.file("logbook.txt", package = "dltr"))
#' is_logbook_file(system.file("trends.txt", package = "dltr"))
#' @export
is_logbook_file <- function(file) {
    l <- suppressWarnings(readLines(file, n = 1L))
    isTRUE(substring(l, 1, 41) == "Date / Time,Label,Old value,Current value")
}

#' @export
#' @rdname is_logbook_file
is_trends_file <- function(file) {
    l <- suppressWarnings(readLines(file, n = 1L))
    isTRUE(substring(l, 1, 7) == ",,etCO2")
}

