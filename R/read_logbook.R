#' @importFrom lubridate dmy_hms
.read_logbook <- function(file) {
    if (!file.exists(file))
        stop(file, " doesn't exist")

    d <- fread(file)
    setnames(d,
        c("Date / Time", "Old value", "Current value", "Unit of value"),
        c("DateTime", "Old", "Current", "Unit")
    )
    d[, DateTime := dmy_hms(DateTime)]
}

.reshape_logbook <- function(file) {
    lb <- .read_logbook(file)
    .melt_fgf(lb)
}

#' @param lb `data.table`, logbook data
#' @return `integer`, row indices
#' @noRd
.start_therapy_index <- function(lb) {
    lb[Label == "Start of therapy", which = TRUE]
}

#' @param lb `data.table`, logbook data
#' @return `integer`, row indices
#' @noRd
.stop_therapy_index <- function(lb) {
    lb[
        Label == "System state changed" & Old == "Operation" &
            Current == "Standby",
        which = TRUE
    ]
}

#' @param lb `data.table`, logbook data
#' @return `integer`, row indices
#' @noRd
.fgf_index <- function(lb) {
    lb[Label == "Gas settings" | Label == "FG flow", which = TRUE]
}

#' @param lb `data.table`, logbook data
#' @return `integer`, row indices
#' @noRd
.fio2_index <- function(lb) {
    lb[Label == "Gas settings" | Label == "O2", which = TRUE]
}

#' @param lb `data.table`, logbook data
#' @return `integer`, row indices
#' @noRd
.vaporizer_setting_index <- function(lb) {
    lb[Label == "Vaporizer setting", which = TRUE]
}

#' @param lb `data.table`, logbook data
#' @return `data.table`, long format
#' @noRd
.melt_fgf <- function(lb) {
    m <- melt(
        lb[
            Label == "Gas settings" | Label == "FG flow",
        ][, `:=`
            (
                Current = as.double(Current),
                `Set FG flow [l/min]` = as.double(`Set FG flow [l/min]`)
            )
        ],
        id.vars = "DateTime",
        measure.vars = c("Current", "Set FG flow [l/min]"),
        variable.name = "Label",
        variable.factor = FALSE,
        value.name = "Current",
        na.rm = TRUE
    )[order(DateTime), .(DateTime, Label, Current)]
    m[, `:=` (Label = "FGF", Unit = "l/min")]
    m
}

#' @param lb `data.table`, logbook data
#' @return `data.table`, long format
#' @noRd
.melt_measurements <- function(lb) {
    measure.vars <- colnames(lb)[
        grepl("^etCO2|^MV|^Pmean|^PIP|^PP|^PEEP|^FiO|^prim.", colnames(lb))
    ]
    lb[, (measure.vars) := lapply(.SD, as.double), .SDcols = measure.vars]
    m <- melt(
        lb[Label == "Measurements", c("DateTime", measure.vars), with = FALSE],
        id.vars = "DateTime",
        measure.vars = measure.vars,
        variable.name = "Label",
        variable.factor = FALSE,
        value.name = "Current",
        na.rm = TRUE
    )[order(DateTime),]
    m[, `:=` (
        Label = .drop_units_from_column_name(Label),
        Unit = .extract_units_from_column_name(Label)
    )]
}
