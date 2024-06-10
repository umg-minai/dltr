#' @param file `character(1)`, file path to logbook or trend data.
#' @importFrom lubridate dmy_hms
#' @noRd
.read_logbook <- function(file) {
    if (!file.exists(file))
        stop(file, " doesn't exist")

    r <- readLines(file)
    ## fix misformated lines
    r <- gsub(
        "Hose connected to a wrong port, leakage, or condensate in the hoses.",
        "'Hose connected to a wrong port, leakage, or condensate in the hoses.'",
        r
    )
    d <- fread(text = r, quote = "'")
    setnames(d,
        c("Date / Time", "Old value", "Current value", "Unit of value"),
        c("DateTime", "Old", "Current", "Unit")
    )
    d[, DateTime := dmy_hms(DateTime)]
}

#' @param x `data.table`, logbook data
#' @return `data.table`, extracted logbook values
#' @noRd
.reshape_logbook <- function(file) {
    lb <- .read_logbook(file)
    lb <- rbindlist(
        list(
            .start_therapy(lb),
            .stop_therapy(lb),
            .ventilation_settings(lb),
            .melt_fgf(lb),
            .vaporizer_settings(lb),
            .melt_measurements(lb)
        )
    )[order(DateTime),]
    lb[, Current := as.double(Current)]
    add_anaesthesia_case_id(lb)
    lb[!is.na(CaseId) & CaseId > 0,]
}

#' Find values
#'
#' Look up values in the logbook for a given time point relative to a reference.
#'
#' @param x `data.table`, logbook data
#' @param label `character(1)`, LABEL of value, e.g. PEEP.
#' @param time `numeric(1)`, time from reference in minutes.
#' @param reference `character(1)`, reference time point.
#' @return `double`, of value `label`
#' @importFrom lubridate minutes
#' @export
#' @examples
#' f <- system.file("logbook.txt", package = "dltr")
#' lb <- read_logbook(f)
#' value_at(lb, "FGF", time = 15, reference = "start")
value_at <- function(x, label, time,
                     reference = c(
                        "start", "vaporizer-opening",
                        "mechanical-ventilation"
                    )) {
    reference <- match.arg(reference)
    r <- .reference_time(x, reference)
    r[, ReferenceTime := ReferenceTime + minutes(time)]
    m <- merge(x, r)
    m[,
        .SD[Label == label & DateTime <= ReferenceTime, Current[.N]],
        by = CaseId
    ]
}

#' @inheritParams value_at
#' @noRd
.reference_time <- function(x, reference = c(
                            "start", "vaporizer-opening",
                            "mechanical-ventilation"
                            )) {
    ref_label <- switch(
        match.arg(reference),
        "start" = "Start of therapy",
        "vaporizer-opening" = "Vaporizer setting",
        "mechanical-ventilation" = "Ventilation settings"
    )
    x[,
      .(ReferenceTime =
        .SD[Label == ref_label & !Current %in% c(0, 9), DateTime[1]]),
      by = CaseId]
}

#' Filter cases
#'
#' @param x `data.table`, logbook data.
#' @param min_duration `numeric(1)`, minimal accepted duration in minutes.
#'
#' @return `data.table`, filtered cases.
#'
#' @export
#' @examples
#' f <- system.file("logbook.txt", package = "dltr")
#' lb <- read_logbook(f)
#' filter_short_cases(lb)
#' filter_short_cases(lb, min_duration = 60)
filter_short_cases <- function(x, min_duration = 10) {
    x[rep(case_duration(x) >= min_duration, x[, .N, by = CaseId][, N]),]
}

#' Case times
#'
#' Start/Stop and duration of anesthesia cases.
#'
#' @param x `data.table`, logbook data
#'
#' @return `double`, duration in minutes.
#' @export
#' @examples
#' f <- system.file("logbook.txt", package = "dltr")
#' lb <- read_logbook(f)
#' case_duration(lb)
case_duration <- function(x) {
    x[Label == "Case duration", Current]
}

#' @param reference `character(1)`, reference time point.
#' @return `POSIXct`, case_start times
#' @export
#' @rdname case_duration
#' @examples
#' case_start(lb)
#' case_start(lb, reference = "mechanical-ventilation")
case_start  <- function(x, reference = c(
                   "start", "vaporizer-opening",
                   "mechanical-ventilation"
                   )) {
    .reference_time(x, reference)[, ReferenceTime]
}

#' @return `POSIXct`, end times
#' @export
#' @rdname case_duration
#' @examples
#' case_end(lb)
case_end <- function(x) {
    x[Label == "Case duration", DateTime]
}

#' Volatile anaesthetics
#'
#' Are volatile anesthetics used?
#'
#' @param x `data.table`, logbook data
#' @return `logical`, volatile anesthetic used?
#' @export
#' @examples
#' f <- system.file("logbook.txt", package = "dltr")
#' lb <- read_logbook(f)
#' is_volatile_anesthesia(lb)
is_volatile_anesthesia <- function(x) {
    # we require endtital sevoflurane concentration and
    # a vaporizer setting > 0.0 (otherwise an agent could be introduced from the
    # induction machine/ICU)
    x[,
        any(.SD[Label == "prim. Agent exp", any(Current > 0)]) &
        any(.SD[Label == "Vaporizer setting", any(Current > 0)]),
        by = CaseId
    ][, V1]
}

#' Case Id
#'
#' Add an additional column with a corresponding case id.
#'
#' @param x `data.table`, logbook data
#' @return `data.table`, with additional CaseId column.
#' @export
add_anaesthesia_case_id <- function(x) {
    setorder(x, DateTime)
    x[, CaseId := cumsum(Label == "Start of therapy")]
    x[, CaseId := fifelse(
        DateTime > .SD[Label == "Case duration", DateTime], NA, CaseId),
        by = CaseId
    ]
}

#' @param x `data.table`, logbook data
#' @return `integer`, row indices
#' @noRd
.fgf_index <- function(x) {
    x[Label == "Gas settings" | Label == "FG flow", which = TRUE]
}

#' @param x `data.table`, logbook data
#' @return `integer`, row indices
#' @noRd
.fio2_index <- function(x) {
    x[Label == "Gas settings" | Label == "O2", which = TRUE]
}


#' @param x `data.table`, logbook data
#' @return `data.table`, extracted ventilator mode settings
#' @noRd
.start_therapy <- function(x) {
    x <- x[Label == "Start of therapy", .(DateTime, Label, Current, Unit)]
    x[,
        Current := fcase(
            Current == "New neonate", 1.0,
            Current == "New pediatric", 2.0,
            Current == "New adult", 3.0
        )
    ]
    x
}

#' @param x `data.table`, logbook data
#' @return `data.table`, extracted ventilator mode settings
#' @importFrom lubridate hm
#' @noRd
.stop_therapy <- function(x) {
    dt <- x[Label == "Case duration", DateTime]
    x <- x[
        DateTime %in% dt &
            Label %in% c(
                "Case duration",
                "Consumption O2",
                "Consumption Air",
                "Consumption Sev",
                "Uptake Sev"
            ),
        .(DateTime, Label, Current, Unit)
    ]
    x[Label == "Case duration", Current := as.double(hm(Current)) / 60]
    x
}

#' @param x `data.table`, logbook data
#' @return `integer`, row indices
#' @noRd
.vaporizer_setting_index <- function(x) {
    x[Label == "Vaporizer setting", which = TRUE]
}

#' @param x `data.table`, logbook data
#' @return `data.table`, extracted vaporizer settings
#' @noRd
.vaporizer_settings <- function(x) {
    x <- x[Label == "Vaporizer setting", .(DateTime, Label, Current, Unit)]
    x[,
        `:=` (
            Current = as.double(sub("---|T", "", Current)),
            Unit = "Vol%"
        )
    ]
    x[!is.na(x$Current),]
}

#' @param x `data.table`, logbook data
#' @return `data.table`, extracted ventilator mode settings
#' @noRd
.ventilation_settings <- function(x) {
    x <- x[Label == "Ventilation settings", .(DateTime, Label, Current, Unit)]
    x[,
        Current := fcase(
            Current == "MAN/SPON", 0.0,
            Current == "Press.Ctrl.", 1.0,
            Current == "Press.Supp.", 2.0,
            Current == "Vol.Ctrl.AutoFl.", 3.0,
            Current == "Vol.Ctrl.", 4.0,
            Current == "Pause", 9.0
        )
    ]
    x
}

#' @param x `data.table`, logbook data
#' @return `data.table`, long format
#' @noRd
.melt_fgf <- function(x) {
    m <- melt(
        x[
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

#' @param x `data.table`, logbook data
#' @return `data.table`, long format
#' @noRd
.melt_measurements <- function(x) {
    measure.vars <- colnames(x)[
        grepl("^etCO2|^MV|^Pmean|^PIP|^PP|^PEEP|^FiO|^prim.", colnames(x))
    ]
    x[,
        (measure.vars) :=
            lapply(.SD, function(x)as.double(sub("ERR|\\+\\+\\+", "", x))),
        .SDcols = measure.vars
    ]
    m <- melt(
        x[Label == "Measurements", c("DateTime", measure.vars), with = FALSE],
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
