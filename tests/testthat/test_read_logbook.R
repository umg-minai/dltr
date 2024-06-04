test_that(".read_logbook throws errors", {
    expect_error(.read_logbook("foo.bar"), "doesn't exist")
})

test_that(".read_logbook", {
    lb <- .read_logbook(test_path("..", "..", "inst", "logbook.txt"))

    expect_true(is.data.table(lb))

    expect_s3_class(lb$DateTime, "POSIXct")
})

test_that(".start_therapy_index", {
    lb <- data.table(
        Label = c("foo", "Start of therapy", "bar", "Start of therapy")
    )
    expect_in(.start_therapy_index(lb), c(2, 4))
})

test_that(".stop_therapy_index", {
    lb <- data.table(
        Label = c(
            "System state changed", "System state changed", "bar",
            "System state changed"
        ),
        Old = c("Operation", "Operation", "bar", "Operation"),
        Current = c("bar", "Standby", "Standby", "Standby")
    )
    expect_in(.stop_therapy_index(lb), c(2, 4))
})

test_that(".fgf_index", {
    lb <- data.table(Label = c("Gas settings", "foo", "bar", "FG flow"))
    expect_in(.fgf_index(lb), c(1, 4))
})

test_that(".melt_fgf", {
    lb <- data.table(
        DateTime = lubridate::ymd_hm(
            c(rep("2024-06-04 12:45", 3), "2024-06-04 13:00")
        ),
        Label = c("Gas settings", "foo", "bar", "FG flow"),
        Current = c(NA, NA, "1", "0.5"),
        `Set FG flow [l/min]` = c(15, NA, NA, NA)
    )
    r <- data.table(
        DateTime = lubridate::ymd_hm(c("2024-06-04 12:45", "2024-06-04 13:00")),
        Label = rep("FGF", 2),
        Current = c(15, 0.5),
        Unit = c("l/min", "l/min")
    )
    expect_equal(.melt_fgf(lb), r)
})

test_that(".fio2_index", {
    lb <- data.table(Label = c("Gas settings", "foo", "bar", "O2"))
    expect_in(.fio2_index(lb), c(1, 4))
})

test_that(".vaporizer_setting_index", {
    lb <- data.table(
        Label = c("Vaporizer setting", "foo", "bar", "Vaporizer setting")
    )
    expect_in(.vaporizer_setting_index(lb), c(1, 4))
})

test_that(".melt_measurements", {
    lb <- data.table(
        DateTime = lubridate::ymd_hm(
            c(rep("2024-06-04 12:45", 3), "2024-06-04 13:00")
        ),
        Label = c("Measurements", "foo", "bar", "Measurements"),
        `etCO2 [mmHg]` = c(35, NA, NA, 37),
        `MV [l/min]` = c(5.0, NA, NA, 4.8)
    )
    r <- data.table(
        DateTime = lubridate::ymd_hm(
            rep(c("2024-06-04 12:45", "2024-06-04 13:00"), each = 2)
        ),
        Label = rep(c("etCO2", "MV"), 2),
        Current = c(35, 5.0, 37, 4.8),
        Unit = rep(c("mmHg", "l/min"), 2)
    )
    expect_equal(.melt_measurements(lb), r)
})

