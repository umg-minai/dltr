test_that(".read_logbook throws errors", {
    expect_error(.read_logbook("foo.bar"), "doesn't exist")
})

test_that(".read_logbook", {
    lb <- .read_logbook(system.file("logbook.txt", package = "dltr"))

    expect_true(is.data.table(lb))

    expect_s3_class(lb$DateTime, "POSIXct")
})

test_that("value_at", {
   lb <- data.table(
        DateTime = lubridate::ymd_hms("2024-06-09 20:00:00") +
            lubridate::minutes(c(1:9, 12)),
        Label = c("Start of therapy", "Ventilation settings",
                  "Vaporizer setting", "Ventilation settings",
                  "PEEP", "FGF", "FGF", "FGF", "Start of therapy", "FGF"),
        Current = c(2, 0, 0.5, 2, 5, 0.5, 0.6, 0.7, 2, 1.5),
        CaseId = c(rep(1, 8), 2, 2)
    )
    expect_equal(value_at(lb, "FGF", 5, "start")[,V1], c(0.5, 1.5))
    expect_equal(value_at(lb, "FGF", 6, "start")[,V1], c(0.6, 1.5))
    expect_equal(value_at(lb, "FGF", 7, "start")[,V1], c(0.7, 1.5))
    expect_equal(value_at(lb, "FGF", 3, "vaporizer-opening")[,V1], 0.5)
    expect_equal(value_at(lb, "FGF", 2, "mechanical-ventilation")[,V1], 0.5)
    expect_equal(value_at(lb, "PEEP", 15, "start")[,V1], 5)
})


test_that(".reference_time", {
   lb <- data.table(
        DateTime = 1:4,
        Label = c("Start of therapy", "Ventilation settings",
                  "Vaporizer setting", "Ventilation settings"),
        Current = c(2, 0, 0.5, 2),
        CaseId = 1
    )
    expect_equal(.reference_time(lb, "start")[, ReferenceTime], 1)
    expect_equal(.reference_time(lb, "vaporizer-opening")[, ReferenceTime], 3)
    expect_equal(
        .reference_time(lb, "mechanical-ventilation")[, ReferenceTime], 4
    )
})

test_that("filter_short_cases", {
    lb <- data.table(
        DateTime = 1:3,
        Label = c("Start of therapy", "bar", "Case duration"),
        Current = c(2, NA, 2),
        CaseId = 1
    )
    expect_equal(filter_short_cases(lb, min_duration = 1), setindex(lb, NULL))
    expect_equal(filter_short_cases(lb, min_duration = 10), lb[0,])
})

test_that("case_duration", {
    lb <- data.table(
        Label = c("foo", "Case duration", "bar"),
        Current = c(NA, 2.5, NA)
    )
    expect_equal(case_duration(lb), 2.5)
})

test_that("case_start", {
    lb <- data.table(
        DateTime = 2:5,
        Label = c("foo", "Start of therapy", "bar", "Start of therapy"),
        Current = 1,
        CaseId = c(1, 2, 2, 3)
    )
    expect_in(case_start(lb), c(NA, 3, 5))
})

test_that("case_end", {
    lb <- data.table(
        DateTime = 2:5,
        Label = c("foo", "Case duration", "bar", "Case duration")
    )
    expect_in(case_end(lb), c(3, 5))
})

test_that("is_volatile_anesthesia", {
   lb <- data.table(
        Label = c(
            "prim. Agent exp", "Vaporizer setting",
            "prim. Agent exp",
            "prim. Agent exp", "Vaporizer setting",
            "prim. Agent exp", "Vaporizer setting"
        ),
        Current = c(
            0, 0,
            1.2,
            1.2, 8,
            0, 8
        ),
        CaseId = c(
            1, 1,
            2,
            3, 3,
            4, 4
        )
    )
    expect_equal(is_volatile_anesthesia(lb), c(FALSE, FALSE, TRUE, FALSE))
})

test_that("add_anaesthesia_case_id", {
   lb <- data.table(
        DateTime = 1:6,
        Label = c(
            "Start of therapy", "Case duration", "bar",
            "Start of therapy", "Case duration", "foo"
        )
    )
    expect_equal(add_anaesthesia_case_id(lb)$CaseId, c(1, 1, NA, 2, 2, NA))
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

test_that(".vaporizer_settings", {
    lb <- data.table(
        DateTime = lubridate::ymd_hm(
            c(rep("2024-06-04 12:45", 3), "2024-06-04 13:00")
        ),
        Label = c("Vaporizer setting", "foo", "bar", "Vaporizer setting"),
        Current = c(5.0, NA, NA, 4.8),
        Unit = "",
        `etCO2 [mmHg]` = c(35, NA, NA, 37)
    )
    r <- data.table(
        DateTime = lubridate::ymd_hm(c("2024-06-04 12:45", "2024-06-04 13:00")),
        Label = rep(c("Vaporizer setting"), 2),
        Current = c(5.0, 4.8),
        Unit = rep(c("Vol%"), 2)
    )
    expect_equal(.vaporizer_settings(lb), r)
})

test_that(".melt_measurements", {
    lb <- data.table(
        DateTime = lubridate::ymd_hm(
            c(rep("2024-06-04 12:45", 3), "2024-06-04 13:00")
        ),
        Label = c("Measurements", "foo", "bar", "Measurements"),
        `etCO2 [mmHg]` = c(35, "ERR", NA, 37),
        `MV [l/min]` = c(5.0, NA, "+++", 4.8)
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

