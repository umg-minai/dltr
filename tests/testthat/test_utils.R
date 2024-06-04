test_that(".extract_units_from_column_name", {
    expect_equal(
        .extract_units_from_column_name(
            c("foo", "bar [l/min]", "Set FG O2 [Vol%]", "Set FG flow [l/min]")
        ),
        c(NA, "l/min", "Vol%", "l/min")
    )
})

test_that(".drop_units_from_column_name", {
    expect_equal(
        .drop_units_from_column_name(
            c("foo", "bar [l/min]", "Set FG O2 [Vol%]", "Set FG flow [l/min]")
        ),
        c("foo", "bar", "Set FG O2", "Set FG flow")
    )
})
