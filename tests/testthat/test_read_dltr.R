test_that("read_dltr throws errors", {
    expect_error(read_dltr("foo.bar"), "doesn't exist")
})

test_that("read_dltr returns a data.table", {
    expect_true(
        is.data.table(read_dltr(system.file("logbook.txt", package = "dltr")))
    )
})

test_that("is_logbook_file throws errors", {
    expect_error(is_logbook_file("foo.bar"), "cannot open the connection")
})

test_that("is_trends_file throws errors", {
    expect_error(is_trends_file("foo.bar"), "cannot open the connection")
})

test_that("is_logbook_file works", {
    expect_true(is_logbook_file(system.file("logbook.txt", package = "dltr")))
    expect_false(is_logbook_file(system.file("trends.txt", package = "dltr")))
})

test_that("is_trends_file works", {
    expect_true(is_trends_file(system.file("trends.txt", package = "dltr")))
    expect_false(is_trends_file(system.file("logbook.txt", package = "dltr")))
})
