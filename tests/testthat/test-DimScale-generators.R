
context("DimScale-generators.R")

## Intervals -------------------------------------------------------------------

test_that("'Intervals' creates IntervalsInteger objects", {
    expect_is(Intervals(dimvalues = 0:4,
                        time_unit = NULL,
                        is_open_left = FALSE,
                        is_open_right = TRUE,
                        is_age = TRUE),
              "IntervalsInteger")
    expect_is(Intervals(dimvalues = c(2000, 2005),
                        time_unit = NULL,
                        is_open_left = FALSE,
                        is_open_right = FALSE,
                        is_age = FALSE),
              "IntervalsInteger")
    expect_is(Intervals(dimvalues = character(),
                        time_unit = NULL,
                        is_open_left = FALSE,
                        is_open_right = FALSE,
                        is_age = FALSE),
              "IntervalsInteger")
})

test_that("'Intervals' creates IntervalsDuration objects", {
    expect_is(Intervals(dimvalues = 0:4,
                        time_unit = "month",
                        is_open_left = FALSE,
                        is_open_right = TRUE,
                        is_age = TRUE),
              "IntervalsDuration")
    expect_is(Intervals(dimvalues = c(3, 4, 5),
                        time_unit = "quarter",
                        is_open_left = TRUE,
                        is_open_right = FALSE,
                        is_age = TRUE),
              "IntervalsDuration")
    expect_is(Intervals(dimvalues = character(),
                        time_unit = "month",
                        is_open_left = FALSE,
                        is_open_right = FALSE,
                        is_age = TRUE),
              "IntervalsDuration")
})

test_that("'Intervals' creates IntervalsDate objects", {
    expect_is(Intervals(dimvalues = as.Date(c("2001-01-01", "2001-02-01")),
                        time_unit = "month",
                        is_open_left = FALSE,
                        is_open_right = FALSE,
                        is_age = FALSE),
              "IntervalsDate")
    expect_is(Intervals(dimvalues = "2001-01-01",
                        time_unit = "quarter",
                        is_open_left = TRUE,
                        is_open_right = FALSE,
                        is_age = FALSE),
              "IntervalsDate")
    expect_is(Intervals(dimvalues = character(),
                        time_unit = "month",
                        is_open_left = FALSE,
                        is_open_right = FALSE,
                        is_age = FALSE),
              "IntervalsDate")
})


