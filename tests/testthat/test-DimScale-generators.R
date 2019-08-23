
context("DimScale-generators.R")

## Intervals -------------------------------------------------------------------

test_that("'Intervals' creates IntervalsInteger objects", {
    Intervals <- demarray:::Intervals
    expect_is(Intervals(dimvalues = 0:4,
                        unit = NULL,
                        is_open_left = FALSE,
                        is_open_right = TRUE,
                        is_age = TRUE),
              "IntervalsInteger")
    expect_is(Intervals(dimvalues = c(2000, 2005),
                        unit = NULL,
                        is_open_left = FALSE,
                        is_open_right = FALSE,
                        is_age = FALSE),
              "IntervalsInteger")
    expect_is(Intervals(dimvalues = character(),
                        unit = NULL,
                        is_open_left = FALSE,
                        is_open_right = FALSE,
                        is_age = FALSE),
              "IntervalsInteger")
})

test_that("'Intervals' creates IntervalsDuration objects", {
    Intervals <- demarray:::Intervals
    expect_is(Intervals(dimvalues = 0:4,
                        unit = "month",
                        is_open_left = FALSE,
                        is_open_right = TRUE,
                        is_age = TRUE),
              "IntervalsDuration")
    expect_is(Intervals(dimvalues = c(3, 4, 5),
                        unit = "quarter",
                        is_open_left = TRUE,
                        is_open_right = FALSE,
                        is_age = TRUE),
              "IntervalsDuration")
    expect_is(Intervals(dimvalues = character(),
                        unit = "month",
                        is_open_left = FALSE,
                        is_open_right = FALSE,
                        is_age = TRUE),
              "IntervalsDuration")
})

test_that("'Intervals' creates IntervalsDate objects", {
    Intervals <- demarray:::Intervals
    expect_is(Intervals(dimvalues = as.Date(c("2001-01-01", "2001-02-01")),
                        unit = "month",
                        is_open_left = FALSE,
                        is_open_right = FALSE,
                        is_age = FALSE),
              "IntervalsDate")
    expect_is(Intervals(dimvalues = "2001-01-01",
                        unit = "quarter",
                        is_open_left = TRUE,
                        is_open_right = FALSE,
                        is_age = FALSE),
              "IntervalsDate")
    expect_is(Intervals(dimvalues = character(),
                        unit = "month",
                        is_open_left = FALSE,
                        is_open_right = FALSE,
                        is_age = FALSE),
              "IntervalsDate")
})


## Points ----------------------------------------------------------------------

test_that("'Points' creates PointsInteger objects", {
    Points <- demarray:::Points
    expect_is(Points(dimvalues = 2000:2004,
                     unit = NULL,
                     is_age = FALSE),
              "PointsInteger")
    expect_is(Points(dimvalues = character(),
                     unit = NULL,
                     is_age = TRUE),
              "PointsInteger")
})

test_that("'Points' creates PointsDuration objects", {
    Points <- demarray:::Points
    expect_is(Points(dimvalues = 0:4,
                     unit = "month",
                     is_age = TRUE),
              "PointsDuration")
    expect_is(Points(dimvalues = character(),
                     unit = "quarter",
                     is_age = TRUE),
              "PointsDuration")
})

test_that("'Points' creates PointsDate objects", {
    Points <- demarray:::Points
    expect_is(Points(dimvalues = as.Date(c("2001-01-01", "2001-02-01")),
                     unit = "month",
                     is_age = FALSE),
              "PointsDate")
    expect_is(Points(dimvalues = "2001-01-01",
                     unit = "quarter",
                     is_age = FALSE),
              "PointsDate")
    expect_is(Points(dimvalues = character(),
                     unit = "month",
                     is_age = FALSE),
              "PointsDate")
})
