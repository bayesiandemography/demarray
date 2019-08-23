
context("AllClasses-01-DimScale")

## Points ----------------------------------------------------------------------

## PointsInteger

test_that("can create empty object of class PointsInteger", {
    expect_is(new("PointsInteger",
                  dimvalues = integer()),
              "PointsInteger")                  
})

test_that("can have unequally spaced points in object of class PointsInteger", {
    expect_is(new("PointsInteger",
                  dimvalues = c(0L, 1L, 5L)),
              "PointsInteger")                  
})

test_that("cannot have NA or Inf in object of class PointsInteger", {
    expect_error(new("PointsInteger",
                     dimvalues = c(1L, NA_integer_)),
                 "'dimvalues' has NAs")                  
    expect_error(new("PointsInteger",
                     dimvalues = c(1L, Inf)),
                 "got class \"numeric\"")                  
})


## PointsDate

test_that("can create empty object of class PointsDate", {
    expect_is(new("PointsDate",
                  dimvalues = as.Date(character()),
                  unit = "month"),
              "PointsDate")                  
})

test_that("cannot have NA in object of class PointsDate", {
    expect_error(new("PointsDate",
                     dimvalues = as.Date(c("2001-01-01", NA)),
                     unit = "month"),
                 "'dimvalues' has NAs")                  
})

test_that("dates must be consistent with time unit in object of class PointsDate", {
    expect_is(new("PointsDate",
                  dimvalues = as.Date(c("2001-01-01", "2001-04-01")),
                  unit = "quarter"),
              "PointsDate")                  
    expect_error(new("PointsDate",
                     dimvalues = as.Date(c("2001-01-01", "2001-04-01")),
                     unit = "month"),
                 "do not belong to consecutive months")                  
    expect_error(new("PointsDate",
                     dimvalues = as.Date(c("2001-01-01", "2001-02-02")),
                     unit = "month"),
                 "is not the first day of the month")                  
})


## PointsDuration

test_that("can create empty object of class PointsDuration", {
    expect_is(new("PointsDuration",
                  dimvalues = integer(),
                  unit = "month"),
              "PointsDuration")                  
})

test_that("cannot have NA in object of class PointsDuration", {
    expect_error(new("PointsDuration",
                     dimvalues = c(1L, NA),
                     unit = "month"),
                 "'dimvalues' has NAs")                  
})

test_that("integers must be consecutive in object of class PointsDuration", {
    expect_is(new("PointsDuration",
                  dimvalues = 1:3,
                  unit = "quarter"),
              "PointsDuration")                  
    expect_error(new("PointsDuration",
                     dimvalues = c(1L, 3L),
                     unit = "month"),
                 "are not consecutive integers")                  
})


## Intervals ----------------------------------------------------------------------

## IntervalsInteger

test_that("can create empty object of class IntervalsInteger", {
    expect_is(new("IntervalsInteger",
                  dimvalues = integer(),
                  is_open_left = FALSE,
                  is_open_right = FALSE,
                  is_age = TRUE),
              "IntervalsInteger")                  
})

test_that("can have object of class IntervalsInteger consisting of single open interval", {
    expect_is(new("IntervalsInteger",
                  dimvalues = 0L,
                  is_open_left = TRUE,
                  is_open_right = FALSE,
                  is_age = FALSE),
              "IntervalsInteger")                  
    expect_is(new("IntervalsInteger",
                  dimvalues = 0L,
                  is_open_left = FALSE,
                  is_open_right = TRUE,
                  is_age = FALSE),
              "IntervalsInteger")                  
})

test_that("can have object of class IntervalsInteger consisting of two open intervals", {
    expect_is(new("IntervalsInteger",
                  dimvalues = 0L,
                  is_open_left = TRUE,
                  is_open_right = TRUE,
                  is_age = FALSE),
              "IntervalsInteger")                  
})

test_that("can have unequally spaced points in object of class IntervalsInteger", {
    expect_is(new("IntervalsInteger",
                  dimvalues = c(0L, 1L, 5L),
                  is_open_left = FALSE,
                  is_open_right = FALSE,
                  is_age = TRUE),
              "IntervalsInteger")                  
})

test_that("cannot have NA or Inf in object of class IntervalsInteger", {
    expect_error(new("IntervalsInteger",
                     dimvalues = c(1L, NA_integer_),
                     is_open_left = FALSE,
                     is_open_right = FALSE,
                     is_age = TRUE),
                 "'dimvalues' has NAs")                  
    expect_error(new("IntervalsInteger",
                     dimvalues = c(1L, Inf),
                     is_open_left = FALSE,
                     is_open_right = FALSE,
                     is_age = TRUE),
                 "got class \"numeric\"")                  
})


## IntervalsDate

test_that("can create empty object of class IntervalsDate", {
    expect_is(new("IntervalsDate",
                  dimvalues = as.Date(character()),
                  unit = "month",
                  is_open_left = FALSE,
                  is_open_right = FALSE),
              "IntervalsDate")                  
})

test_that("can have object of class IntervalsDate consisting of single open interval", {
    expect_is(new("IntervalsDate",
                  dimvalues = as.Date("2000-01-01"),
                  unit = "month",
                  is_open_left = TRUE,
                  is_open_right = FALSE),
              "IntervalsDate")                  
    expect_is(new("IntervalsDate",
                  dimvalues = as.Date("2000-01-01"),
                  unit = "quarter",
                  is_open_left = FALSE,
                  is_open_right = TRUE),
              "IntervalsDate")                  
})

test_that("can have object of class IntervalsDate consisting of two open intervals", {
    expect_is(new("IntervalsDate",
                  dimvalues = as.Date("2000-01-01"),
                  unit = "month",
                  is_open_left = TRUE,
                  is_open_right = TRUE),
              "IntervalsDate")                  
})

test_that("cannot have NA in object of class IntervalsDate", {
    expect_error(new("IntervalsDate",
                     dimvalues = as.Date(c("2001-01-01", NA)),
                     unit = "month",
                     is_open_left = FALSE,
                     is_open_right = FALSE),
                 "'dimvalues' has NAs")                  
})

test_that("dates must be consistent with time unit in object of class IntervalsDate", {
    expect_is(new("IntervalsDate",
                  dimvalues = as.Date(c("2001-01-01", "2001-04-01")),
                  unit = "quarter",
                  is_open_left = FALSE,
                  is_open_right = FALSE),
              "IntervalsDate")                  
    expect_error(new("IntervalsDate",
                     dimvalues = as.Date(c("2001-01-01", "2001-04-01")),
                     unit = "month",
                     is_open_left = FALSE,
                     is_open_right = FALSE),
                 "do not belong to consecutive months")                  
    expect_error(new("IntervalsDate",
                     dimvalues = as.Date(c("2001-01-01", "2001-02-02")),
                     unit = "month",
                     is_open_left = FALSE,
                     is_open_right = FALSE),
                 "is not the first day of the month")                  
})


## IntervalsDuration

test_that("can create empty object of class IntervalsDuration", {
    expect_is(new("IntervalsDuration",
                  dimvalues = integer(),
                  unit = "month",
                  is_open_left = FALSE,
                  is_open_right = FALSE),
              "IntervalsDuration")                  
})

test_that("can have object of class IntervalsDuration consisting of single open interval", {
    expect_is(new("IntervalsDuration",
                  dimvalues = 0L,
                  unit = "month",
                  is_open_left = TRUE,
                  is_open_right = FALSE),
              "IntervalsDuration")                  
    expect_is(new("IntervalsDuration",
                  dimvalues = 0L,
                  unit = "quarter",
                  is_open_left = FALSE,
                  is_open_right = TRUE),
              "IntervalsDuration")                  
})

test_that("can have object of class IntervalsDuration consisting of two open intervals", {
    expect_is(new("IntervalsDuration",
                  dimvalues = 0L,
                  unit = "month",
                  is_open_left = TRUE,
                  is_open_right = TRUE),
              "IntervalsDuration")                  
})

test_that("cannot have NA in object of class IntervalsDuration", {
    expect_error(new("IntervalsDuration",
                     dimvalues = c(1L, NA),
                     unit = "month",
                     is_open_left = FALSE,
                     is_open_right = FALSE),
                 "'dimvalues' has NAs")                  
})

test_that("integers must be consecutive in object of class IntervalsDuration", {
    expect_is(new("IntervalsDuration",
                  dimvalues = 1:3,
                  unit = "quarter",
                  is_open_left = FALSE,
                  is_open_right = FALSE),
              "IntervalsDuration")                  
    expect_error(new("IntervalsDuration",
                     dimvalues = c(1L, 3L),
                     unit = "month",
                     is_open_left = FALSE,
                     is_open_right = FALSE),
                 "not consecutive integers")                  
})
