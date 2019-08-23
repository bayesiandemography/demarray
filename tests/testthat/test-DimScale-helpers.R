
context("DimScale-helpers")

## make_dimscale_labels --------------------------------------------------------

## make_dimscale_labels_points_duration

test_that("make_dimscale_labels_points_duration gives correct answers with valid input", {
    make_dimscale_labels_points_duration <- demarray:::make_dimscale_labels_points_duration
    expect_identical(make_dimscale_labels_points_duration(dimvalues = 0:14,
                                                             unit = "month"),
                     c(sprintf("0y %dm", 0:11), sprintf("1y %dm", 0:2)))
    expect_identical(make_dimscale_labels_points_duration(dimvalues = 2:5,
                                                          unit = "quarter"),
                     c("0y 2q", "0y 3q", "1y 0q", "1y 1q"))
    expect_identical(make_dimscale_labels_points_duration(dimvalues = integer(),
                                                          unit = "quarter"),
                     character())
    expect_identical(make_dimscale_labels_points_duration(dimvalues = 0L,
                                                          unit = "quarter"),
                     "0y 0q")
})

test_that("make_dimscale_labels_points_duration throws error with invalid input", {
    make_dimscale_labels_points_duration <- demarray:::make_dimscale_labels_points_duration
    expect_error(make_dimscale_labels_points_duration(dimvalues = 2:5,
                                                      unit = "wrong"),
                 "can't handle time unit 'wrong'")
})


## make_dimscale_labels_points_date

test_that("make_dimscale_labels_points_date gives correct answers with valid input", {
    make_dimscale_labels_points_date <- demarray:::make_dimscale_labels_points_date
    dv <- seq.Date(from = as.Date("2019-01-01"),
                   to = as.Date("2020-02-01"),
                   by = "month")
    expect_identical(make_dimscale_labels_points_date(dimvalues = dv,
                                                      unit = "month"),
                     paste(format(dv, "%Y"), months(dv, abb = TRUE)))
    expect_identical(make_dimscale_labels_points_date(dimvalues = as.Date(c("2020-04-01", "2020-07-01", "2020-10-01")),
                                                      unit = "quarter"),
                     c("2020 Q2", "2020 Q3", "2020 Q4"))
    expect_identical(make_dimscale_labels_points_date(dimvalues = as.Date(character()),
                                                      unit = "quarter"),
                     character())
})

test_that("make_dimscale_labels_points_date throws error with invalid input", {
    make_dimscale_labels_points_date <- demarray:::make_dimscale_labels_points_date
    expect_error(make_dimscale_labels_points_date(dimvalues = as.Date(c("2001-01-01", "2001-01-02")),
                                                  unit = "day"),
                 "can't handle time unit 'day'")
})


## make_dimscale_labels_intervals_integer

test_that("make_dimscale_labels_intervals_integer gives correct answers with valid input", {
    make_dimscale_labels_intervals_integer <- demarray:::make_dimscale_labels_intervals_integer
    expect_identical(make_dimscale_labels_intervals_integer(dimvalues = 0:5,
                                                            is_open_left = FALSE,
                                                            is_open_right = FALSE,
                                                            is_age = TRUE),
                     c("0", "1", "2", "3", "4"))
    expect_identical(make_dimscale_labels_intervals_integer(dimvalues = 0:5,
                                                            is_open_left = TRUE,
                                                            is_open_right = FALSE,
                                                            is_age = TRUE),
                     c("<0", "0", "1", "2", "3", "4"))
    expect_identical(make_dimscale_labels_intervals_integer(dimvalues = 0:5,
                                                            is_open_left = TRUE,
                                                            is_open_right = TRUE,
                                                            is_age = TRUE),
                     c("<0", "0", "1", "2", "3", "4", "5+"))
    expect_identical(make_dimscale_labels_intervals_integer(dimvalues = c(0L, 1L, 5L),
                                                            is_open_left = FALSE,
                                                            is_open_right = TRUE,
                                                            is_age = TRUE),
                     c("0", "1-4", "5+"))
    expect_identical(make_dimscale_labels_intervals_integer(dimvalues = c(0L, 1L, 5L),
                                                            is_open_left = FALSE,
                                                            is_open_right = TRUE,
                                                            is_age = FALSE),
                     c("0", "1-5", "5+"))
    expect_identical(make_dimscale_labels_intervals_integer(dimvalues = c(2000L, 2005L, 2020L),
                                                            is_open_left = FALSE,
                                                            is_open_right = FALSE,
                                                            is_age = FALSE),
                     c("2000-2005", "2005-2020"))
    expect_identical(make_dimscale_labels_intervals_integer(dimvalues = 2000L,
                                                            is_open_left = FALSE,
                                                            is_open_right = TRUE,
                                                            is_age = FALSE),
                     "2000+")
    expect_identical(make_dimscale_labels_intervals_integer(dimvalues = integer(),
                                                            is_open_left = FALSE,
                                                            is_open_right = FALSE,
                                                            is_age = FALSE),
                     character())
})


## make_dimscale_labels_intervals_duration

test_that("make_dimscale_labels_intervals_duration gives correct answers with valid input", {
    make_dimscale_labels_intervals_duration <- demarray:::make_dimscale_labels_intervals_duration
    expect_identical(make_dimscale_labels_intervals_duration(dimvalues = 0:14,
                                                             unit = "month",
                                                             is_open_left = FALSE,
                                                             is_open_right = FALSE),
                     c(sprintf("0y %dm", 0:11), sprintf("1y %dm", 0:1)))
    expect_identical(make_dimscale_labels_intervals_duration(dimvalues = 0:14,
                                                             unit = "month",
                                                             is_open_left = TRUE,
                                                             is_open_right = TRUE),
                     c("<0y 0m", sprintf("0y %dm", 0:11), sprintf("1y %dm", 0:1), "1y 2m+"))
    expect_identical(make_dimscale_labels_intervals_duration(dimvalues = 2:5,
                                                             unit = "quarter",
                                                             is_open_left = FALSE,
                                                             is_open_right = FALSE),
                     c("0y 2q", "0y 3q", "1y 0q"))
    expect_identical(make_dimscale_labels_intervals_duration(dimvalues = integer(),
                                                             unit = "quarter",
                                                             is_open_left = FALSE,
                                                             is_open_right = FALSE),
                     character())
    expect_identical(make_dimscale_labels_intervals_duration(dimvalues = 0L,
                                                             unit = "quarter",
                                                             is_open_left = FALSE,
                                                             is_open_right = TRUE),
                     "0y 0q+")
    expect_identical(make_dimscale_labels_intervals_duration(dimvalues = 0L,
                                                             unit = "quarter",
                                                             is_open_left = TRUE,
                                                             is_open_right = FALSE),
                     "<0y 0q")
    expect_identical(make_dimscale_labels_intervals_duration(dimvalues = 0L,
                                                             unit = "quarter",
                                                             is_open_left = TRUE,
                                                             is_open_right = TRUE),
                     c("<0y 0q", "0y 0q+"))
})

test_that("make_dimscale_labels_intervals_duration throws error with invalid input", {
    make_dimscale_labels_intervals_duration <- demarray:::make_dimscale_labels_intervals_duration
    expect_error(make_dimscale_labels_intervals_duration(dimvalues = 2:5,
                                                         unit = "wrong",
                                                         is_open_left = FALSE,
                                                         is_open_right = FALSE),
                 "can't handle time unit 'wrong'")
})


## make_dimscale_labels_intervals_date

test_that("make_dimscale_labels_intervals_date gives correct answers with valid input", {
    make_dimscale_labels_intervals_date <- demarray:::make_dimscale_labels_intervals_date
    dv <- seq.Date(from = as.Date("2019-01-01"),
                   to = as.Date("2020-02-01"),
                   by = "month")
    expect_identical(make_dimscale_labels_intervals_date(dimvalues = dv,
                                                         unit = "month",
                                                         is_open_left = FALSE,
                                                         is_open_right = FALSE),
                     paste(format(dv[1:13], "%Y"), months(dv[1:13], abb = TRUE)))
    expect_identical(make_dimscale_labels_intervals_date(dimvalues = dv,
                                                         unit = "month",
                                                         is_open_left = TRUE,
                                                         is_open_right = TRUE),
                     c("<2019 Jan",
                       paste(format(dv[1:13], "%Y"), months(dv[1:13], abb = TRUE)),
                       "2020 Feb+"))
    expect_identical(make_dimscale_labels_intervals_date(dimvalues = as.Date(c("2020-04-01", "2020-07-01", "2020-10-01")),
                                                         unit = "quarter",
                                                         is_open_left = FALSE,
                                                         is_open_right = FALSE),
                     c("2020 Q2", "2020 Q3"))
    expect_identical(make_dimscale_labels_intervals_date(dimvalues = as.Date(character()),
                                                         unit = "quarter",
                                                         is_open_left = FALSE,
                                                         is_open_right = FALSE),
                     character())
    expect_identical(make_dimscale_labels_intervals_date(dimvalues = as.Date("2020-04-01"),
                                                         unit = "quarter",
                                                         is_open_left = FALSE,
                                                         is_open_right = TRUE),
                     "2020 Q2+")
    expect_identical(make_dimscale_labels_intervals_date(dimvalues = as.Date("2020-04-01"),
                                                         unit = "quarter",
                                                         is_open_left = TRUE,
                                                         is_open_right = FALSE),
                     "<2020 Q2")
    expect_identical(make_dimscale_labels_intervals_date(dimvalues = as.Date("2020-04-01"),
                                                         unit = "quarter",
                                                         is_open_left = TRUE,
                                                         is_open_right = TRUE),
                     c("<2020 Q2", "2020 Q2+"))
})

    
test_that("make_dimscale_labels_intervals_date throws error with invalid input", {
    make_dimscale_labels_intervals_date <- demarray:::make_dimscale_labels_intervals_date
    expect_error(make_dimscale_labels_intervals_date(dimvalues = as.Date(c("2001-01-01", "2001-01-02")),
                                                     unit = "day",
                                                     is_open_left = FALSE,
                                                     is_open_right = FALSE),
                 "can't handle time unit 'day'")
})


