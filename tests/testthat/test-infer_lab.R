
context("infer_lab")

## General function -----------------------------------------------------------

test_that("infer_lab gives correct answer with valid inputs", {
    infer_lab <- demarray:::infer_lab
    labels <- 1:3
    expect_identical(infer_lab(labels = labels, dimtype = "time"),
                     LabIntegers(int_min = 1L,
                                 int_max = 3L,
                                 include_na = FALSE))
    labels <- c("0", "100-999", NA, "-5--1")
    expect_identical(infer_lab(labels = labels, dimtype = "attribute"),
                     LabGroupedIntEnumerations(breaks = c(-5L, 0L, 1L, 100L, 1000L),
                                               open_first = FALSE,
                                               open_last = FALSE,
                                               include_na = TRUE))
    labels <- c("A", "B", "C")
    expect_identical(infer_lab(labels = labels, dimtype = "age"),
                     LabCategories(labels = labels,
                                   include_na = FALSE))
    labels <- character()
    expect_identical(infer_lab(labels = labels, dimtype = "cohort"),
                     LabCategories(labels = character(),
                                   include_na = FALSE))
})

test_that("infer_lab throws correct error with invalid inputs", {
    infer_lab <- demarray:::infer_lab
    labels <- 1:3
    expect_error(infer_lab(labels = labels, dimtype = "wrong"),
                 "\"wrong\" is not a valid dimtype")
})



## Functions for specific dimtypes --------------------------------------------

test_that("infer_lab_attribute gives correct answer with valid inputs", {
    infer_lab_attribute <- demarray:::infer_lab_attribute
    labels <- 1:3
    expect_identical(infer_lab_attribute(labels),
                     LabIntegers(int_min = 1L,
                                 int_max = 3L,
                                 include_na = FALSE))
    labels <- c("0", "100-999", NA, "-5--1")
    expect_identical(infer_lab_attribute(labels),
                     LabGroupedIntEnumerations(breaks = c(-5L, 0L, 1L, 100L, 1000L),
                                               open_first = FALSE,
                                               open_last = FALSE,
                                               include_na = TRUE))
    labels <- c("A", "B", "C")
    expect_identical(infer_lab_attribute(labels),
                     LabCategories(labels = labels,
                                   include_na = FALSE))
    labels <- character()
    expect_identical(infer_lab_attribute(labels),
                     LabCategories(labels = character(),
                                   include_na = FALSE))
})

test_that("infer_lab_iterations gives correct answer with valid inputs", {
    infer_lab_iterations <- demarray:::infer_lab_iterations
    labels <- 1:3
    expect_identical(infer_lab_integers(labels),
                     LabIntegers(int_min = 1L,
                                 int_max = 3L,
                                 include_na = FALSE))
    labels <- c("0", "100-999", NA, "-5--1")
    expect_identical(infer_lab_iterations(labels),
                     LabCategories(labels = labels[-3],
                                   include_na = TRUE))
    labels <- c("A", "B", "C")
    expect_identical(infer_lab_iterations(labels),
                     LabCategories(labels = labels,
                                   include_na = FALSE))
    labels <- character()
    expect_identical(infer_lab_iterations(labels),
                     LabCategories(labels = character(),
                                   include_na = FALSE))
})

test_that("infer_lab_iterations gives correct answer with valid inputs", {
    infer_lab_iterations <- demarray:::infer_lab_iterations
    labels <- 1:3
    expect_identical(infer_lab_integers(labels),
                     LabIntegers(int_min = 1L,
                                 int_max = 3L,
                                 include_na = FALSE))
    labels <- c("0", "100-999", NA, "-5--1")
    expect_identical(infer_lab_iterations(labels),
                     LabCategories(labels = labels[-3],
                                   include_na = TRUE))
    labels <- c("A", "B", "C")
    expect_identical(infer_lab_iterations(labels),
                     LabCategories(labels = labels,
                                   include_na = FALSE))
    labels <- character()
    expect_identical(infer_lab_iterations(labels),
                     LabCategories(labels = character(),
                                   include_na = FALSE))
})

test_that("infer_lab_age gives correct answer with valid inputs", {
    infer_lab_age <- demarray:::infer_lab_age
    labels <- 1:3
    expect_identical(infer_lab_integers(labels),
                     LabIntegers(int_min = 1L,
                                 int_max = 3L,
                                 include_na = FALSE))
    labels <- c("0", "100-999", NA, "-5--1")
    expect_identical(infer_lab_age(labels),
                     LabGroupedIntEnumerations(breaks = c(-5L, 0L, 1L, 100L, 1000L),
                                               open_first = FALSE,
                                               open_last = FALSE,
                                               include_na = TRUE))
    labels <- c("0q", "1q", "2q", "3q+")
    LabDurationsQuarters(break_min = 0L,
                         break_max = 3L,
                         open_last = FALSE,
                         include_na = FALSE)
    labels <- c("0m", "1m", "2m", "3m+")
    LabDurationsMonths(break_min = 0L,
                       break_max = 3L,
                       open_last = FALSE,
                       include_na = FALSE)
    labels <- c("A", "B", "C")
    expect_identical(infer_lab_age(labels),
                     LabCategories(labels = labels,
                                   include_na = FALSE))
    labels <- character()
    expect_identical(infer_lab_age(labels),
                     LabCategories(labels = character(),
                                   include_na = FALSE))
})

test_that("infer_lab_time gives correct answer with valid inputs", {
    infer_lab_time <- demarray:::infer_lab_time
    labels <- 1:3
    expect_identical(infer_lab_integers(labels),
                     LabIntegers(int_min = 1L,
                                 int_max = 3L,
                                 include_na = FALSE))
    labels <- c("2000-2001", "2010-2020", NA, "1600-1604")
    expect_identical(infer_lab_time(labels),
                     LabGroupedIntEndpoints(breaks = c(1600L, 1604L, 2000L, 2001L,
                                                       2010L, 2020L),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = TRUE))
    labels <- c("2000 Q1", "2000 Q3", "2000Q2", "2001 Q1+")
    LabCalendarQuarters(break_min = as.Date("2000-01-01"),
                        break_max = as.Date("2001-01-01"),
                        open_first = FALSE,
                        open_last = TRUE,
                        include_na = FALSE)
    labels <- c("2020 Feb", "2020 Oct", "<2020 Jan", "2020 Nov+")
    LabCalendarMonths(break_min = as.Date("2020-01-01"),
                      break_max = as.Date("2020-11-01"),
                      open_first = TRUE,
                      open_last = TRUE,
                      include_na = FALSE)
    labels <- c("A", "B", "C")
    expect_identical(infer_lab_time(labels),
                     LabCategories(labels = labels,
                                   include_na = FALSE))
    labels <- character()
    expect_identical(infer_lab_time(labels),
                     LabCategories(labels = character(),
                                   include_na = FALSE))
})

test_that("infer_lab_cohort gives correct answer with valid inputs", {
    infer_lab_cohort <- demarray:::infer_lab_cohort
    labels <- 1:3
    expect_identical(infer_lab_integers(labels),
                     LabIntegers(int_min = 1L,
                                 int_max = 3L,
                                 include_na = FALSE))
    labels <- c("2000-2001", "2010-2020", NA, "1600-1604")
    expect_identical(infer_lab_cohort(labels),
                     LabGroupedIntEndpoints(breaks = c(1600L, 1604L, 2000L, 2001L,
                                                       2010L, 2020L),
                                            open_first = FALSE,
                                            open_last = FALSE,
                                            include_na = TRUE))
    labels <- c("2000 Q1", "2000 Q3", "2000Q2", "2001 Q1+")
    LabCalendarQuarters(break_min = as.Date("2000-01-01"),
                        break_max = as.Date("2001-01-01"),
                        open_first = FALSE,
                        open_last = TRUE,
                        include_na = FALSE)
    labels <- c("2020 Feb", "2020 Oct", "<2020 Jan", "2020 Nov+")
    LabCalendarMonths(break_min = as.Date("2020-01-01"),
                      break_max = as.Date("2020-11-01"),
                      open_first = TRUE,
                      open_last = TRUE,
                      include_na = FALSE)
    labels <- c("A", "B", "C")
    expect_identical(infer_lab_cohort(labels),
                     LabCategories(labels = labels,
                                   include_na = FALSE))
    labels <- character()
    expect_identical(infer_lab_cohort(labels),
                     LabCategories(labels = character(),
                                   include_na = FALSE))
})








## Functions for individual "Label" classes -----------------------------------

test_that("infer_lab_categories gives correct answer with valid inputs", {
    infer_lab_categories <- demarray:::infer_lab_categories
    labels <- c("A", "B", "C")
    expect_identical(infer_lab_categories(labels),
                     LabCategories(labels = labels,
                                   include_na = FALSE))
    labels <- c("A", "B", NA, "C")
    expect_identical(infer_lab_categories(labels),
                     LabCategories(labels = labels[-3],
                                   include_na = TRUE))
    labels <- NA_character_
    expect_identical(infer_lab_categories(labels),
                     LabCategories(labels = character(),
                                   include_na = TRUE))
})

test_that("infer_lab_categories throws correct error with invalid inputs", {
    infer_lab_categories <- demarray:::infer_lab_categories
    labels <- c("A", "B", "")
    expect_error(infer_lab_categories(labels),
                 "'labels' has blanks")
})

test_that("infer_lab_triangles gives correct answer with valid inputs", {
    infer_lab_triangles <- demarray:::infer_lab_triangles
    labels <- c("Upper", "Lower", "Upper")
    expect_identical(infer_lab_triangles(labels),
                     LabTriangles(include_na = FALSE))
    labels <- c("Lower", "Upper", NA)
    expect_identical(infer_lab_triangles(labels),
                     LabTriangles(include_na = TRUE))
    labels <- NA_character_
    expect_identical(infer_lab_triangles(labels),
                     LabTriangles(include_na = TRUE))
})

test_that("infer_lab_triangles throws correct error or message with invalid inputs", {
    infer_lab_triangles <- demarray:::infer_lab_triangles
    labels <- ""
    expect_error(infer_lab_triangles(labels),
                 "'labels' has blanks")
    labels <- "wrong"
    expect_identical(infer_lab_triangles(labels),
                     "\"wrong\" not a valid label for triangles")
})

test_that("infer_lab_pool gives correct answer with valid inputs", {
    infer_lab_pool <- demarray:::infer_lab_pool
    labels <- c("Ins", "Outs", "Ins")
    expect_identical(infer_lab_pool(labels),
                     LabPool(include_na = FALSE))
    labels <- c("Outs", "Ins", NA)
    expect_identical(infer_lab_pool(labels),
                     LabPool(include_na = TRUE))
    labels <- NA_character_
    expect_identical(infer_lab_pool(labels),
                     LabPool(include_na = TRUE))
})

test_that("infer_lab_pool throws correct error or message with invalid inputs", {
    infer_lab_pool <- demarray:::infer_lab_pool
    labels <- ""
    expect_error(infer_lab_pool(labels),
                 "'labels' has blanks")
    labels <- "wrong"
    expect_identical(infer_lab_pool(labels),
                     "\"wrong\" not a valid label for pool")
})

test_that("infer_lab_quantiles gives correct answer with valid inputs", {
    infer_lab_quantiles <- demarray:::infer_lab_quantiles
    labels <- c("50.0001%", "50%", "1.254%")
    expect_identical(infer_lab_quantiles(labels),
                     LabQuantiles(labels = labels[c(3, 2, 1)],
                                  include_na = FALSE))
    labels <- c("0.001%", "100%", NA)
    expect_identical(infer_lab_quantiles(labels),
                     LabQuantiles(labels = labels[1:2],
                                  include_na = TRUE))
    labels <- NA_character_
    expect_identical(infer_lab_quantiles(labels),
                     LabQuantiles(labels = character(),
                                  include_na = TRUE))
})

test_that("infer_lab_quantiles throws correct error or message with invalid inputs", {
    infer_lab_quantiles <- demarray:::infer_lab_quantiles
    labels <- ""
    expect_error(infer_lab_quantiles(labels),
                 "'labels' has blanks")
    labels <- "wrong"
    expect_identical(infer_lab_quantiles(labels),
                     "\"wrong\" is not a valid quantile")
})

test_that("infer_lab_integers gives correct answer with valid inputs", {
    infer_lab_integers <- demarray:::infer_lab_integers
    labels <- c("1", "12", "4", "-1")
    expect_identical(infer_lab_integers(labels),
                     LabIntegers(int_min = -1L,
                                 int_max = 12L,
                                 include_na = FALSE))
    labels <- c("0", "0", NA)
    expect_identical(infer_lab_integers(labels),
                     LabIntegers(int_min = 0L,
                                 int_max = 0L,
                                 include_na = TRUE))
})

test_that("infer_lab_integers throws correct error or message with invalid inputs", {
    infer_lab_integers <- demarray:::infer_lab_integers
    labels <- ""
    expect_error(infer_lab_integers(labels),
                 "'labels' has blanks")
    labels <- c(NA_character_, NA_character_)
    expect_identical(infer_lab_integers(labels),
                     "'labels' has no non-NA elements")
})

test_that("infer_lab_grouped_int_enumeration gives correct answer with valid inputs", {
    infer_lab_grouped_int_enumerations <- demarray:::infer_lab_grouped_int_enumerations
    labels <- c("0", "1-4", "5-9", "10+")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     LabGroupedIntEnumerations(breaks = c(0L, 1L, 5L, 10L),
                                               open_first = FALSE,
                                               open_last = TRUE,
                                               include_na = FALSE))
    labels <- c("10+", "0", "1-4", "5-9", "-5--1", NA, "<-5")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     LabGroupedIntEnumerations(breaks = c(-5L, 0L, 1L, 5L, 10L),
                                               open_first = TRUE,
                                               open_last = TRUE,
                                               include_na = TRUE))
    labels <- "<10"
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     LabGroupedIntEnumerations(breaks = 10L,
                                               open_first = TRUE,
                                               open_last = FALSE,
                                               include_na = FALSE))
    labels <- c(NA, "10+")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     LabGroupedIntEnumerations(breaks = 10L,
                                               open_first = FALSE,
                                               open_last = TRUE,
                                               include_na = TRUE))
    labels <- c("10", "11", "8-9", NA, "12+")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     LabGroupedIntEnumerations(breaks = c(8L, 10L, 11L, 12L),
                                               open_first = FALSE,
                                               open_last = TRUE,
                                               include_na = TRUE))
})

test_that("infer_lab_grouped_int_enumerations gives correct errors/messages with invalid inputs", {
    infer_lab_grouped_int_enumerations <- demarray:::infer_lab_grouped_int_enumerations
    labels <- c(NA, NA)
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "'labels' has no non-NA elements")
    labels <- c("wrong", NA)
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "\"wrong\" not a valid enumeration label")
    labels <- c("<10", "<5", "20-29")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "two different labels for interval open on left : \"<5\" and \"<10\"")
    labels <- c("100+", "50+", "20-29")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "two different labels for interval open on right : \"50+\" and \"100+\"")
    labels <- "10-10"
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "\"10-10\" not a valid enumeration label")
    labels <- "10-9"
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "\"10-9\" not a valid enumeration label")
    labels <- c("<10", "5+")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"<10\" and \"5+\" overlap")
    labels <- c("<10", "10-14", "15-19", "20-25", "25-29")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"20-25\" and \"25-29\" overlap")
    labels <- c("0-4", "0", "1-4")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"0-4\" and \"0\" overlap")
    labels <- c("20+", "<25")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"<25\" and \"20+\" overlap")
    labels <- c("20-30", "<25")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"<25\" and \"20-30\" overlap")
    labels <- c("20-30", "25+")
    expect_identical(infer_lab_grouped_int_enumerations(labels),
                     "intervals defined by labels \"20-30\" and \"25+\" overlap")
})

test_that("infer_lab_grouped_int_endpoints gives correct answer with valid inputs", {
    infer_lab_grouped_int_endpoints <- demarray:::infer_lab_grouped_int_endpoints
    labels <- c("0-1", "1-5", "5-10", "10+")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     LabGroupedIntEndpoints(breaks = c(0L, 1L, 5L, 10L),
                                            open_first = FALSE,
                                            open_last = TRUE,
                                            include_na = FALSE))
    labels <- c("10+", "0-1", "1-5", "5-10", "-5--0", NA, "<-5")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     LabGroupedIntEndpoints(breaks = c(-5L, 0L, 1L, 5L, 10L),
                                            open_first = TRUE,
                                            open_last = TRUE,
                                            include_na = TRUE))
    labels <- "<10"
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     LabGroupedIntEndpoints(breaks = 10L,
                                            open_first = TRUE,
                                            open_last = FALSE,
                                            include_na = FALSE))
    labels <- c(NA, "10+")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     LabGroupedIntEndpoints(breaks = 10L,
                                            open_first = FALSE,
                                            open_last = TRUE,
                                            include_na = TRUE))
    labels <- c("10-11", "11-12", "8-10", NA, "12+")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     LabGroupedIntEndpoints(breaks = c(8L, 10L, 11L, 12L),
                                            open_first = FALSE,
                                            open_last = TRUE,
                                            include_na = TRUE))
})

test_that("infer_lab_grouped_int_endpoints gives correct errors/messages with invalid inputs", {
    infer_lab_grouped_int_endpoints <- demarray:::infer_lab_grouped_int_endpoints
    labels <- c(NA, NA)
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "'labels' has no non-NA elements")
    labels <- c("wrong", NA)
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "\"wrong\" not a valid endpoints label")
    labels <- c("<10", "<5", "20-30")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "two different labels for interval open on left : \"<5\" and \"<10\"")
    labels <- c("100+", "50+", "20-30")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "two different labels for interval open on right : \"50+\" and \"100+\"")
    labels <- "10-10"
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "\"10-10\" not a valid endpoints label")
    labels <- "10-9"
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "\"10-9\" not a valid endpoints label")
    labels <- c("<10", "5+")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"<10\" and \"5+\" overlap")
    labels <- c("<10", "10-15", "15-20", "20-25", "24-29")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"20-25\" and \"24-29\" overlap")
    labels <- c("0-5", "0-1", "1-5")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"0-5\" and \"0-1\" overlap")
    labels <- c("20+", "<25")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"<25\" and \"20+\" overlap")
    labels <- c("20-30", "<25")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"<25\" and \"20-30\" overlap")
    labels <- c("20-30", "25+")
    expect_identical(infer_lab_grouped_int_endpoints(labels),
                     "intervals defined by labels \"20-30\" and \"25+\" overlap")
})

test_that("infer_lab_calendar_quarters gives correct answer with valid inputs", {
    infer_lab_calendar_quarters <- demarray:::infer_lab_calendar_quarters
    labels <- c("2019 Q3", "2019 Q1", "<2018 Q3", "2020 Q1+")
    expect_identical(infer_lab_calendar_quarters(labels),
                     LabCalendarQuarters(break_min = as.Date("2018-07-01"),
                                         break_max = as.Date("2020-01-01"),
                                         open_first = TRUE,
                                         open_last = TRUE,
                                         include_na = FALSE))
    labels <- "<2019 Q3"
    expect_identical(infer_lab_calendar_quarters(labels),
                     LabCalendarQuarters(break_min = as.Date("2019-07-01"),
                                         break_max = as.Date("2019-07-01"),
                                         open_first = TRUE,
                                         open_last = FALSE,
                                         include_na = FALSE))
    labels <- "2019 Q3+"
    expect_identical(infer_lab_calendar_quarters(labels),
                     LabCalendarQuarters(break_min = as.Date("2019-07-01"),
                                         break_max = as.Date("2019-07-01"),
                                         open_first = FALSE,
                                         open_last = TRUE,
                                         include_na = FALSE))
    labels <- c(NA, "<2019 Q3", "2019 Q3+")
    expect_identical(infer_lab_calendar_quarters(labels),
                     LabCalendarQuarters(break_min = as.Date("2019-07-01"),
                                         break_max = as.Date("2019-07-01"),
                                         open_first = TRUE,
                                         open_last = TRUE,
                                         include_na = TRUE))
    labels <- "2000 Q1"
    expect_identical(infer_lab_calendar_quarters(labels),
                     LabCalendarQuarters(break_min = as.Date("2000-01-01"),
                                         break_max = as.Date("2000-04-01"),
                                         open_first = FALSE,
                                         open_last = FALSE,
                                         include_na = FALSE))
})

test_that("infer_lab_calendar_quarters gives correct errors/messages with invalid inputs", {
    infer_lab_calendar_quarters <- demarray:::infer_lab_calendar_quarters
    labels <- c(NA, NA)
    expect_identical(infer_lab_calendar_quarters(labels),
                     "'labels' has no non-NA elements")
    labels <- c("wrong", NA)
    expect_identical(infer_lab_calendar_quarters(labels),
                     "\"wrong\" not a valid label for period of one quarter")
    labels <- c("<2000 Q1", "<2000 Q2", "2000 Q4")
    expect_identical(infer_lab_calendar_quarters(labels),
                     "two different labels for period open on left : \"<2000 Q1\" and \"<2000 Q2\"")
    labels <- c("2000 Q4+", "2001 Q3+", NA)
    expect_identical(infer_lab_calendar_quarters(labels),
                     "two different labels for period open on right : \"2000 Q4+\" and \"2001 Q3+\"")
    labels <- c("<2005 Q1", "2004 Q4+")
    expect_identical(infer_lab_calendar_quarters(labels),
                     "periods \"<2005 Q1\" and \"2004 Q4+\" overlap")
    labels <- c("2010 Q1", "2005 Q1", "<2006 Q3")
    expect_identical(infer_lab_calendar_quarters(labels),
                     "periods \"<2006 Q3\" and \"2005 Q1\" overlap")
    labels <- c("2010 Q1", "2005 Q1", "2006 Q3+")
    expect_identical(infer_lab_calendar_quarters(labels),
                     "periods \"2010 Q1\" and \"2006 Q3+\" overlap")
})

test_that("infer_lab_calendar_months gives correct answer with valid inputs", {
    infer_lab_calendar_months <- demarray:::infer_lab_calendar_months
    labels <- c("2019 Nov", "2019 Oct", "<2018 Feb", "2020 Mar+")
    expect_identical(infer_lab_calendar_months(labels),
                     LabCalendarMonths(break_min = as.Date("2018-02-01"),
                                       break_max = as.Date("2020-03-01"),
                                       open_first = TRUE,
                                       open_last = TRUE,
                                       include_na = FALSE))
    labels <- "<2019 Sep"
    expect_identical(infer_lab_calendar_months(labels),
                     LabCalendarMonths(break_min = as.Date("2019-09-01"),
                                       break_max = as.Date("2019-09-01"),
                                       open_first = TRUE,
                                       open_last = FALSE,
                                       include_na = FALSE))
    labels <- "2019 Sep+"
    expect_identical(infer_lab_calendar_months(labels),
                     LabCalendarMonths(break_min = as.Date("2019-09-01"),
                                       break_max = as.Date("2019-09-01"),
                                       open_first = FALSE,
                                       open_last = TRUE,
                                       include_na = FALSE))
    labels <- c(NA, "<2019 Apr", "2019 Apr+")
    expect_identical(infer_lab_calendar_months(labels),
                     LabCalendarMonths(break_min = as.Date("2019-04-01"),
                                       break_max = as.Date("2019-04-01"),
                                       open_first = TRUE,
                                       open_last = TRUE,
                                       include_na = TRUE))
    labels <- "2000 Feb"
    expect_identical(infer_lab_calendar_months(labels),
                     LabCalendarMonths(break_min = as.Date("2000-02-01"),
                                       break_max = as.Date("2000-03-01"),
                                       open_first = FALSE,
                                       open_last = FALSE,
                                       include_na = FALSE))
})

test_that("infer_lab_calendar_months gives correct errors/messages with invalid inputs", {
    infer_lab_calendar_months <- demarray:::infer_lab_calendar_months
    labels <- c(NA, NA)
    expect_identical(infer_lab_calendar_months(labels),
                     "'labels' has no non-NA elements")
    labels <- c("wrong", NA)
    expect_identical(infer_lab_calendar_months(labels),
                     "\"wrong\" not a valid label for period of one month")
    labels <- c("<2000 Jan", "<2000 Feb", "2000 Mar")
    expect_identical(infer_lab_calendar_months(labels),
                     "two different labels for period open on left : \"<2000 Jan\" and \"<2000 Feb\"")
    labels <- c("2000 Dec+", "2000 Feb+", NA)
    expect_identical(infer_lab_calendar_months(labels),
                     "two different labels for period open on right : \"2000 Feb+\" and \"2000 Dec+\"")
    labels <- c("<2005 Feb", "2004 Dec+")
    expect_identical(infer_lab_calendar_months(labels),
                     "periods \"<2005 Feb\" and \"2004 Dec+\" overlap")
    labels <- c("2010 Jul", "2005 Jun", "<2006 Jul")
    expect_identical(infer_lab_calendar_months(labels),
                     "periods \"<2006 Jul\" and \"2005 Jun\" overlap")
    labels <- c("2010 Jan", "2005 Aug", "2006 Aug+")
    expect_identical(infer_lab_calendar_months(labels),
                     "periods \"2010 Jan\" and \"2006 Aug+\" overlap")
})

test_that("infer_lab_durations_quarters gives correct answer with valid inputs", {
    infer_lab_durations_quarters <- demarray:::infer_lab_durations_quarters
    labels <- c("12q", "3q", "15q+", "1q")
    expect_identical(infer_lab_durations_quarters(labels),
                     LabDurationsQuarters(break_min = 1L,
                                          break_max = 15L,
                                          open_last = TRUE,
                                          include_na = FALSE))
    labels <- c(NA, "12q", "3q")
    expect_identical(infer_lab_durations_quarters(labels),
                     LabDurationsQuarters(break_min = 3L,
                                          break_max = 13L,
                                          open_last = FALSE,
                                          include_na = TRUE))
    labels <- "4q+"
    expect_identical(infer_lab_durations_quarters(labels),
                     LabDurationsQuarters(break_min = 4L,
                                          break_max = 4L,
                                          open_last = TRUE,
                                          include_na = FALSE))
    labels <- "10q"
    expect_identical(infer_lab_durations_quarters(labels),
                     LabDurationsQuarters(break_min = 10L,
                                          break_max = 11L,
                                          open_last = FALSE,
                                          include_na = FALSE))
})

test_that("infer_lab_durations_quarters gives correct errors/messages with invalid inputs", {
    infer_lab_durations_quarters <- demarray:::infer_lab_durations_quarters
    labels <- c(NA, NA)
    expect_identical(infer_lab_durations_quarters(labels),
                     "'labels' has no non-NA elements")
    labels <- c(NA, "wrong")
    expect_identical(infer_lab_durations_quarters(labels),
                     "\"wrong\" not a valid label for interval of one quarter")
    labels <- c(NA, "4q+", "3q+")
    expect_identical(infer_lab_durations_quarters(labels),
                     "two different labels for interval open on right : \"3q+\" and \"4q+\"")
    labels <- c(NA, "3q", "3q+")
    expect_identical(infer_lab_durations_quarters(labels),
                     "intervals \"3q\" and \"3q+\" overlap")
})

test_that("infer_lab_durations_months gives correct answer with valid inputs", {
    infer_lab_durations_months <- demarray:::infer_lab_durations_months
    labels <- c("12m", "3m", "15m+", "1m")
    expect_identical(infer_lab_durations_months(labels),
                     LabDurationsMonths(break_min = 1L,
                                        break_max = 15L,
                                        open_last = TRUE,
                                        include_na = FALSE))
    labels <- c(NA, "12m", "3m")
    expect_identical(infer_lab_durations_months(labels),
                     LabDurationsMonths(break_min = 3L,
                                        break_max = 13L,
                                        open_last = FALSE,
                                        include_na = TRUE))
    labels <- "4m+"
    expect_identical(infer_lab_durations_months(labels),
                     LabDurationsMonths(break_min = 4L,
                                        break_max = 4L,
                                        open_last = TRUE,
                                        include_na = FALSE))
    labels <- "10m"
    expect_identical(infer_lab_durations_months(labels),
                     LabDurationsMonths(break_min = 10L,
                                        break_max = 11L,
                                        open_last = FALSE,
                                        include_na = FALSE))
})

test_that("infer_lab_durations_months gives correct errors/messages with invalid inputs", {
    infer_lab_durations_months <- demarray:::infer_lab_durations_months
    labels <- c(NA, NA)
    expect_identical(infer_lab_durations_months(labels),
                     "'labels' has no non-NA elements")
    labels <- c(NA, "wrong")
    expect_identical(infer_lab_durations_months(labels),
                     "\"wrong\" not a valid label for interval of one month")
    labels <- c(NA, "4m+", "3m+")
    expect_identical(infer_lab_durations_months(labels),
                     "two different labels for interval open on right : \"3m+\" and \"4m+\"")
    labels <- c(NA, "3m", "3m+")
    expect_identical(infer_lab_durations_months(labels),
                     "intervals \"3m\" and \"3m+\" overlap")
})

