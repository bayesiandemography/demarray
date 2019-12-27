
context("Labels-methods")

## labels_imply_quant_scale -----------------------------------------------------------------

test_that("labels_imply_quant_scale works with LabCategories", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabCategories <- demarray:::LabCategories
    x <- LabCategories(labels = c("a", "b", "c"),
                       include_na = TRUE)
    expect_false(labels_imply_quant_scale(x))
})

test_that("labels_imply_quant_scale works with LabTriangles", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabTriangles <- demarray:::LabTriangles
    x <- LabTriangles(include_na = FALSE)
    expect_false(labels_imply_quant_scale(x))
})

test_that("labels_imply_quant_scale works with LabPool", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabPool <- demarray:::LabPool
    x <- LabPool(include_na = FALSE)
    expect_false(labels_imply_quant_scale(x))
})

test_that("labels_imply_quant_scale works with LabQuantiles", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabQuantiles <- demarray:::LabQuantiles
    x <- LabQuantiles(labels = c("2.5%", "50%", "97.5%"),
                      include_na = FALSE)
    expect_false(labels_imply_quant_scale(x))
})

test_that("labels_imply_quant_scale works with LabIntegers", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabIntegers <- demarray:::LabIntegers
    x <- LabIntegers(int_min = 0L,
                     int_max = 100L,
                     include_na = FALSE)
    expect_true(labels_imply_quant_scale(x))
})

test_that("labels_imply_quant_scale works with LabGroupedIntEnumerations", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabGroupedIntEnumerations <- demarray:::LabGroupedIntEnumerations
    x <- LabGroupedIntEnumerations(breaks = c(-10L, 0L, 5L, 100L),
                                   open_first = TRUE,
                                   open_last = TRUE,
                                   include_na = FALSE)
    expect_true(labels_imply_quant_scale(x))
})

test_that("labels_imply_quant_scale works with LabGroupedIntEndpoints", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabGroupedIntEndpoints <- demarray:::LabGroupedIntEndpoints
    x <- LabGroupedIntEndpoints(breaks = c(-10L, 0L, 5L, 100L),
                                open_first = TRUE,
                                open_last = TRUE,
                                include_na = FALSE)
    expect_true(labels_imply_quant_scale(x))
})

test_that("labels_imply_quant_scale works with LabCalendarQuarters", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabCalendarQuarters <- demarray:::LabCalendarQuarters
    x <- LabCalendarQuarters(break_min = as.Date("2000-04-01"),
                             break_max = as.Date("2002-01-01"),
                             open_first = TRUE,
                             open_last = TRUE,
                             include_na = FALSE)
    expect_true(labels_imply_quant_scale(x))
})

test_that("labels_imply_quant_scale works with LabCalendarMonths", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabCalendarMonths <- demarray:::LabCalendarMonths
    x <- LabCalendarMonths(break_min = as.Date("2000-04-01"),
                             break_max = as.Date("2002-01-01"),
                             open_first = TRUE,
                             open_last = TRUE,
                             include_na = FALSE)
    expect_true(labels_imply_quant_scale(x))
})

test_that("labels_imply_quant_scale works with LabDurationsQuarters", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabDurationsQuarters <- demarray:::LabDurationsQuarters
    x <- LabDurationsQuarters(break_min = 0L,
                              break_max = 120L,
                              open_last = TRUE,
                              include_na = FALSE)
    expect_true(labels_imply_quant_scale(x))
})

test_that("labels_imply_quant_scale works with LabDurationsMonths", {
    labels_imply_quant_scale <- demarray:::labels_imply_quant_scale
    LabDurationsMonths <- demarray:::LabDurationsMonths
    x <- LabDurationsMonths(break_min = 0L,
                            break_max = 120L,
                            open_last = TRUE,
                            include_na = FALSE)
    expect_true(labels_imply_quant_scale(x))
})


## make_labels -----------------------------------------------------------------

test_that("make_labels works with LabCategories", {
    make_labels <- demarray:::make_labels
    LabCategories <- demarray:::LabCategories
    labels <- c("a", "b", "c")
    x <- LabCategories(labels = labels,
                       include_na = TRUE)
    expect_identical(make_labels(x),
                     c(labels, NA))
})

test_that("make_labels works with LabTriangles", {
    make_labels <- demarray:::make_labels
    LabTriangles <- demarray:::LabTriangles
    x <- LabTriangles(include_na = FALSE)
    expect_identical(make_labels(x),
                     c("Lower", "Upper"))
})

test_that("make_labels works with LabPool", {
    make_labels <- demarray:::make_labels
    LabPool <- demarray:::LabPool
    x <- LabPool(include_na = FALSE)
    expect_identical(make_labels(x),
                     c("Ins", "Outs"))
})

test_that("make_labels works with LabQuantiles", {
    make_labels <- demarray:::make_labels
    LabQuantiles <- demarray:::LabQuantiles
    labels <- c("2.5%", "50%", "97.5%")
    x <- LabQuantiles(labels = labels,
                      include_na = FALSE)
    expect_identical(make_labels(x),
                     labels)
})

test_that("make_labels works with LabIntegers", {
    make_labels <- demarray:::make_labels
    LabIntegers <- demarray:::LabIntegers
    x <- LabIntegers(int_min = 0L,
                     int_max = 100L,
                     include_na = FALSE)
    expect_identical(make_labels(x),
                     as.character(0:100))
})

test_that("make_labels works with LabGroupedIntEnumerations", {
    make_labels <- demarray:::make_labels
    LabGroupedIntEnumerations <- demarray:::LabGroupedIntEnumerations
    x <- LabGroupedIntEnumerations(breaks = c(-10L, 0L, 5L, 100L),
                                   open_first = TRUE,
                                   open_last = TRUE,
                                   include_na = FALSE)
    expect_identical(make_labels(x),
                     c("<-10", "-10--1", "0-4", "5-99", "100+"))
})

test_that("make_labels works with LabGroupedIntEndpoints", {
    make_labels <- demarray:::make_labels
    LabGroupedIntEndpoints <- demarray:::LabGroupedIntEndpoints
    x <- LabGroupedIntEndpoints(breaks = c(-10L, 0L, 5L, 100L),
                                open_first = TRUE,
                                open_last = TRUE,
                                include_na = FALSE)
    expect_identical(make_labels(x),
                     c("<-10", "-10-0", "0-5", "5-100", "100+"))
})

test_that("make_labels works with LabCalendarQuarters", {
    make_labels <- demarray:::make_labels
    LabCalendarQuarters <- demarray:::LabCalendarQuarters
    x <- LabCalendarQuarters(break_min = as.Date("2000-04-01"),
                             break_max = as.Date("2002-01-01"),
                             open_first = TRUE,
                             open_last = TRUE,
                             include_na = FALSE)
    expect_identical(make_labels(x),
                     c("<2000 Q2", "2000 Q2", "2000 Q3", "2000 Q4",
                       "2001 Q1", "2001 Q2", "2001 Q3", "2001 Q4",
                       "2002 Q1+"))
})

test_that("make_labels works with LabCalendarMonths", {
    make_labels <- demarray:::make_labels
    LabCalendarMonths <- demarray:::LabCalendarMonths
    x <- LabCalendarMonths(break_min = as.Date("2000-04-01"),
                             break_max = as.Date("2000-07-01"),
                             open_first = TRUE,
                             open_last = TRUE,
                             include_na = FALSE)
    expect_identical(make_labels(x),
                     c("<2000 Apr",
                       "2000 Apr", "2000 May", "2000 Jun",
                       "2000 Jul+"))
})

test_that("make_labels works with LabDurationsQuarters", {
    make_labels <- demarray:::make_labels
    LabDurationsQuarters <- demarray:::LabDurationsQuarters
    x <- LabDurationsQuarters(break_min = 0L,
                              break_max = 120L,
                              open_last = TRUE,
                              include_na = FALSE)
    expect_identical(make_labels(x),
                     c(paste0(0:119, "q"), "120q+"))
})

test_that("make_labels works with LabDurationsMonths", {
    make_labels <- demarray:::make_labels
    LabDurationsMonths <- demarray:::LabDurationsMonths
    x <- LabDurationsMonths(break_min = 0L,
                            break_max = 120L,
                            open_last = TRUE,
                            include_na = FALSE)
    expect_identical(make_labels(x),
                     c(paste0(0:119, "m"), "120m+"))
})






