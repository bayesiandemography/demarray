
context("helper-functions")

test_that("infer_dimtypes works as expected", {
    expect_identical(infer_dimtypes(c("age", "age5", "age10yr", "sex", "year")),
                     c("age", "age", "age", "attribute", "time"))
    expect_identical(infer_dimtypes(c("duration", "unknown")),
                     c("age", "attribute"))
    expect_identical(infer_dimtypes(c("start", "end")),
                     c("attribute", "attribute"))
    expect_identical(infer_dimtypes(c("stage", "parity")),
                     c("attribute", "attribute"))
    expect_identical(infer_dimtypes(c("reg_orig", "reg_dest", "birth cohort")),
                     c("origin", "destination", "cohort"))
    expect_identical(infer_dimtypes(c("reg_dest", "birth cohort")),
                     c("destination", "cohort"))
    expect_identical(infer_dimtypes(c("ethnicity_parent", "ethnicity_child")),
                     c("parent", "child"))
    expect_identical(infer_dimtypes(c("Lexis triangle", "Lexis Triangles", "triangle", "Triangles")),
                     rep("triangle", 4))
})

test_that("'sort_durations' works", {
    sort_durations <- demarray:::sort_durations
    x <- c("10q", "50q+", "0q", NA, "5q")
    expect_identical(sort_durations(x),
                     x[c(3, 5, 1, 2, 4)])
    x <- c(NA, "15m+", "0m", "7m")
    expect_identical(sort_durations(x),
                     x[c(3, 4, 2, 1)])
    expect_identical(sort_durations(character()),
                     character())
})

test_that("'sort_intervals' works", {
    sort_intervals <- demarray:::sort_intervals
    x <- c("20-30", NA, "50", "-5", "<-5", "1", "20-30", "100+")
    expect_identical(sort_intervals(x),
                     c("<-5", "-5", "1", "20-30", "20-30", "50", "100+", NA))
    expect_identical(sort_intervals(character()),
                     character())
})

test_that("'sort_months' works", {
    sort_months <- demarray:::sort_months
    x <- c("2000 Jan", "1999 Oct", NA, "2010 Feb+", "<1900 Dec")
    expect_identical(sort_months(x),
                     x[c(5, 2, 1, 4, 3)])
    expect_identical(sort_months(character()),
                     character())
})

test_that("'sort_quantiles' works", {
    sort_quantiles <- demarray:::sort_quantiles
    x <- c("50%", NA, "0.001%", "5%", "99.03%")
    expect_identical(sort_quantiles(x),
                     x[c(3, 4, 1, 5, 2)])
    expect_identical(sort_quantiles(character()),
                     character())
})
