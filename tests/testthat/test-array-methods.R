
context("array-methods.R")

test_that("limits works with arrays with complete dimnames", {
    x <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                               sex = c("Male", "Female")))
    expect_true(demcheck::chk_dimnames_complete(x = x, name = "x"))
    ans_obtained <- limits(x)
    ans_expected <- data.frame(age = c("0-4", "10+"),
                               sex = c("Male", "Female"),
                               row.names = c("first", "last"))
    expect_identical(ans_obtained, ans_expected)
    x <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = c("0-4", "5-9", NA),
                               sex = c("Male", "Female")))
    expect_true(demcheck::chk_dimnames_complete(x = x, name = "x"))
    ans_obtained <- limits(x)
    ans_expected <- data.frame(age = c("0-4", NA),
                               sex = c("Male", "Female"),
                               row.names = c("first", "last"))
    expect_identical(ans_obtained, ans_expected)
    x <- array(0,
               dim = c(3, 0),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                               sex = NULL))
    expect_true(demcheck::chk_dimnames_complete(x = x, name = "x"))
    ans_obtained <- limits(x)
    ans_expected <- data.frame(age = c("0-4", "10+"),
                               sex = as.character(c(NA, NA)),
                               row.names = c("first", "last"))
    expect_identical(ans_obtained, ans_expected)
    x <- array(0,
               dim = c(3, 1),
               dimnames = list(age = c("0-4", "5-9", "10+"),
                               sex = "Female"))
    expect_true(demcheck::chk_dimnames_complete(x = x, name = "x"))
    ans_obtained <- limits(x)
    ans_expected <- data.frame(age = c("0-4", "10+"),
                               sex = as.character(c("Female", "Female")),
                               row.names = c("first", "last"))
    expect_identical(ans_obtained, ans_expected)    
})

test_that("limits works with arrays with incomplete dimnames", {
    x <- array(1:6,
               dim = c(3, 2))
    ans_obtained <- limits(x)
    ans_expected <- NULL
    expect_identical(ans_obtained, ans_expected)
    x <- array(1:6,
               dim = c(3, 2),
               dimnames = list(age = NULL,
                               sex = c("Male", "Female")))
    ans_obtained <- limits(x)
    ans_expected <- data.frame(age = as.character(c(NA, NA)),
                               sex = c("Male", "Female"),
                               row.names = c("first", "last"))
    expect_identical(ans_obtained, ans_expected)
})

