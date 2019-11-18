
context("Array-generator")

## Array method for array -----------------------------------------------------

test_that("Array method for array gives correct answers with valid input", {
    x <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("0-4", "5+"),
                               time = c("2000-2005", "2005-2010")))
    ## all defaults
    ans <- Array(x)
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "age", time = "time"))
    expect_identical(classif(ans), c(age = NA_character_, time = NA_character_))
    expect_identical(dimnames(ans), dimnames(x))
    ## 'dimtype' and 'classif' supplied
    ans <- Array(x, dimtypes = c(age = "state"), classif = c(time = "Periods"))
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "state", time = "time"))
    expect_identical(classif(ans), c(age = NA_character_, time = "Periods"))
    expect_identical(dimnames(ans), dimnames(x))
    ## zero-length dimension
    x <- array(0L,
               dim = c(0, 2),
               dimnames = list(age = character(),
                               time = c("2000-2005", "2005-2010")))
    ans <- Array(x, dimtypes = c(age = "state"), classif = c(time = "Periods"))
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "state", time = "time"))
    expect_identical(classif(ans), c(age = NA_character_, time = "Periods"))
    expect_identical(dim(ans), dim(x))
    expect_identical(dimnames(ans), dimnames(x))
})

test_that("Array method for array throws correct errors with invalid input", {
    x <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("0-4", "5+"),
                               time = c("2000-2005", "2005-2010")))
    expect_error(Array(x, dimtypes = c(wrong = "age"), classif = c(time = "Periods")),
                 "'dimtypes' has element named \"wrong\" but 'x' does not have dimension named \"wrong\"")
    expect_error(Array(x, dimtypes = c(age = "age"), classif = c(wrong = "Periods")),
                 "'classif' has element named \"wrong\" but 'x' does not have dimension named \"wrong\"")
})


## Array method for Array -----------------------------------------------------

test_that("Array method for Array gives correct answers with valid input", {
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("0-4", "5+"),
                               time = c("2000-2005", "2005-2010")))
    ## all defaults
    x <- Array(a)
    ans <- Array(x)
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "age", time = "time"))
    expect_identical(classif(ans), c(age = NA_character_, time = NA_character_))
    expect_identical(dimnames(ans), dimnames(x))
    ## 'dimtype' and 'classif' supplied in original array
    x <- Array(a, dimtypes = c(age = "state"), classif = c(time = "Periods"))
    ans <- Array(x)
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "state", time = "time"))
    expect_identical(classif(ans), c(age = NA_character_, time = "Periods"))
    expect_identical(dimnames(ans), dimnames(x))
    ## 'dimtype' and 'classif' supplied in original array and in later call
    x <- Array(a, dimtypes = c(age = "state"), classif = c(time = "Periods"))
    ans <- Array(x, dimtypes = c(age = "age", time = "cohort"),
                 classif = c(time = "Periods2"))
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "age", time = "cohort"))
    expect_identical(classif(ans), c(age = NA_character_, time = "Periods2"))
    expect_identical(dimnames(ans), dimnames(x))
    ## zero-length dimension
    a <- array(0L,
               dim = c(0, 2),
               dimnames = list(age = character(),
                               time = c("2000-2005", "2005-2010")))
    x <- Array(a)
    ans <- Array(x, dimtypes = c(age = "state"), classif = c(time = "Periods"))
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "state", time = "time"))
    expect_identical(classif(ans), c(age = NA_character_, time = "Periods"))
    expect_identical(dim(ans), dim(x))
    expect_identical(dimnames(ans), dimnames(x))
})
    

    
