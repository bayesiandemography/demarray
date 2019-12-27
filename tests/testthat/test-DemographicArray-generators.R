
context("DemographicArray-generators")

## DemographicArray method for array -----------------------------------------------------

test_that("DemographicArray method for array gives correct answers with valid input", {
    x <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("0-4", "5+"),
                               time = c("2000-2005", "2005-2010")))
    ## all defaults
    ans <- DemographicArray(x)
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "age", time = "time"))
    expect_identical(dimnames(ans), dimnames(x))
    ## 'dimtype' supplied
    ans <- DemographicArray(x, dimtypes = c(age = "attribute"))
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "attribute", time = "time"))
    expect_identical(dimnames(ans), dimnames(x))
    ## zero-length dimension
    x <- array(0L,
               dim = c(0, 2),
               dimnames = list(age = character(),
                               time = c("2000-2005", "2005-2010")))
    ans <- DemographicArray(x, dimtypes = c(age = "attribute"))
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "attribute", time = "time"))
    expect_identical(dim(ans), dim(x))
    expect_identical(dimnames(ans), dimnames(x))
})

test_that("DemographicArray method for array throws correct errors with invalid input", {
    x <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("0-4", "5+"),
                               time = c("2000-2005", "2005-2010")))
    expect_error(DemographicArray(x, dimtypes = c(wrong = "age")),
                 "'dimtypes' has element named \"wrong\" but 'x' does not have dimension named \"wrong\"")
})


## DemographicArray method for DemographicArray -----------------------------------------------------

test_that("DemographicArray method for DemographicArray gives correct answers with valid input", {
    a <- array(1:4,
               dim = c(2, 2),
               dimnames = list(age = c("0-4", "5+"),
                               time = c("2000-2005", "2005-2010")))
    ## all defaults
    x <- DemographicArray(a)
    ans <- DemographicArray(x)
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "age", time = "time"))
    expect_identical(dimnames(ans), dimnames(x))
    ## 'dimtype' supplied in original array
    x <- DemographicArray(a, dimtypes = c(age = "attribute"))
    ans <- DemographicArray(x)
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "attribute", time = "time"))
    expect_identical(dimnames(ans), dimnames(x))
    ## 'dimtype' supplied in original array and in later call
    x <- DemographicArray(a, dimtypes = c(age = "attribute"))
    ans <- DemographicArray(x, dimtypes = c(age = "age", time = "cohort"))
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "age", time = "cohort"))
    expect_identical(dimnames(ans), dimnames(x))
    ## zero-length dimension
    a <- array(0L,
               dim = c(0, 2),
               dimnames = list(age = character(),
                               time = c("2000-2005", "2005-2010")))
    x <- DemographicArray(a)
    ans <- DemographicArray(x, dimtypes = c(age = "attribute"))
    expect_true(validObject(ans))
    expect_identical(dimtypes(ans), c(age = "attribute", time = "time"))
    expect_identical(dim(ans), dim(x))
    expect_identical(dimnames(ans), dimnames(x))
})
    

    
