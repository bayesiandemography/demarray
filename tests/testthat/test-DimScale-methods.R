
context("DimScale-methods")


## make_dimscale_labels

test_that("'make_dimscale_labels' works with PointsInteger", {
    ## Uses generic
    make_dimscale_labels <- demarray:::make_dimscale_labels
    object_age <- Points(dimvalues = 0:5,
                         time_unit = NULL,
                         is_age = TRUE)
    expect_identical(make_dimscale_labels(object_age),
                     c("0", "1", "2", "3", "4", "5"))
    object_time <- Points(dimvalues = c(2015L, 2020L, 2025L),
                          time_unit = NULL,
                          is_age = FALSE)
    expect_identical(make_dimscale_labels(object_time),
                     c("2015", "2020", "2025"))
    object_empty <- Points(dimvalues = integer(),
                          time_unit = NULL,
                          is_age = FALSE)
    expect_identical(make_dimscale_labels(object_empty),
                     character())
})


test_that("'make_dimscale_labels' works with PointsDuration", {
    ## Uses generic
    make_dimscale_labels <- demarray:::make_dimscale_labels
    object_age <- Points(dimvalues = 0:5,
                         time_unit = NULL,
                         is_age = TRUE)
    expect_identical(make_dimscale_labels(object_age),
                     c("0", "1", "2", "3", "4", "5"))
    object_time <- Points(dimvalues = c(2015L, 2020L, 2025L),
                          time_unit = NULL,
                          is_age = FALSE)
    expect_identical(make_dimscale_labels(object_time),
                     c("2015", "2020", "2025"))
    object_empty <- Points(dimvalues = integer(),
                          time_unit = NULL,
                          is_age = FALSE)
    expect_identical(make_dimscale_labels(object_empty),
                     character())
})

    
