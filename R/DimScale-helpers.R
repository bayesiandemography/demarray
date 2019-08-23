
## make_dimscale_labels --------------------------------------------------------

## HAS_TESTS
make_dimscale_labels_points_duration <- function(dimvalues,
                                                 unit) {
    n <- length(dimvalues)
    if (n == 0L)
        return(character())
    if (unit == "month") {
        year <- dimvalues %/% 12L
        month <- dimvalues %% 12L
        suffix <- paste0(month, "m")
    }
    else if (unit == "quarter") {
        year <- dimvalues %/% 4L
        quarter <- dimvalues %% 4L
        suffix <- paste0(quarter, "q")
    }
    else
        stop(gettextf("can't handle time unit '%s'",
                      unit))
    paste0(year, "y", " ", suffix)
}

## HAS_TESTS
make_dimscale_labels_points_date <- function(dimvalues,
                                             unit) {
    n <- length(dimvalues)
    if (n == 0L)
        return(character())
    year <- format(dimvalues, format = "%Y")
    if (unit == "month") {
        suffix <- months(dimvalues,
                         abbreviate = TRUE)
    }
    else if (unit == "quarter") {
        suffix <- quarters(dimvalues)
    }
    else
        stop(gettextf("can't handle time unit '%s'",
                      unit))
    paste(year, suffix)
}


## HAS_TESTS
make_dimscale_labels_intervals_integer <- function(dimvalues,
                                                   is_open_left,
                                                   is_open_right,
                                                   is_age) {
    n <- length(dimvalues)
    if (n == 0L)
        return(character())
    ans_mid <- character(n - 1L)
    diff <- diff(dimvalues)
    is_single_unit <- diff == 1L
    ans_mid[is_single_unit] <- dimvalues[-n][is_single_unit]
    if (any(!is_single_unit)) {
        lower <- dimvalues[-n][!is_single_unit]
        upper <- dimvalues[-1L][!is_single_unit]
        if (is_age)
            upper <- upper - 1L
        ans_mid[!is_single_unit] <- paste(lower, upper, sep = "-")
    }
    if (is_open_left)
        ans_left <- paste0("<", dimvalues[[1L]])
    else
        ans_left <- NULL
    if (is_open_right)
        ans_right <- paste0(dimvalues[[n]], "+")
    else
        ans_right <- NULL
    c(ans_left, ans_mid, ans_right)
}

## HAS_TESTS
make_dimscale_labels_intervals_duration <- function(dimvalues,
                                                    unit,
                                                    is_open_left,
                                                    is_open_right) {
    n <- length(dimvalues)
    if (n == 0L)
        return(character())
    if (unit == "month") {
        year <- dimvalues %/% 12L
        month <- dimvalues %% 12L
        suffix <- paste0(month, "m")
    }
    else if (unit == "quarter") {
        year <- dimvalues %/% 4L
        quarter <- dimvalues %% 4L
        suffix <- paste0(quarter, "q")
    }
    else
        stop(gettextf("can't handle time unit '%s'",
                      unit))
    year <- paste0(year, "y")
    if (n == 1L)
        ans_mid <- NULL
    else
        ans_mid <- paste(year[-n], suffix[-n])
    if (is_open_left)
        ans_left <- paste0("<", year[[1L]], " ", suffix[[1L]])
    else
        ans_left <- NULL
    if (is_open_right)
        ans_right <- paste0(year[[n]], " ", suffix[[n]], "+")
    else
        ans_right <- NULL
    ans <- c(ans_left, ans_mid, ans_right)
    ans
}

## HAS_TESTS
make_dimscale_labels_intervals_date <- function(dimvalues,
                                                unit,
                                                is_open_left,
                                                is_open_right) {
    n <- length(dimvalues)
    if (n == 0L)
        return(character())
    year <- format(dimvalues, format = "%Y")
    if (unit == "month") {
        suffix <- months(dimvalues,
                         abbreviate = TRUE)
    }
    else if (unit == "quarter") {
        suffix <- quarters(dimvalues)
    }
    else
        stop(gettextf("can't handle time unit '%s'",
                      unit))
    if (n == 1L)
        ans_mid <- NULL
    else
        ans_mid <- paste(year[-n], suffix[-n])
    if (is_open_left)
        ans_left <- paste0("<", year[[1L]], " ", suffix[[1L]])
    else
        ans_left <- NULL
    if (is_open_right)
        ans_right <- paste0(year[[n]], " ", suffix[[n]], "+")
    else
        ans_right <- NULL
    ans <- c(ans_left, ans_mid, ans_right)
    ans
}





                
    
