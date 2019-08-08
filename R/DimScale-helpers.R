
make_dimscale_labels_intervals_duration <- function(dimvalues,
                                                    time_unit,
                                                    is_open_left,
                                                    is_open_right) {
    n <- length(dimvalues)
    if (n == 0L)
        return(character())
    if (time_unit == "month") {
        year <- dimvalues %/% 12L
        month <- year %% 12
        suffix <- paste0(month, "m")
    }
    else if (time_unit == "quarter") {
        year <- dimvalues %/% 4L
        quarter <- year %% 4L
        suffix <- paste0(quarter, "q")
    }
    else
        stop(gettextf("can't handle time unit '%s'",
                      time_unit))
    ans_mid <- paste0(year[-n], suffix[-n])
    if (is_open_left)
        ans_left <- paste0("<", ans_mid[[1L]])
    else
        ans_left <- NULL
    if (is_open_right)
        ans_right <- paste0(year[[n]], suffix[[n]], "+")
    else
        ans_right <- NULL
    ans <- c(ans_left, ans_mid, ans_right)
    ans
}


make_dimscale_labels_intervals_date <- function(dimvalues,
                                                time_unit,
                                                is_open_left,
                                                is_open_right) {
    n <- length(dimvalues)
    if (n == 0L)
        return(character())
    year <- format(dimvalues, format = "%Y")
    if (time_unit == "month") {
        suffix <- months(dimvalues,
                         abbreviate = TRUE)
    }
    else if (time_unit == "quarter") {
        suffix <- quarters(dimvalues)
    }
    else
        stop(gettextf("can't handle time unit '%s'",
                      time_unit))
    ans_mid <- paste(year[-n], suffix[-n])
    if (is_open_left)
        ans_left <- paste0("<", ans_mid[[1L]])
    else
        ans_left <- NULL
    if (is_open_right)
        ans_right <- paste0(year[n], " ", suffix[n], "+")
    else
        ans_right <- NULL
    ans <- c(ans_left, ans_mid, ans_right)
    ans
}




make_dimscale_labels_intervals_integer <- function(dimvalues,
                                                   is_open_left,
                                                   is_open_right,
                                                   is_age) {
    n <- length(dimvalues)
    ans_mid <- character(length = n - 1L)
    diff <- diff(dimvalues)
    is_single_unit <- diff == 1L
    ans_mid[is_single_unit] <- dimvalues[is_single_unit]
    if (any(!is_single_unit)) {
        lower <- dimvalues[!is_single_unit]
        upper <- dimvalues[which(!is_single_unit) + 1L]
        if (is_age)
            upper <- upper - 1L
        ans_mid[!is_single_unit] <- paste(upper, lower, sep = "-")
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
                
    
