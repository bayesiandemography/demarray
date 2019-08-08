#' @export
Intervals <- function(dimvalues, time_unit, is_open_left, is_open_right, is_age) {
    demcheck::err_is_not_na_vector(x = dimvalues,
                                   name = "dimvalues")
    demcheck::err_is_logical_flag(x = is_open_left,
                                  name = "is_open_left")
    demcheck::err_is_logical_flag(x = is_open_right,
                                  name = "is_open_right")
    demcheck::err_is_logical_flag(x = is_age,
                                  name = "is_age")
    if (is.null(time_unit)) { # IntervalsInteger - age, time, or cohort
        demcheck::err_is_integer_equiv(x = dimvalues,
                                       name = "dimvalue")
        dimvalues <- as.integer(dimvalues)        
        ans <- methods::new("IntervalsInteger",
                            dimvalues = dimvalues,
                            is_open_left = is_open_left,
                            is_open_right = is_open_right,
                            is_age = is_age)
    }
    else { # IntervalsDuration or IntervalsDate
        demcheck::err_member_time_unit(x = time_unit,
                                       name = "time_unit")
        if (is_age) { # IntervalsDuration - age
            demcheck::err_is_integer_equiv(x = dimvalues,
                                           name = "dimvalues")
            dimvalues <- as.integer(dimvalues)
            ans <- methods::new("IntervalsDuration",
                                dimvalues = dimvalues,
                                time_unit = time_unit,
                                is_open_left = is_open_left,
                                is_open_right = is_open_right)
        }
        else { # IntervalsDate - time or cohort
            demcheck::err_is_date_equiv(x = dimvalues,
                                        name = "dimvalues")
            dimvalues <- as.Date(dimvalues)
            ans <- methods::new("IntervalsDate",
                                dimvalues = dimvalues,
                                time_unit = time_unit,
                                is_open_left = is_open_left,
                                is_open_right = is_open_right)
        }
    }
    ans
}

#' @export
Points <- function(dimvalues, time_unit) {
    demcheck::err_is_not_na_vector(x = dimvalues,
                                   name = "dimvalues")
    if (is.null(time_unit)) { # PointsInteger - age or time
        demcheck::err_is_integer_equiv(x = dimvalues,
                                       name = "dimvalue")
        dimvalues <- as.integer(dimvalues)        
        ans <- methods::new("PointsInteger",
                            dimvalues = dimvalues)
    }
    else { # PointsDuration or PointsDate
        demcheck::err_member_time_unit(x = time_unit,
                                       name = "time_unit")
        is_int <- isTRUE(demcheck::chk_is_integer_equiv(x = dimvalues,
                                                        name = "dimvalues"))
        is_date <- isTRUE(demcheck::chk_is_date_equiv(x = dimvalues,
                                                      name = "dimvalues"))
        if (is_int) { # PointsDuration - age
            dimvalues <- as.integer(dimvalues)
            ans <- methods::new("PointsDuration",
                                dimvalues = dimvalues,
                                time_unit = time_unit)
        }
        else if (is_date) { # PointsDate - time
            dimvalues <- as.Date(dimvalues)
            ans <- methods::new("IntervalsDate",
                                dimvalues = dimvalues,
                                time_unit = time_unit,
                                is_open_left = is_open_left,
                                is_open_right = is_open_right)
        }
        else {
            stop(gettextf("'%s' are not integers or dates",
                          "dimvalues"))
        }
    }
    ans
}
    
    





        
    
