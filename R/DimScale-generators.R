
Intervals <- function(dimvalues, unit, is_open_left, is_open_right, is_age) {
    demcheck::err_is_not_na_vector(x = dimvalues,
                                   name = "dimvalues")
    demcheck::err_is_logical_flag(x = is_open_left,
                                  name = "is_open_left")
    demcheck::err_is_logical_flag(x = is_open_right,
                                  name = "is_open_right")
    demcheck::err_is_logical_flag(x = is_age,
                                  name = "is_age")
    if (is.null(unit)) { # IntervalsInteger - age, time, or cohort
        demcheck::err_is_integer_equiv_vector(x = dimvalues,
                                              name = "dimvalue")
        dimvalues <- as.integer(dimvalues)        
        ans <- methods::new("IntervalsInteger",
                            dimvalues = dimvalues,
                            is_open_left = is_open_left,
                            is_open_right = is_open_right,
                            is_age = is_age)
    }
    else { # IntervalsDuration or IntervalsDate
        demcheck::err_member_unit(x = unit,
                                       name = "unit")
        if (is_age) { # IntervalsDuration - age
            demcheck::err_is_integer_equiv_vector(x = dimvalues,
                                                  name = "dimvalues")
            dimvalues <- as.integer(dimvalues)
            ans <- methods::new("IntervalsDuration",
                                dimvalues = dimvalues,
                                unit = unit,
                                is_open_left = is_open_left,
                                is_open_right = is_open_right)
        }
        else { # IntervalsDate - time or cohort
            demcheck::err_is_date_equiv(x = dimvalues,
                                        name = "dimvalues")
            dimvalues <- as.Date(dimvalues)
            ans <- methods::new("IntervalsDate",
                                dimvalues = dimvalues,
                                unit = unit,
                                is_open_left = is_open_left,
                                is_open_right = is_open_right)
        }
    }
    ans
}


Points <- function(dimvalues, unit, is_age) {
    demcheck::err_is_not_na_vector(x = dimvalues,
                                   name = "dimvalues")
    demcheck::err_is_logical_flag(x = is_age,
                                  name = "is_age")
    if (is.null(unit)) { # PointsInteger - age or time
        demcheck::err_is_integer_equiv_vector(x = dimvalues,
                                              name = "dimvalue")
        dimvalues <- as.integer(dimvalues)        
        ans <- methods::new("PointsInteger",
                            dimvalues = dimvalues)
    }
    else { # PointsDuration or PointsDate
        demcheck::err_member_unit(x = unit,
                                       name = "unit")
        if (is_age) { # PointsDuration - age
            demcheck::err_is_integer_equiv_vector(x = dimvalues,
                                                  name = "dimvalues")
            dimvalues <- as.integer(dimvalues)
            ans <- methods::new("PointsDuration",
                                dimvalues = dimvalues,
                                unit = unit)
        }
        else { # PointsDate - time
            demcheck::err_is_date_equiv(x = dimvalues,
                                        name = "dimvalues")
            dimvalues <- as.Date(dimvalues)
            ans <- methods::new("PointsDate",
                                dimvalues = dimvalues,
                                unit = unit)
        }
    }
    ans
}
    

