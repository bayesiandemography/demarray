

## Mixin Classes ---------------------------------------------------------------

validity_TimeUnitMixin <- function(object) {
    time_unit <- object@time_unit
    val <- demcheck::chk_member_time_unit(x = time_unit,
                                          name = "time_unit")
    if (is.character(val))
        return(val)
    TRUE
}

setClass("TimeUnitMixin",
         contains = "VIRTUAL",
         slots = c(time_unit = "character"),
         validity = validity_TimeUnitMixin)



validity_IntegerDimvaluesMixin <- function(object) {
    dimvalues <- object@dimvalues
    ## Strictly increasing
    val <- demcheck::chk_is_strictly_increasing(x = dimvalues,
                                             name = "dimvalues")
    if (is.character(val))
        return(val)
    TRUE
}

setClass("IntegerDimvaluesMixin",
         contains = "VIRTUAL",
         validity = validity_IntegerDimvaluesMixin)



validity_DateDimvaluesMixin <- function(object) {
    dimvalues <- object@dimvalues
    time_unit <- object@time_unit
    ## Dates fall on first day of consecutive time units
    val <- demcheck::chk_is_first_day_time_unit_consec(x = dimvalues,
                                                       name = "dimvalues",
                                                       time_unit = time_unit)
    if (is.character(val))
        return(val)
    TRUE
}

setClass("DateDimvaluesMixin",
         contains = "VIRTUAL",
         validity = validity_DateDimvaluesMixin)



validity_DurationDimvaluesMixin <- function(object) {
    dimvalues <- object@dimvalues
    ## Consecutive integers
    val <- demcheck::chk_is_integer_consec(x = dimvalues,
                                           name = "dimvalues")
    if (is.character(val))
        return(val)
    TRUE
}

setClass("DurationDimvaluesMixin",
         contains = "VIRTUAL",
         validity = validity_DurationDimvaluesMixin)


validity_IntervalsOpenMixin <- function(object) {
    is_open_left <- object@is_open_left
    is_open_right <- object@is_open_right
    dimvalues <- object@dimvalues
    for (name in c("is_open_left", "is_open_right")) {
        x <- slot(object, name)
        val <- demcheck::chk_is_logical_flag(x = x,
                                             name = name)
        if (is.character(val))
            return(val)
    }
    if ((length(dimvalues) == 0L) && (is_open_left || is_open_right))
        return(gettextf("'%s' has length %d but '%s' or '%s' is %s",
                        "dimvalues", 0L, "is_open_left", "is_open_right", TRUE))
    if ((length(dimvalues) == 1L) && (!is_open_left && !is_open_right))
        return(gettextf("'%s' has length %d but '%s' and '%s' are both %s",
                        "dimvalues", 1L, "is_open_left", "is_open_right", FALSE))
}

setClass("IntervalsOpenMixin",
         contains = "VIRTUAL",
         slots = c(is_open_left = "logical",
                   is_open_right = "logical"),
         validity = validity_IntervalsOpenMixin)


## 'is_age' slot needed for constructing
## labels: "0-4" (age) vs "0-5" (time, cohort)
validity_IsAgeMixin <- function(object) {
    is_age <- object@is_age
    val <- demcheck::chk_is_logical_flag(x = is_age,
                                         name = name)
    if (is.character(val))
        return(val)
    TRUE
}

setClass("IsAgeMixin",
         contains = "VIRTUAL",
         slots = c(is_age = "logical"),
         validity = validity_IsAgeMixin)
         

## Class Hierarchy -------------------------------------------------------------

setClass("DimScale",
         contains = "VIRTUAL")

setClass("Points",
         contains = "VIRTUAL")

## Used with dimtypes "age" or "time"
setClass("PointsInteger",
         slots = c(dimvalues = "integer"),
         contains = c("Points",
                      "IntegerDimvaluesMixin"))

## Used with dimtype "time"
setClass("PointsDate",
         slots = c(dimvalues = "Date"),
         contains = c("Points",
                      "DateDimvaluesMixin",
                      "TimeUnitMixin"))

## Used with dimtype "age"
setClass("PointsDuration",
         slots = c(dimvalues = "integer"),
         contains = c("Points",
                      "DurationDimvaluesMixin",
                      "TimeUnitMixin"))

setClass("Intervals",
         contains = c("VIRTUAL",
                      "IntervalsOpenMixin"))

## Used with dimtypes "age", "time", or "cohort"
setClass("IntervalsInteger",
         slots = c(dimvalues = "integer"),
         contains = c("Intervals",
                      "IntegerDimvaluesMixin",
                      "IsAgeMixin"))

## Used with dimtypes "time" or "cohort"
setClass("IntervalsDate",
         slots = c(dimvalues = "Date"),
         contains = c("Intervals",
                      "DateDimvaluesMixin",
                      "TimeUnitMixin"))

## Used with dimtype "age"
setClass("IntervalsDuration",
         slots = c(dimvalues = "integer"),
         contains = c("Intervals",
                      "DurationDimvaluesMixin",
                      "TimeUnitMixin"))





