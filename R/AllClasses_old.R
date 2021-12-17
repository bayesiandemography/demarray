
## DemographicArray ----------------------------------------------------------------------

validity_DemographicArray <- function(object) {
    dim <- dim(object)
    dimtypes <- object@dimtypes
    ## is numeric
    val <- demcheck::chk_is_numeric(x = object,
                                    name = "object")
    if (!isTRUE(val))
        return(val)
    ## has valid dimnames
    val <- demcheck::chk_names_dimnames_complete(x = object,
                                                 name = "object")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_dimnames_complete(x = object,
                                           name = "object")
    if (!isTRUE(val))
        return(val)
    dimnames <- dimnames(object)
    names <- names(dimnames)
    ## dimtypes valid (for the moment we are not enforcing
    ## the requirement that origin, destination, parent,
    ## and child dimensions have their pair present)
    val <- demcheck::chk_member_dimtype(x = dimtypes,
                                        name = "dimtypes")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_no_names(x = dimtypes,
                                  name = "dimtypes")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_dimtypes_mutually_compatible(dimtypes)
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_names_pairs_suffix(dimtypes = dimtypes,
                                            names = names)
    if (!isTRUE(val))
        return(val)
    ## dim and dimtypes consistent
    val <- demcheck::chk_length_same(x1 = dimtypes,
                                     x2 = dim,
                                     name1 = "dimtypes",
                                     name2 = "dim")
    if (!isTRUE(val))
        return(val)
    TRUE
}

#' An S4 class to represent a demographic array
#'
#' Differences from ordinary array:
#'
#' \itemize{
#'   \item \code{drop} is \code{FALSE} by default
#' }
#' 
#'
#' @slot dimtypes Character vector
#'
#' @export
setClass("DemographicArray",
         contains = "array",
         slots = c(dimtypes = "character"),
         validity = validity_DemographicArray)

#' @rdname DemographicArray
#' @export
setClass("Counts",
         contains = "DemographicArray")

#' @rdname DemographicArray
#' @export
setClass("Values",
         contains = "DemographicArray")


## Labels -------------------------------------------------------------------

#' S4 classes to represent information in labels
#'
#' S4 classes to hold information extracted from labels.
#' End users would not normally interact directly with
#' these classes.
#'
#' @slot values Vector holding information for each label.
#' @slot include_na Logical. Whether to append an
#' \code{NA} to the labels.
#' 
#' @keywords internal
#'
#' @name Labels-class
NULL

#' @rdname Labels-class
setClass("Labels",
         contains = "VIRTUAL",
         slots = c(values = "vector",
                   include_na = "logical"),
         validity = function(object) {
             include_na <- object@include_na
             val <- demcheck::chk_is_logical_flag(x = include_na,
                                                  name = "include_na")
             if (!isTRUE(val))
                 return(val)
             TRUE
         })


## Categories -----------------------------------------------------------------

## HAS_TESTS
#' @rdname Labels-class
setClass("Categories",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_categories(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })
         

## Specialised classes --------------------------------------------------------

## HAS_TESTS
#' @rdname Labels-class
setClass("Triangles",
         contains = "Categories",
         prototype = prototype(values = c("Lower", "Upper")),
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_triangles(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## HAS_TESTS
#' @rdname Labels-class
setClass("Direction",
         contains = "Categories",
         prototype = prototype(values = c("In", "Out")),
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_direction(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## HAS_TESTS
#' @rdname Labels-class
setClass("Quantiles",
         contains = "Categories",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_quantiles(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })


## Numeric --------------------------------------------------------------------

## HAS_TESTS
#' @rdname Labels-class
setClass("Integers",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_integers(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })


## HAS_TESTS
#' @rdname Labels-class
setClass("Quantities",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_quantities(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })


## HAS_TESTS
#' @rdname Labels-class
setClass("Intervals",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_intervals(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })


## Dates ----------------------------------------------------------------------

## HAS_TESTS
#' @rdname Labels-class
setClass("Quarters",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_quarters(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## HAS_TESTS
#' @rdname Labels-class
setClass("Months",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_months(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## HAS_TESTS
#' @rdname Labels-class
setClass("Dates",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_dates(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## HAS_TESTS
#' @rdname Labels-class
setClass("DateRanges",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_label_values_dateranges(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })


####################


validity_CalendarQuarters <- function(object) {
    for (name in c("break_min", "break_max")) {
        x <- methods::slot(object, name)
        val <- demcheck::chk_first_day_unit_scalar(x = x,
                                                   name = name,
                                                   unit = "quarter")
        if (!isTRUE(val))
            return(val)
    }
    TRUE
}

## HAS_TESTS
#' @rdname Labels-class
setClass("LabCalendarQuarters",
         contains = "LabCalendar",
         validity = validity_CalendarQuarters)

validity_CalendarMonths <- function(object) {
    for (name in c("break_min", "break_max")) {
        x <- methods::slot(object, name)
        val <- demcheck::chk_first_day_unit_scalar(x = x,
                                                   name = name,
                                                   unit = "month")
        if (!isTRUE(val))
            return(val)
    }
    TRUE
}

## HAS_TESTS
#' @rdname Labels-class
setClass("LabCalendarMonths",
         contains = "LabCalendar",
         validity = validity_CalendarMonths)


## Durations ------------------------------------------------------------------

## contains the breaks between intervals

validity_Durations <- function(object) {
    break_min <- object@break_min
    break_max <- object@break_max
    open_last <- object@open_last
    for (name in c("break_min", "break_max")) {
        x <- methods::slot(object, name)
        val <- demcheck::chk_length_1(x = x,
                                         name = name)
        if (!isTRUE(val))
            return(val)
        val <- demcheck::chk_not_na_scalar(x = x,
                                              name = name)
        if (!isTRUE(val))
            return(val)
    }
    val <- demcheck::chk_is_logical_flag(x = open_last,
                                         name = "open_last")
    if (!isTRUE(val))
        return(val)
    if (break_max < break_min)
        return(gettextf("'%s' [%s] less than '%s' [%s]",
                        "break_max", break_max, "break_min", break_min))
    if (break_min == break_max) {
        if (!open_last)
            return(gettextf("'%s' [%s] equals '%s' but '%s' is %s",
                            "break_min",
                            break_min,
                            "break_max",
                            "open_last",
                            "FALSE"))
    }
    TRUE
}

#' @rdname Labels-class
setClass("LabDurations",
         contains = c("Labels",
                      "VIRTUAL"),
         slots = c(break_min = "integer",
                   break_max = "integer",
                   open_last = "logical"),
         validity = validity_Durations)

## HAS_TESTS
#' @rdname Labels-class
setClass("LabDurationsQuarters",
         contains = "LabDurations")

## HAS_TESTS
#' @rdname Labels-class
setClass("LabDurationsMonths",
         contains = "LabDurations")
