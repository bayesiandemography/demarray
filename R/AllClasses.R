
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
    val <- demcheck::chk_dimtypes_pairs_suffix(dimtypes = dimtypes,
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
#' @slot labels Character vector holding labels.
#' @slot int_min Integer scalar. Lowest value for integer labels.
#' @slot int_max Integer scalar. Highest value for integer labels.
#' @slot breaks Integers or dates. Boundaries between intervals.
#' @slot break_min Integer scalar or date. Lower boundary for intervals.
#' @slot break_max Integer scalar or date. Upper boundary for intervals.
#' @slot open_first Logical flag. Whether there is an interval
#' open on the left.
#' @slot open_last Logical flag. Whether there is an interval
#' open on the right.
#' @slot include_na Logical. Whether labels include an
#' \code{NA}.
#' 
#' @keywords internal
#'
#' @name Labels-class
NULL

validity_Labels <- function(object) {
    include_na <- object@include_na
    val <- demcheck::chk_is_logical_flag(x = include_na,
                                         name = "include_na")
    if (!isTRUE(val))
        return(val)
    TRUE
}

#' @rdname Labels-class
setClass("Labels",
         contains = "VIRTUAL",
         slots = c(include_na = "logical"),
         validity = validity_Labels)


## Categories -----------------------------------------------------------------

validity_LabCategories <- function(object) {
    labels <- object@labels
    val <- demcheck::chk_categories_complete(x = labels,
                                             name = "labels")
    if (!isTRUE(val))
        return(val)
    TRUE
}

## HAS_TESTS
#' @rdname Labels-class
setClass("LabCategories",
         contains = "Labels",
         slots = c(labels = "character"),
         validity = validity_LabCategories)
         

## Triangles -----------------------------------------------------------------

validity_LabTriangles <- function(object) {
    labels <- object@labels
    if (!identical(labels, c("Lower", "Upper"))) {
        labels <- sprintf("\"%s\"", labels)
        labels <- paste(labels, collapse = ", ")
        return(gettextf("'%s' [%s] not identical to \"%s\", \"%s\"",
                        "labels", labels, "Lower", "Upper"))
    }
    TRUE
}

## HAS_TESTS
#' @rdname Labels-class
setClass("LabTriangles",
         contains = "Labels",
         prototype = prototype(labels = c("Lower", "Upper")),
         slots = c(labels = "character"),
         validity = validity_LabTriangles)


## Pool -----------------------------------------------------------------------

validity_LabPool <- function(object) {
    labels <- object@labels
    if (!identical(labels, c("Ins", "Outs"))) {
        labels <- sprintf("\"%s\"", labels)
        labels <- paste(labels, collapse = ", ")
        return(gettextf("'%s' [%s] not identical to \"%s\", \"%s\"",
                        "labels", labels, "Ins", "Outs"))
    }
    TRUE
}

## HAS_TESTS
#' @rdname Labels-class
setClass("LabPool",
         contains = "Labels",
         prototype = prototype(labels = c("Ins", "Outs")),
         slots = c(labels = "character"),
         validity = validity_LabPool)


## Quantiles ------------------------------------------------------------------

validity_LabQuantiles <- function(object) {
    labels <- object@labels
    val <- demcheck::chk_categories_complete(x = labels,
                                             name = "labels")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_is_valid_quantile(x = labels,
                                           name = "labels")
    if (!isTRUE(val))
        return(val)
    TRUE
}

## HAS_TESTS
#' @rdname Labels-class
setClass("LabQuantiles",
         contains = "Labels",
         slots = c(labels = "character"),
         validity = validity_LabQuantiles)


## Integers -------------------------------------------------------------------

## contains the labels themselves, rather than the breaks defining them

validity_LabIntegers <- function(object) {
    int_min <- object@int_min
    int_max <- object@int_max
    for (name in c("int_min", "int_max")) {
        x <- methods::slot(object, name)
        val <- demcheck::chk_is_length_1(x = x,
                                         name = name)
        if (!isTRUE(val))
            return(val)
        val <- demcheck::chk_is_not_na_scalar(x = x,
                                              name = name)
        if (!isTRUE(val))
            return(val)
    }
    if (int_max < int_min)
        return(gettextf("'%s' [%d] less than '%s' [%d]",
                        "int_max", int_max, "int_min", int_min))
    TRUE
}

## HAS_TESTS
#' @rdname Labels-class
setClass("LabIntegers",
         contains = "Labels",
         slots = c(int_min = "integer",
                   int_max = "integer"),
         validity = validity_LabIntegers)



## GroupedInt -----------------------------------------------------------------

## contains the breaks between intervals

validity_LabGroupedInt <- function(object) {
    breaks <- object@breaks
    open_first <- object@open_first
    open_last <- object@open_last
    val <- demcheck::chk_is_not_na_vector(x = breaks,
                                          name = "breaks")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_is_strictly_increasing(x = breaks,
                                                name = "breaks")
    if (!isTRUE(val))
        return(val)
    for (name in c("open_first", "open_last")) {
        x <- methods::slot(object, name)
        val <- demcheck::chk_is_logical_flag(x = x,
                                             name = name)
        if (!isTRUE(val))
            return(val)
    }
    n <- length(breaks)
    if (n == 0L) {
        if (open_first)
            return(gettextf("'%s' has length %d but '%s' is %s",
                            "breaks", 0L, "open_first", "TRUE"))
        if (open_last)
            return(gettextf("'%s' has length %d but '%s' is %s",
                            "breaks", 0L, "open_last", "TRUE"))
    }
    if (n == 1L) {
        if (!open_first && !open_last)
            return(gettextf("'%s' has length %d but '%s' and '%s' are both %s",
                            "breaks", 1L, "open_first", "open_last", "FALSE"))
    }
    TRUE
}

#' @rdname Labels-class
setClass("LabGroupedInt",
         contains = c("Labels",
                      "VIRTUAL"),
         slots = c(breaks = "integer",
                   open_first = "logical",
                   open_last = "logical"),
         validity = validity_LabGroupedInt)


## HAS_TESTS
#' @rdname Labels-class
setClass("LabGroupedIntEnumerations",
         contains = "LabGroupedInt")


## HAS_TESTS
#' @rdname Labels-class
setClass("LabGroupedIntEndpoints",
         contains = "LabGroupedInt")


## Dates ----------------------------------------------------------------------

## contains the breaks between intervals

validity_LabCalendar <- function(object) {
    break_min <- object@break_min
    break_max <- object@break_max
    open_first <- object@open_first
    open_last <- object@open_last
    for (name in c("break_min", "break_max")) {
        x <- methods::slot(object, name)
        val <- demcheck::chk_is_length_1(x = x,
                                         name = name)
        if (!isTRUE(val))
            return(val)
        val <- demcheck::chk_is_not_na_scalar(x = x,
                                              name = name)
        if (!isTRUE(val))
            return(val)
    }
    for (name in c("open_first", "open_last")) {
        x <- methods::slot(object, name)
        val <- demcheck::chk_is_logical_flag(x = x,
                                             name = name)
        if (!isTRUE(val))
            return(val)
    }
    if (break_max < break_min)
        return(gettextf("'%s' [%s] less than '%s' [%s]",
                        "break_max", break_max, "break_min", break_min))
    if (break_min == break_max) {
        if (!open_first && !open_last)
            return(gettextf("'%s' [%s] equals '%s' but '%s' and '%s' are both %s",
                            "break_min",
                            break_min,
                            "break_max",
                            "open_first",
                            "open_last",
                            "FALSE"))
    }
    TRUE
}

#' @rdname Labels-class
setClass("LabCalendar",
         contains = c("Labels",
                      "VIRTUAL"),
         slots = c(break_min = "Date",
                   break_max = "Date",
                   open_first = "logical",
                   open_last = "logical"),
         validity = validity_LabCalendar)


validity_LabCalendarQuarters <- function(object) {
    for (name in c("break_min", "break_max")) {
        x <- methods::slot(object, name)
        val <- demcheck::chk_is_first_day_unit_scalar(x = x,
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
         validity = validity_LabCalendarQuarters)

validity_LabCalendarMonths <- function(object) {
    for (name in c("break_min", "break_max")) {
        x <- methods::slot(object, name)
        val <- demcheck::chk_is_first_day_unit_scalar(x = x,
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
         validity = validity_LabCalendarMonths)


## Durations ------------------------------------------------------------------

## contains the breaks between intervals

validity_LabDurations <- function(object) {
    break_min <- object@break_min
    break_max <- object@break_max
    open_last <- object@open_last
    for (name in c("break_min", "break_max")) {
        x <- methods::slot(object, name)
        val <- demcheck::chk_is_length_1(x = x,
                                         name = name)
        if (!isTRUE(val))
            return(val)
        val <- demcheck::chk_is_not_na_scalar(x = x,
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
         validity = validity_LabDurations)

## HAS_TESTS
#' @rdname Labels-class
setClass("LabDurationsQuarters",
         contains = "LabDurations")

## HAS_TESTS
#' @rdname Labels-class
setClass("LabDurationsMonths",
         contains = "LabDurations")
