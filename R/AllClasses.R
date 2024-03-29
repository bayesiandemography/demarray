
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
         slots = c(values = "vector"))


## Categories -----------------------------------------------------------------

## NO_TESTS
#' @rdname Labels-class
setClass("Categories",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_categories(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })
         
## NO_TESTS
#' @rdname Labels-class
setClass("Triangles",
         contains = "Categories",
         prototype = prototype(values = c("Lower", "Upper")),
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_triangles(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## NO_TESTS
#' @rdname Labels-class
setClass("Direction",
         contains = "Categories",
         prototype = prototype(values = c("In", "Out")),
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_direction(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## NO_TESTS
#' @rdname Labels-class
setClass("Quantiles",
         contains = "Categories",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_quantiles(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })


## Iterations -----------------------------------------------------------------

## NO_TESTS
#' @rdname Labels-class
setClass("Iterations",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_iterations(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })


## Pairs ----------------------------------------------------------------------

## Each subclass of the "Pairs" virtual superclass
## has its own validity function because
## this validity function is used for other purposes,
## eg the 'make_labels' functions.

## NO_TESTS
#' @rdname Labels-class
setClass("Pairs",
         contains = c("Labels", "VIRTUAL"))

## NO_TESTS
#' @rdname Labels-class
setClass("Integers",
         contains = "Pairs",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_integers(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## NO_TESTS
#' @rdname Labels-class
setClass("Intervals",
         contains = "Pairs",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_intervals(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## NO_TESTS
#' @rdname Labels-class
setClass("Quantities",
         contains = "Pairs",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_quantities(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## NO_TESTS
#' @rdname Labels-class
setClass("Quarters",
         contains = "Pairs",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_quarters(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## NO_TESTS
#' @rdname Labels-class
setClass("Months",
         contains = "Pairs",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_months(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })

## NO_TESTS
#' @rdname Labels-class
setClass("DateRanges",
         contains = "Pairs",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_dateranges(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })


## DatesPoints ----------------------------------------------------------------

## NO_TESTS
#' @rdname Labels-class
setClass("DatePoints",
         contains = "Labels",
         validity = function(object) {
             values <- object@values
             val <- demcheck::chk_values_datepoints(values)
             if (!isTRUE(val))
                 return(val)
             TRUE
         })



