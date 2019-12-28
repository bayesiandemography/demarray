
#' Create a demographic array
#'
#' Create an object of class \code{\linkS4class{DemographicArray}}.
#'
#' @param x An array, with a complete set of dimnames.
#' @param dimtypes A named character vector specifying dimtypes
#' to be used by each dimension.
#'
#' @return A \code{\linkS4class{DemographicArray}}.
#'
#' @examples
#' x <- array(1:6,
#'            dim = c(2, 3),
#'            dimnames = list(sex = c("Female", "Male"),
#'                            age = c("0-14", "15-64", "65+")))
#' DemographicArray(x)
#'
#' # specify dimtype
#' x <- array(1:6,
#'            dim = c(2, 3),
#'            dimnames = list(sex = c("Female", "Male"),
#'                            year = c("1945-1965", "1965-1985", "1985-2000")))
#' DemographicArray(x, dimtypes = c(year = "cohort"))
#'
#' # specify dimtypes
#' x <- array(1:6,
#'            dim = c(2, 3),
#'            dimnames = list(sex = c("Female", "Male"),
#'                            year = c("1945-1965", "1965-1985", "1985-2000")))
#' DemographicArray(x,
#'       dimtypes = c(year = "cohort"))
#' @export
setGeneric("DemographicArray",
           function(x, dimtypes = NULL)
               standardGeneric("DemographicArray"))


#' Get or set dimtypes
#'
#' Query or modify the dimtypes of an object.
#'
#' Every dimension of a  demographic array has a
#' 'dimtype' describing the type of information held by that dimension.
#' Examples include \code{"age"}, \code{"time"}, and \code{"state"}. See below
#' for details.
#'
#' The range of dimtypes are as follows:
#'
#' \tabular{rll}{ dimtype \tab Description \tab Labels \cr
#' \code{"age"} \tab Age, as points or intervals \tab <any> \cr
#' \code{"time"} \tab Time, as points or intervals \cr
#' \code{"cohort"} \tab Cohort, measured as points or intervals \tab <any> \cr
#' \code{"attribute"} \tab Any qualitative attribute \tab  <any> \cr
#' \code{"origin"}, \code{"destination"} \tab Starting and finishing states
#' \tab <any> \cr
#' \code{"parent"}, \code{"child"} \tab Parent's state versus child's state
#' \tab <any> \cr
#' \code{"iteration"} \tab Simulation number \tab Positive integers \cr
#' \code{"quantile"} \tab Position in distribution \tab Percentage \cr
#' \code{"triangle"} \tab Lexis triangle \tab \code{"Lower", "Upper"} \cr
#' \code{"pool"} \tab Type of migration flow \tab \code{"Ins", "Outs"} \cr
#' }
#'
#' \code{"origin"}, \code{"destination"}.  Starting and
#' finishing values for an attribute that is subject to change, such as country
#' of residence.  \code{"origin"} and \code{"destination"} dimensions must come
#' in pairs with a common base name and suffixes \code{"_orig"} and
#' \code{"_dest"}, each \code{"country_orig"} and \code{"country_dest"}.  There
#' is no limit to the number of origin-destination pairs that an object may
#' have.
#'
#' \code{"parent"}, \code{"child"}.  Parents' attributes versus
#' those of their children.  Used to model transmission of attributes such as
#' ethnicity from parents to children. \code{"parent"} and \code{"child"}
#' dimensions must come in pairs with a common base name and suffixes
#' \code{"_parent"} and \code{"_child"}, e.g. \code{"ethnicity_parent"} and
#' \code{"ethnicity_child"}.  There is no limit to the number of parent-child
#' pairs that an object may have.
#'
#' \code{"iteration"}.  Iteration number
#' from a simulation.  An object can have at most one dimension with dimtype
#' \code{"iteration"}.  If an object has a dimension with dimtype
#' \code{"iteration"} it may not have a dimension with dimtype
#' \code{"quantile"}.
#'
#' \code{"quantile"}.  Sample quantiles, typically
#' summarizing simulation results.  An object can have at most one dimension
#' with dimtype \code{"quantile"}.  If an object has a dimension with dimtype
#' \code{"quantile"} it may not have a dimension with dimtype
#' \code{"iteration"}.
#'
#' \code{"triangle"}.  Lexis triangle.
#'
#' \code{"pool"}. Direction of flow in "pool" model of migration.
#'
#' @param x An object of class \code{\linkS4class{DemographicArray}}.
#' \emph{Add reference to demographic account, once we have implemented
#' them.}
#'
#' @return A named vector
#'
#' @examples
#' x <- DemographicArray(array(1:6,
#'                  dim = c(2, 3),
#'                  dimnames = list(sex = c("Female", "Male"),
#'                                  time = c(2000, 2010, 2020))))
#' dimtypes(x)
#' @aliases dimtype dimtypes
#' @export
setGeneric("dimtypes",
           function(x)
               standardGeneric("dimtypes"))


#' See whether cross-classifying dimensions of demographic array
#' or account have quantitative scales
#'
#' See how functions in \code{demarray} and associated packages
#' will interpret the labels from each dimension of a demographic
#' array or account.
#' 
#' Each dimension of a demographic array has a measurement scale.
#' A dimension may, for instance, represent age using years since birth,
#' or represent geographical region using the name of that region.
#' Years since birth is an example of a quantitative measurement
#' scale, and region name is an example of a qualitative measurement
#' scale. Functions in \code{demarray} and associated packages infer
#' measurement scales from \code{\link{dimtypes}} and dimnames.
#'
#' Function \code{is_quant_scale} returns a logical vector stating,
#' for each dimension of a demographic array, whether the that dimension
#' has quantitative measurement scale. When used with a demographic
#' account, \code{is_quant_scale} returns the values for the population
#' series.
#' 
#' To change the dimnames, and hence, potentially, to switch
#' between quantitative and qualitative scales, use base
#' function \code{\link[base]{dimnames}}.
#'
#' @inheritParams dimtypes
#' 
#' @return A named logical vector.
#'
#' @seealso dimtypes
#'
#' @examples
#' x <- Counts(array(1:8,
#'                   dim = c(2, 2, 2),
#'                   dimnames = list(sex = c("Female", "Male"),
#'                                   age = c("0-39", "40+"),
#'                                   month = c("2015 Jan", "2015 Jul"))))
#' x
#' is_quant_scale(x)
#' dimnames(x)$age <- c("Young", "Old")
#' x
#' is_quant_scale(x)
#' @export
setGeneric("is_quant_scale",
           function(x) {
               stop(gettextf("cannot handle object of class \"%s\"",
                             class(x)),
                    call. = FALSE)
           })

setGeneric("labels_imply_quant_scale",
           function(x) {
               stop(gettextf("cannot handle object of class \"%s\"",
                             class(x)),
                    call. = FALSE)
           })

setGeneric("make_labels",
           function(x) {
               stop(gettextf("cannot handle object of class \"%s\"",
                             class(x)),
                    call. = FALSE)
           })


#' Get first and last labelsl used by each dimension.
#'
#' Get the first and last categories used by each dimension
#' of an array or demographic account. These categories can
#' be useful in understanding the structure of the array
#' or account. When used with an account, \code{limits}
#' returns the values for the population series.
#'
#' @param x An object of class \code{\link[base]{array}},
#' \code{\linkS4class{DemographicArray}}, or a demographic account.
#'
#' @return A data frame.
#'
#' @examples
#' x <- Counts(array(1:8,
#'                   dim = c(2, 4),
#'                   dimnames = list(sex = c("Female", "Male"),
#'                                   age = c("0-14", "15-39", "40-64", "65+"))))
#' x
#' limits(x)
#' @export
setGeneric("limits",
           function(x) {
               stop(gettextf("cannot handle object of class \"%s\"",
                             class(x)),
                    call. = FALSE)
           })
