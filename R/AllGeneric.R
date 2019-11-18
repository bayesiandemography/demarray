
#' Create a demographic array
#'
#' Create an object of class \code{\linkS4class{Array}}.
#'
#' @param x An array, with a complete set of dimnames.
#' @param dimtypes A named character specifying dimtypes
#' to be used by each dimension.
#' @param classif A named character vector specifying
#' classifications to be used by each dimension.
#'
#' @return A \code{\linkS4class{Array}}.
#'
#' @examples
#' x <- array(1:6,
#'            dim = c(2, 3),
#'            dimnames = list(sex = c("Female", "Male"),
#'                            age = c("0-14", "15-64", "65+")))
#' Array(x)
#'
#' # specify dimtype
#' x <- array(1:6,
#'            dim = c(2, 3),
#'            dimnames = list(sex = c("Female", "Male"),
#'                            year = c("1945-1965", "1965-1985", "1985-2000")))
#' Array(x, dimtypes = c(year = "cohort"))
#'
#' # specify dimtypes and classification
#' x <- array(1:6,
#'            dim = c(2, 3),
#'            dimnames = list(sex = c("Female", "Male"),
#'                            year = c("1945-1965", "1965-1985", "1985-2000")))
#' Array(x,
#'       dimtypes = c(year = "cohort"),
#'       classif = c(sex = "Sex", year = "Generation"))
#' @export
setGeneric("Array",
           function(x, dimtypes = NULL, classif = NULL)
               standardGeneric("Array"))


#' Get or set the classifications used by a demographic array
#'
#' @param x An \code{\linkS4class{Array}}.
#'
#' @return A named vector
#'
#' @examples
#' x <- Array(array(1:6,
#'                  dim = c(2, 3),
#'                  dimnames = list(sex = c("Female", "Male"),
#'                                  year = c("1945-1965", "1965-1985", "1985-2000"))),
#'            classif = c(sex = "Sex", year = "Generation"))
#' classif(x)
#' @export 
setGeneric("classif",
           function(x)
               standardGeneric("classif"))


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
#' \code{"state"} \tab Any qualitative attribute \tab  <any> \cr
#' \code{"origin"}, \code{"destination"} \tab Starting and finishing states
#' \tab <any> \cr
#' \code{"parent"}, \code{"child"} \tab Parent's state versus child's state
#' \tab <any> \cr
#' \code{"iteration"} \tab Simulation number \tab Positive integers \cr
#' \code{"quantile"} \tab Position in distribution \tab Percentage \cr
#' \code{"triangle"} \tab Lexis triangle \tab \code{"Lower", "Upper"} \cr
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
#' @inheritParams classif
#'
#' @return A named vector
#'
#' @examples
#' x <- Array(array(1:6,
#'                  dim = c(2, 3),
#'                  dimnames = list(sex = c("Female", "Male"),
#'                                  time = c(2000, 2010, 2020))))
#' dimtypes(x)
#' @aliases dimtype dimtypes
#' @export
setGeneric("dimtypes",
           function(x)
               standardGeneric("dimtypes"))
