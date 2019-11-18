
validity_Array <- function(object) {
    dim <- dim(object)
    dimtypes <- object@dimtypes
    classif <- object@classif
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
    ## classif valid
    val <- demcheck::chk_is_not_blank_vector(x = classif,
                                             name = "classif")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_no_names(x = classif,
                                  name = "classif")
    if (!isTRUE(val))
        return(val)
    ## dim and dimtypes consistent
    val <- demcheck::chk_length_same(x1 = dimtypes,
                                     x2 = dim,
                                     name1 = "dimtypes",
                                     name2 = "dim")
    if (!isTRUE(val))
        return(val)
    ## dim and classif consistent
    val <- demcheck::chk_length_same(x1 = classif,
                                     x2 = dim,
                                     name1 = "classif",
                                     name2 = "dim")
    if (!isTRUE(val))
        return(val) 
    ## dimnames and dimtypes consistent
    for (i in seq_along(dimnames)) {
        val <- demcheck::chk_labels_valid_for_dimtype(labels = dimnames[[i]],
                                                      dimtype = dimtypes[[i]])
        if (!isTRUE(val)) {
            return(gettextf("problem with \"%s\" dimension : %s",
                            names[[i]], val))
        }
    }
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
#' @slot classif Character vector
#'
#' @export
setClass("Array",
         contains = "array",
         slots = c(dimtypes = "character",
                   classif = "character"),
         validity = validity_Array)

#' @rdname Array
#' @export
setClass("Counts",
         contains = "Array")

#' @rdname Array
#' @export
setClass("Values",
         contains = "Array")


