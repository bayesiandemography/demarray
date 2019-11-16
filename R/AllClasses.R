
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
    ## dimtypes valid
    val <- demcheck::chk_member_dimtype(x = dimtypes,
                                        name = "dimtypes")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_no_names(x = dimtypes,
                                  name = "dimtypes")
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
    dimnames <- dimnames(object)
    names <- names(dimnames)
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
    
setClass("Array",
         contains = "array",
         slots = c(dimtypes = "character",
                   classif = "character"),
         validity = validity_Array)

setClass("Counts",
         contains = "Array")

setClass("Values",
         contains = "Array")


