
validity_MetaData <- function(object) {
    names <- object@names
    dimtypes <- object@dimtypes
    dimscales <- object@dimscales
    val <- demcheck::chk_all_class(x = dimscales,
                                   name = "dimscales",
                                   class = "DimScale")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_length_same(x1 = names,
                                     x2 = dimtypes,
                                     name1 = "names",
                                     name2 = "dimtypes")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_length_same(x1 = names,
                                     x2 = dimscales,
                                     name1 = "names",
                                     name2 = "dimscales")
    if (!isTRUE(val))
        return(val)
    ## NEED TO ADD MORE!!!
    TRUE
}
    

setClass("MetaData",
         slots = c(names = "character",
                   dimtypes = "character",
                   dimscales = "list"),
         validity = validity_DemographicArray)

setClass("Counts",
         contains = "DemographicArray")


setClass("Values",
         contains = "DemographicArray")


