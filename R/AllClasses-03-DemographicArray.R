
validity_DemographicArray <- function(object) {
    .Data <- object@.Data
    val <- demcheck::chk_is_numeric(x = .Data,
                                    name = ".Data")
    if (!isTRUE(val))
        return(val)
    val <- demcheck::chk_no_dimnames(x = .Data,
                                     name = ".Data")
    if (!isTRUE(val))
        return(val)
    TRUE
}
    


setClass("DemographicArray",
         contains = c("VIRTUAL",
                      "array"),
         slots = c(metadata = "MetaData"),
         validity = validity_DemographicArray)

setClass("Counts",
         contains = "DemographicArray")


setClass("Values",
         contains = "DemographicArray")


