

setGeneric("make_dimscale_labels",
           function(object) {
               dimscales <- object@dimscales
               as.character(dimscales)
           })



setGeneric("n_level_dimscale",
           function(object)
               standardGeneric("n_level_dimscale"))
