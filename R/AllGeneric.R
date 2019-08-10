

setGeneric("make_dimscale_labels",
           function(object) {
               dimvalues <- object@dimvalues
               as.character(dimvalues)
           })

setGeneric("n_level_dimscale",
           function(object)
               standardGeneric("n_level_dimscale"))
