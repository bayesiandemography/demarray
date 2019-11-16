
setMethod("classif",
          signature(x = "Array"),
          function(x) {
              ans <- x@classif
              names(ans) <- names(dimnames(x))
              ans
          })

setMethod("dimtypes",
          signature(x = "Array"),
          function(x) {
              ans <- x@classif
              names(ans) <- names(dimnames(x))
              ans
          })


