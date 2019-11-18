
#' @rdname classif
#' @export
setMethod("classif",
          signature(x = "Array"),
          function(x) {
              ans <- x@classif
              names(ans) <- names(dimnames(x))
              ans
          })

#' @rdname dimtypes
#' @export
setMethod("dimtypes",
          signature(x = "Array"),
          function(x) {
              ans <- x@dimtypes
              names(ans) <- names(dimnames(x))
              ans
          })


