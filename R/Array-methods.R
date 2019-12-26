

#' @rdname dimtypes
#' @export
setMethod("dimtypes",
          signature(x = "Array"),
          function(x) {
              ans <- x@dimtypes
              names(ans) <- names(dimnames(x))
              ans
          })


