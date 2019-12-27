

#' @rdname dimtypes
#' @export
setMethod("dimtypes",
          signature(x = "DemographicArray"),
          function(x) {
              ans <- x@dimtypes
              names(ans) <- names(dimnames(x))
              ans
          })

#' @rdname is_quant_scale
#' @export
setMethod("is_quant_scale",
          signature(x = "DemographicArray"),
          function(x) {
              dimnames <- dimnames(x)
              dimtypes <- dimtypes(x)
              names <- names(dimnames)
              n <- length(dimnames)
              ans <- logical(length = n)
              for (i in seq_len(n)) {
                  lab <- infer_lab(labels = dimnames[[i]],
                                   dimtype = dimtypes[[i]])
                  ans[[i]] <- labels_imply_quant_scale(lab)
              }
              names(ans) <- names
              ans
          })


