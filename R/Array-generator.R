
setMethod("Array",
          signature(x = "array"),
          function(x, dimtypes = NULL, classif = NULL) {
              demcheck::err_dimnames_complete(x = x,
                                              names = "x")
              demcheck::err_names_dimnames_complete(x = x,
                                                    names = "x")
              names <- names(dimnames(x))
              dimtypes_ans <- infer_dimtypes(names)
              classif_ans <- rep(NA_character_, times = length(dim))
              if (!is.null(dimtypes)) {
                  demcheck::err_names_complete(x = dimtypes,
                                               name = "dimtypes")
                  nms_dimtypes <- names(dimtypes)
                  i_dimtype <- match(nms_dimtypes, names, nomatch = 0L)
                  i_invalid <- match(0L, i_dimtype, nomatch = 0L)
                  if (i_invalid > 0L)
                      stop(gettextf("'%s' has an element named \"%s\" but '%s' does not have a dimension named \"%s\"",
                                    "dimtypes",
                                    nms_dimtypes[[i_invalid]],
                                    "x",
                                    nms_dimtypes[[i_invalid]]))
                  dimtypes_ans[i_dimtype] <- unname(dimtypes)
              }
              if (!is.null(classif)) {
                  demcheck::err_names_complete(x = classif,
                                               name = "classif")
                  nms_classif <- names(classif)
                  i_classif <- match(nms_classif, names, nomatch = 0L)
                  i_invalid <- match(0L, i_dimtype, nomatch = 0L)
                  if (i_invalid > 0L)
                      stop(gettextf("'%s' has an element named \"%s\" but '%s' does not have a dimension named \"%s\"",
                                    "classif",
                                    nms_classif[[i_invalid]],
                                    "x",
                                    nms_classif[[i_invalid]]))
                  classif_ans[i_classif] <- unname(classif)
              }
              methods::new("Array",
                           .Data = x,
                           dimtypes = dimtypes_ans,
                           classif = classif_ans)
          })


setMethod("Array",
          signature(x = "Array"),
          function(x, dimtypes = NULL, classif = NULL) {
              methods::validObject(x)
              names <- names(dimnames(x))
              dimtypes_ans <- unname(dimtypes(x))
              classif_ans <- unname(classif(x))
              if (!is.null(dimtypes)) {
                  demcheck::err_names_complete(x = dimtypes,
                                               name = "dimtypes")
                  nms_dimtypes <- names(dimtypes)
                  i_dimtype <- match(nms_dimtypes, names, nomatch = 0L)
                  i_invalid <- match(0L, i_dimtype, nomatch = 0L)
                  if (i_invalid > 0L)
                      stop(gettextf("'%s' has an element named \"%s\" but '%s' does not have a dimension named \"%s\"",
                                    "dimtypes",
                                    nms_dimtypes[[i_invalid]],
                                    "x",
                                    nms_dimtypes[[i_invalid]]))
                  dimtypes_ans[i_dimtype] <- unname(dimtypes)
              }
              if (!is.null(classif)) {
                  demcheck::err_names_complete(x = classif,
                                               name = "classif")
                  nms_classif <- names(classif)
                  i_classif <- match(nms_classif, names, nomatch = 0L)
                  i_invalid <- match(0L, i_dimtype, nomatch = 0L)
                  if (i_invalid > 0L)
                      stop(gettextf("'%s' has an element named \"%s\" but '%s' does not have a dimension named \"%s\"",
                                    "classif",
                                    nms_classif[[i_invalid]],
                                    "x",
                                    nms_classif[[i_invalid]]))
                  classif_ans[i_classif] <- unname(classif)
              }
              methods::new("Array",
                           .Data = x,
                           dimtypes = dimtypes_ans,
                           classif = classif_ans)
          })


setMethod("Array",
          signature(x = "Array"),
          function(x, dimtypes = NULL, classif = NULL) {
              methods::validObject(x)
              .Data  <- x@.Data
              dimtypes <- unname(dimtypes(x))
              classif <- unname(classif(x))
              methods::new("Array",
                           .Data = .Data,
                           dimtypes = dimtypes,
                           classif = classif)
          })
          
Counts <- function(x, dimtypes = NULL, classif = NULL) {
    ans <- Array(x = x,
                 dimtypes = dimtypes,
                 classif = classif)
    methods::as(ans, "Counts")
}

          
Values <- function(x, dimtypes = NULL, classif = NULL) {
    ans <- Array(x = x,
                 dimtypes = dimtypes,
                 classif = classif)
    methods::as(ans, "Values")
}

          
