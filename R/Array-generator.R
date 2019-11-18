
## HAS_TESTS
#' @rdname Array
#' @export
setMethod("Array",
          signature(x = "array"),
          function(x, dimtypes = NULL, classif = NULL) {
              demcheck::err_dimnames_complete(x = x,
                                              name = "x")
              demcheck::err_names_dimnames_complete(x = x,
                                                    name = "x")
              names <- names(dimnames(x))
              dimtypes_ans <- infer_dimtypes(names)
              classif_ans <- rep(NA_character_, times = length(names))
              if (!is.null(dimtypes)) {
                  demcheck::err_names_complete(x = dimtypes,
                                               name = "dimtypes")
                  nms_dimtypes <- names(dimtypes)
                  i_dimtype <- match(nms_dimtypes, names, nomatch = 0L)
                  i_invalid <- match(0L, i_dimtype, nomatch = 0L)
                  if (i_invalid > 0L)
                      stop(gettextf("'%s' has element named \"%s\" but '%s' does not have dimension named \"%s\"",
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
                  i_invalid <- match(0L, i_classif, nomatch = 0L)
                  if (i_invalid > 0L)
                      stop(gettextf("'%s' has element named \"%s\" but '%s' does not have dimension named \"%s\"",
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

#' @rdname Array
#' @export
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
                      stop(gettextf("'%s' has element named \"%s\" but '%s' does not have dimension named \"%s\"",
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
                  i_invalid <- match(0L, i_classif, nomatch = 0L)
                  if (i_invalid > 0L)
                      stop(gettextf("'%s' has element named \"%s\" but '%s' does not have dimension named \"%s\"",
                                    "classif",
                                    nms_classif[[i_invalid]],
                                    "x",
                                    nms_classif[[i_invalid]]))
                  classif_ans[i_classif] <- unname(classif)
              }
              methods::new("Array",
                           .Data = x@.Data,
                           dimtypes = dimtypes_ans,
                           classif = classif_ans)
          })

#' @rdname Array
#' @export
Counts <- function(x, dimtypes = NULL, classif = NULL) {
    ans <- Array(x = x,
                 dimtypes = dimtypes,
                 classif = classif)
    methods::as(ans, "Counts")
}

          
#' @rdname Array
#' @export
Values <- function(x, dimtypes = NULL, classif = NULL) {
    ans <- Array(x = x,
                 dimtypes = dimtypes,
                 classif = classif)
    methods::as(ans, "Values")
}

          
