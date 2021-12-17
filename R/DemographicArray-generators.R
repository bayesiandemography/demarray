
## HAS_TESTS
#' @rdname DemographicArray
#' @export
setMethod("DemographicArray",
          signature(x = "array"),
          function(x, check = NULL) {
              demcheck::err_chk_array_metadata_complete(x = x,
                                                        name = "x")
              check <- demcheck::err_tdy_check(check)
              if ("dimtypes" %in% check)
                  
              
              dimtypes_ans <- infer_dimtypes(names)
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
              methods::new("DemographicArray",
                           .Data = x,
                           dimtypes = dimtypes_ans)
          })

#' @rdname DemographicArray
#' @export
setMethod("DemographicArray",
          signature(x = "DemographicArray"),
          function(x, dimtypes = NULL) {
              methods::validObject(x)
              names <- names(dimnames(x))
              dimtypes_ans <- unname(dimtypes(x))
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
              methods::new("DemographicArray",
                           .Data = x@.Data,
                           dimtypes = dimtypes_ans)
          })

#' @rdname DemographicArray
#' @export
Counts <- function(x, dimtypes = NULL) {
    ans <- DemographicArray(x = x,
                            dimtypes = dimtypes)
    methods::as(ans, "Counts")
}

          
#' @rdname DemographicArray
#' @export
Values <- function(x, dimtypes = NULL) {
    ans <- DemographicArray(x = x,
                            dimtypes = dimtypes)
    methods::as(ans, "Values")
}

          
