

## NO_TESTS
#' @rdname limits
#' @export
setMethod("limits",
          signature(x = "array"),
          function(x) {
              limits_one_dim <- function(x) {
                  n <- length(x)
                  if (n == 0L)
                      rep(as.character(NA), times = 2L)
                  else if (n == 1L)
                      rep(x, times = 2L)
                  else
                      x[c(1L, n)]
              }
              ans <- dimnames(x)
              if (is.null(ans))
                  return(ans)
              ans <- lapply(ans, limits_one_dim)
              ans <- data.frame(ans)
              rownames(ans) <- c("first", "last")
              ans
          })          
