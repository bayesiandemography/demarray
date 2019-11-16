
## HAS_TESTS
infer_dimtypes <- function(names) {
    p_pairs <- c(origin = "_orig$",
                 destination = "_dest$",
                 parent = "_parent$",
                 child = "_child$")
    synonyms <- c(age = "age",
                  age5 = "age",
                  age10 = "age",
                  age5yr = "age",
                  age10yr = "age",
                  age5year = "age",
                  age10year = "age",
                  "age group" = "age",
                  agegroup = "age",
                  "birth cohort" = "cohort",
                  cohort = "cohort",
                  duration = "age",
                  gender = "sex",
                  genders = "sex",
                  iterations = "iteration",
                  iter = "iteration",
                  iteration = "iteration",
                  sim = "iteration",
                  simulation = "iteration",
                  quantile = "quantile",
                  quantiles = "quantile",
                  time = "time",
                  period = "time",
                  period5 = "time",
                  quarter = "time",
                  year = "time",
                  yr = "time",
                  lexis = "triangle",
                  "lexis triangle" = "triangle",
                  "lexis triangles" = "triangle",
                  triangle = "triangle",
                  triangles = "triangle")
    n <- length(names)
    ans <- character(n)
    for (i_pair in seq_along(p_pairs)) {
        is_pair <- grepl(p_pairs[[i_pair]], names)
        ans[is_pair] <- names(p_pairs)[[i_pair]]
    }
    names <- tolower(names)
    names_syn <- names(synonyms)
    for (i_ans in seq_len(n)) {
        if (identical(ans[[i_ans]], "")) {
            i_syn <- match(names[[i_ans]], names_syn, nomatch = 0L)
            if (i_syn > 0L)
                ans[[i_ans]] <- synonyms[[i_syn]]
            else
                ans[[i_ans]] <- "state"
        }
    }
    ans
}
