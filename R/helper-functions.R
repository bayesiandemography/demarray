
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
                ans[[i_ans]] <- "attribute"
        }
    }
    ans
}

## HAS_TESTS
sort_durations <- function(durations) {
    p <- "^([0-9]+).*$"
    durations_int <- sub(p, "\\1", durations)
    durations_int <- suppressWarnings(as.integer(durations_int))
    i <- order(durations_int, na.last = TRUE)
    durations[i]
}

## HAS_TESTS
sort_intervals <- function(intervals) {
    p_first <- "^<(-?[0-9]+).*"
    p_ordinary <- "^(-?[0-9]+).*"
    is_first <- grepl(p_first, intervals)
    is_ordinary  <- grepl(p_ordinary, intervals)
    intervals_first <- intervals[is_first]
    intervals_ordinary <- intervals[is_ordinary]
    intervals_other <- intervals[!is_first & !is_ordinary]
    num_first <- as.numeric(sub(p_first, "\\1", intervals_first))    
    num_ordinary <- as.numeric(sub(p_ordinary, "\\1", intervals_ordinary))
    i_first <- order(num_first)
    i_ordinary <- order(num_ordinary)
    c(intervals_first[i_first],
      intervals_ordinary[i_ordinary],
      intervals_other)
}

## assumes months valid
sort_months <- function(months) {
    if (identical(length(months), 0L))
        return(months)
    months_date <- sub("<|\\+", "", months)
    months_date <- paste(months_date, 1)
    months_date <- as.Date(months_date, format = "%Y %b %d")
    i <- order(months_date, na.last = TRUE)
    months[i]
}


## HAS_TESTS
## assume quantiles valid
sort_quantiles <- function(quantiles) {
    quantiles_numeric <- as.numeric(sub("%", "", quantiles))
    i <- order(quantiles_numeric, na.last = TRUE)
    quantiles[i]
}
