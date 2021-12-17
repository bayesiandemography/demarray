## demprep --------------------------------------------------------------------

clean_age(x)
clean_time(x)
clean_cohort(x)
clean_triangle(x)
clean_quantile(x)

date_to_age
date_to_cohort
date_to_period_year
date_to_period_
date_to_triangle

format_age
format_cohort
format_period
format_triangle

as_date_range

## demarray -------------------------------------------------------------------

## wherever a name is supplied, can use base name, and process two dimensions at once

## Aggregating

sum_to(x, names, na_rm = FALSE)
sum_along(x, names, na_rm = FALSE)

mean_to(x, names, na_rm = FALSE)
mean_along(x, names, na_rm = FALSE)

sum_cat(x, name, old, new, na_rm = FALSE)
sum_custom(x, name, breaks, na_rm = FALSE)
sum_multi(x, name, width = 5, break_min = NULL, break_max = NULL, na_rm = FALSE)
sum_lifetab(x, name, break_max = NULL, na_rm = FALSE)
sum_year(x, name, na_rm = FALSE)
sum_quarter(x, name, na_rm = FALSE)

sum_map(x, name, map = NULL, na_rm = FALSE)

sum_agetime(x, width, na_rm = FALSE)

sum_open(x, name, break_max, na_rm = FALSE)

mean_cat(x, name, old, new, wt, na_rm = FALSE)
mean_custom(x, name, breaks, wt, na_rm = FALSE)
mean_multi(x, name, width = 5, break_min = NULL, break_max = NULL, wt, na_rm = FALSE)
mean_lifetab(x, name, break_max = NULL, wt, na_rm = FALSE)
mean_year(x, name, wt, na_rm = FALSE)
mean_quarter(x, name, wt, na_rm = FALSE)

mean_agetime(x, width, na_rm = FALSE)

mean_open(x, name, break_max, na_rm = FALSE)


mean_map(x, name, map = NULL, na_rm = FALSE)


## Broadcasting

broadcast_along(x, name, cat)

broadcast_cat(x, name, old, new)
broadcast_custom(x, name, breaks)
broadcast_multi(x, name, width, break_min = NULL, break_max = NULL)
broadcast_year(x, name)
broadcast_quarter(x, name)

broadcast_map(x, name, map = NULL)



## Redistributing (not Values)

redist_to(x, names, wt = NULL)
redist_along(x, names, wt = NULL)

redist_cat(x, name, old, new, wt = NULL)
redist_custom(x, name, breaks, wt = NULL)
redist_multi(x, name, width, wt = NULL)
redist_lifetab(x, name, break_max, wt = NULL)
redist_year(x, name, wt = NULL)
redist_quarter(x, name, wt = NULL)
redist_month(x, name, wt = NULL)

redist_map(x, name, map = NULL, wt = NULL)


## subsetting

subarray(x, ...) ## ... is expressions, each involving single dimension
extract_array(x, where = NULL)
trim_onto(x, y, names = NULL)

## drop


             
              

## replacing

replace_array(x, y)


## mutating

mutate_array(x, ..., expr)


## padding out

pad_onto(x, y, pad = NULL, names = NULL)


## permuting

aperm(x, perm, ...)


## aligning

## does not trim or pad
align_onto(x, y) ## generic
align_both(x, y) ## generic. Called by binary operations


## iterations

summarise_iter(x, fun, ...)
quantile_iter(x, prob = c(0.025, 0.25, 0.5, 0.75, 0.975), na_rm = FALSE)
thin_iter(x, n)
reset_iter(x)



## agetime plan

pivot_agetime(x, to)
is_regular(x, name)
step_length(x, name)

## migration format

to_net(x, name)
to_pool(x, name)


## getting, setting metadata

dimtypes(x)
names(x)
`names<-`(x, value)
dimnames(x)
`dimnames<-`(x, value)
relabel_cat(x, name, old = NULL, new = NULL, map = NULL)
to_date_range(x, name)
from_date_range(x, name, month_start = NULL, label_year_start = NULL)


reorder_cat(x, name, fun = mean, ...)

dim_ranges(x)
dim_min(x, name, finite = TRUE)
dim_max(x, name, finite = TRUE)


## apply

dapply(x, names, fun, ...)



## arithmetic

to_integer(x, force = TRUE)


## converting to data.frame

explode(x)
as.data.frame(x, midpoints = NULL)


## combining

dbind(..., dots = NULL, along = NULL)



## duration dimtypes

duration_



## demtech --------------------------------------------------------------------

make_exposure(x, triangles = FALSE, method = c("weighted", "standard"))

make_exposure_fert(x, dominant = "Female", age_min, age_max)
                                      
growth



median_age(x, method)

extrapolate(x, along = NULL, n = NULL, labels = NULL)

interpolate(x, along = NULL)

life_table
life_exp

rate_to_prob
prob_to_rate


## demaccount -----------------------------------------------------------------

accession

