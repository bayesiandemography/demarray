
## demarray -------------------------------------------------------------------

collapse_to(.data, ..., .wt = NULL)
sum_within(x, ...)
avg_within(x, ..., wt)

collapse_at(.data, ..., .wt = NULL)
sum_along(x, ...)
avg_along(x, ..., wt)

broadcast(x, name, labels)

.collapse(x, transform)

.expand(x, transform)

make_transform(x, y, trim = TRUE)

collapse(x, ..., old, new)

collapse_custom(x, ..., breaks)

collapse_multi(x, ..., width, break_min, break_max)

collapse_lifetab(x, ..., break_max)

collapse_year(x, ...)

collapse_quarter(x, ...)

collapse_month(x, ...)

filter(x, ...)

with_filter(x, ...)

trim_to(x, y)

pad_to(x, y, fill = NULL)

add_dim(x, name, label)

set_open_first(x, name)

set_open_last(x, name)

set_break_min(x, name, value)

set_break_max(x, name, value)

is_regular(x, name)

step_length(x, name)

dimtypes(x)

dimtypes<-(x, value)

align_pair(x, name)

is_compatible(x, y, trim = FALSE)???

are_compatible(x, y, trim = TRUE)???



## demtech --------------------------------------------------------------------

explode(x)

median_age(x, method)

to_net(x, ...)

to_pool(x, ..., direction)


extrapolate



## demaccount -----------------------------------------------------------------

accession

