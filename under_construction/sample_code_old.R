
x %>% subarray(where = list(age = "15+"))

x %>% subarray(age > 15)




replace_when <- funcion(...) {
    dots <- list(...)
    
}


structural_zeros <- schools %>%
    collapse_to(region, area) %>%
    replace_when(region == "Bangkok" & area == "Rural" ~ 0L,
                 TRUE ~ 1L) %>%
    Values()


extract_array(x, where)

extract_array(deaths, where = list(age = 1:4, sex = "Female", time = 12:13))

extract_array(deaths, where = list(age = "<15", sex = "Female", time = "2000-2015"))


collapse_to(c("a", "s"))




subarray(deaths,
         age = c("<5", "15+"),
         sex = "Female",
         region = tourist_dest,
         occupation = c(1:4, 10))

rw(scale = 0.1, incr = "30+")

rw(scale = 0.1, incr = list(age = c("<10", "50+"), 



              

extract_array(deaths,
              where = list(age = index_dim(deaths, name = "age", to = "15-19"),
                           sex = "Female",
                           time = index_dim(deaths, name = "time", equals = c("2018", "2019"))))


   
index_dim(deaths, name = "ans", from = "0-4", to = "20-24")

subarray(age > 5 & sex == "Female")

replace_where(age > 5 & sex == "Female", 0L)


rw(scale = 0.01,
   incr = age > 30,
   decr = age < 5)

i
   

exch(zero = non_tourist)

exch(reg_org == reg_dest ~ 0)






 <- metadata(schools, dims = c("region", "area"))
structural_zeros <- Values(1L, metadata = md)
structura_zeros["Bangkok", "Rural"] <- 0L

where_array(x, name, equals = NULL, min = NULL, max = NULL)

where_dim(labels, equals = NULL, min = NULL, max = NULL)



          


structural_zeros <- Values(1L, 

structural_zeros <- schools %>%
    sum_to(dims = c("region", "area"))
structural_zeros[] <- FALSE









nhes_propn <- cotdata::nhes_est_obese %>%
    dtabs(mean ~ age + sex + year) %>%
    Values()

popn <- readRDS("out/popn.rds") %>%
    align_to(nhes_pron, trim = TRUE)

nhes_est_obese <- (nhes_propn * popn) %>%
    to_integer(force = TRUE)


population <- census %>%
    interpolate() %>% ## or let function deduce these
    extrapolate(n = 1)

mult <- Counts(c(0.45, 0.55),
               dim = 2,
               dimnames = list(sex = c("Female", "Male")))

births <- (mult * births_un) %>%
    to_integer()


exposure <- make_exposure(popn)
death_rates <- deaths_un / exposure
deaths <- death_rates * exposure
check_same_meta(exposure, deaths)






constraint <- ValuesOne(NA_integer_,
                        labels = c("2-4", "5-9", "10-14", "15-18"),
                        name = "age")

popn <- popn %>%
    collapse_to(region, age, sex) %>%
    as.data.frame()

initial <- runif_array(min = schools, max = 1.4 * schools)
initial <- rpois_array(schools)
initial <- rpois_array(schools, prob = 0.4)

schools_all <- schools_all %>%
    Counts() %>%
    extrapolate(along = "age", labels = "2", type = "zero")

schools_all <- schools_all %>%
    combine_custom(name = "age", breaks = c(2, 5, 10, 15, 18))

schools_obese %>% replace_vals(region == "Bangkok" & area == "Rural" & age > 5 ~ 0,
                               region == "Northeast" & age > 30 ~ exp(value))

schools_obese <- schools_obese %>%
    mutate_array(region == "Bangkok" & area == "Rural" ~ 0L)

schools_obese <- schools_obese %>%
    mutate_array(region == "Bangkok" & area == "Rural" ~ x^2)





schools_obese %>% replace_array(list(region = "Bangkok",
                                     area = "Rural"),
                                val = 
                          
                               area = name = c("region", "area"),
                         equals = c("Bangkok", "Rural"),
                         fill = 0L)


schools %>% subarray(age > 10)
schools %>% subdim(name = "age", min = 10)




           







## demarray -------------------------------------------------------------------


## wherever 'name' appears, can use base name, and process two dimensions at once


## Aggregating - dimensions

## Low level

sum_except(x, names)
sum_along(x, names)
mean_except(x, names, wt)
mean_along(x, names, wt)

## High level

collapse_except(x, ..., wt = NULL)     # weights required if Counts; uses NSE
collapse_along(x, ..., wt = NULL)  # prohibited if Values; uses NSE


## Aggregating - categories (note that inherit starting dates, unbounded, etc from disag categories)

## Low level

sum_cat(x, name, old, new)
sum_custom(x, name, breaks)
sum_multi(x, name, width, break_min, break_max)
sum_lifetab(x, name, break_max)
sum_year(x, name)
sum_quarter(x, name)
sum_month(x, name)

mean_cat(x, name, old, new, wt = NULL)
mean_custom(x, name, breaks, wt = NULL)
mean_multi(x, name, width, break_min, break_max, wt = NULL)
mean_lifetab(x, name, break_max, wt = NULL)
mean_year(x, name, wt = NULL)
mean_quarter(x, name, wt = NULL)
mean_month(x, name, wt = NULL)

## High level

combine_cat(x, name, old, new, wt = NULL)
combine_custom(x, name, breaks, wt = NULL)
combine_multi(x, name, width, break_min, break_max, wt = NULL)
combine_lifetab(x, name, break_max, wt = NULL)
combine_year(x, name, wt = NULL)
combine_quarter(x, name, wt = NULL)
combine_month(x, name, wt = NULL)


## Broadcasting

broadcast_along(x, ..., dots = NULL)







## Disaggregating - dimensions

## Low level - or replace with other functions?

dim_dis_along(x, name, labels, dimtype = NULL, wt, rand = FALSE)
dim_rep_along(x, name, labels, dimtype = NULL, scale = 1)

## High level

extend_along(x, name, labels, dimtype = NULL) # Values only




## Disaggregating - dimensions

dis(x, name, wt, rand = FALSE)
dis_custom(x, name, breaks, wt = NULL)
dis_multi(x, name, width, break_min, break_max, wt = NULL)
dis_lifetab(x, name, break_max, wt = NULL)
dis_year(x, name, wt = NULL)
dis_quarter(x, name, wt = NULL)
dis_month(x, name, wt = NULL)



extend_along(x, name, labels, dimtype = NULL, wt = NULL, rand = FALSE)


split_cat(x, name, old, new, wt = NULL, rand = FALSE)
split_custom(x, name, breaks, wt = NULL, rand = FALSE)
split_multi(x, name, width, break_min, break_max, wt = NULL, rand = FALSE)
split_lifetab(x, name, break_max, wt = NULL, rand = FALSE)
split_year(x, name, wt = NULL, rand = FALSE)
split_quarter(x, name, wt = NULL, rand = FALSE)
split_month(x, name, wt = NULL, rand = FALSE)


add_points(x, name, at = NULL, width = NULL)

trim_to(x, y, test = FALSE)
pad_to(x, y, pad = NULL, test = FALSE)



align_dims(x, y, wt = NULL)

align_cat(x, y, name, wt = NULL)



    


align_to <- function(x, y, dim = TRUE, cat = TRUE, trim = TRUE, pad = NULL, test = FALSE) {
    if (dim)
        x <- (x = x, y = y, test = test)
    if (combine)
        x <- combine_to(x = x, y = y, test = test)
    if (trim)
        x <- trim_to(x = x, y = y, test = test)
    if (pad)
        x <- pad_to(x = x, y = y, pad = pad, test = test)
    x
}
    

align_pair(x, name)



subarray(x, ..., dots = NULL)


to_quantiles(x, prob = c(0.025, 0.25, 0.5, 0.75, 0.975), na_rm = FALSE)
summary_iter(x, fun, ...)
thin_iter(x, n)

pivot_agetime(x, to)

attach_map(x, name, map)

to_net(x, name)

to_pool(x, name)

to_integer(x, force = TRUE)

explode(x)

set_open_first(x, name)

set_open_last(x, name)

set_break_min(x, name, value)

set_break_max(x, name, value)

is_regular(x, name)

step_length(x, name)

dimtypes(x)

dimtypes<-(x, value)

dbind(..., dots = NULL, along = NULL)




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

