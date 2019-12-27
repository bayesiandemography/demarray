
## HAS_TESTS
LabCategories <- function(labels,
                          include_na) {
    labels <- as.character(labels)
    methods::new("LabCategories",
                 labels = labels,
                 include_na = include_na)
}

## HAS_TESTS
LabTriangles <- function(include_na) {
    labels <- c("Lower", "Upper")
    methods::new("LabTriangles",
                 labels = labels,
                 include_na = include_na)
}

## HAS_TESTS
LabPool <- function(include_na) {
    labels <- c("Ins", "Outs")
    methods::new("LabPool",
                 labels = labels,
                 include_na = include_na)
}

## HAS_TESTS
LabQuantiles <- function(labels,
                         include_na) {
    methods::new("LabQuantiles",
                 labels = labels,
                 include_na = include_na)
}

## HAS_TESTS
LabIntegers <- function(int_min,
                        int_max,
                        include_na) {
    methods::new("LabIntegers",
                 int_min = int_min,
                 int_max = int_max,
                 include_na = include_na)
}

## HAS_TESTS
LabGroupedIntEnumerations <- function(breaks,
                                      open_first,
                                      open_last,
                                      include_na) {
    methods::new("LabGroupedIntEnumerations",
                 breaks = breaks,
                 open_first = open_first,
                 open_last = open_last,
                 include_na = include_na)
}

## HAS_TESTS
LabGroupedIntEndpoints <- function(breaks,
                                   open_first,
                                   open_last,
                                   include_na) {
    methods::new("LabGroupedIntEndpoints",
                 breaks = breaks,
                 open_first = open_first,
                 open_last = open_last,
                 include_na = include_na)
}

## HAS_TESTS
LabCalendarQuarters <- function(break_min,
                                break_max,
                                open_first,
                                open_last,
                                include_na) {
    methods::new("LabCalendarQuarters",
                 break_min = break_min,
                 break_max = break_max,
                 open_first = open_first,
                 open_last = open_last,
                 include_na = include_na)
}

## HAS_TESTS
LabCalendarMonths <- function(break_min,
                              break_max,
                              open_first,
                              open_last,
                              include_na) {
    methods::new("LabCalendarMonths",
                 break_min = break_min,
                 break_max = break_max,
                 open_first = open_first,
                 open_last = open_last,
                 include_na = include_na)
}

## HAS_TESTS
LabDurationsQuarters <- function(break_min,
                                 break_max,
                                 open_last,
                                 include_na) {
    methods::new("LabDurationsQuarters",
                 break_min = break_min,
                 break_max = break_max,
                 open_last = open_last,
                 include_na = include_na)
}

## HAS_TESTS
LabDurationsMonths <- function(break_min,
                               break_max,
                               open_last,
                               include_na) {
    methods::new("LabDurationsMonths",
                 break_min = break_min,
                 break_max = break_max,
                 open_last = open_last,
                 include_na = include_na)
}














