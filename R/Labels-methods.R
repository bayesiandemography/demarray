
## labels_imply_quant_scale -----------------------------------------------------------------

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabCategories"),
          function(x) {
              FALSE
          })

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabTriangles"),
          function(x) {
              FALSE
          })

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabPool"),
          function(x) {
              FALSE
          })

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabQuantiles"),
          function(x) {
              FALSE
          })

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabIntegers"),
          function(x) {
              TRUE
          })

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabGroupedIntEnumerations"),
          function(x) {
              TRUE
          })

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabGroupedIntEndpoints"),
          function(x) {
              TRUE
          })

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabCalendarQuarters"),
          function(x) {
              TRUE
          })

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabCalendarMonths"),
          function(x) {
              TRUE
          })

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabDurationsQuarters"),
          function(x) {
              TRUE
          })

## HAS_TESTS
setMethod("labels_imply_quant_scale",
          signature(x = "LabDurationsMonths"),
          function(x) {
              TRUE
          })


## make_labels ----------------------------------------------------------------

## HAS_TESTS
setMethod("make_labels",
          signature(x = "Labels"),
          function(x) {
              labels <- x@labels
              include_na <- x@include_na
              demprep::make_labels_default(labels = labels,
                                           include_na = include_na)
          })

## HAS_TESTS
setMethod("make_labels",
          signature(x = "LabIntegers"),
          function(x) {
              int_min <- x@int_min
              int_max <- x@int_max
              include_na <- x@include_na
              demprep::make_labels_integers(int_min = int_min,
                                            int_max = int_max,
                                            include_na = include_na)
          })

## HAS_TESTS
setMethod("make_labels",
          signature(x = "LabGroupedIntEnumerations"),
          function(x) {
              breaks = x@breaks
              open_first <- x@open_first
              open_last <- x@open_last
              include_na <- x@include_na
              demprep::make_labels_grouped_int_enumerations(breaks = breaks,
                                                            open_first = open_first,
                                                            open_last = open_last,
                                                            include_na = include_na)
          })

## HAS_TESTS
setMethod("make_labels",
          signature(x = "LabGroupedIntEndpoints"),
          function(x) {
              breaks = x@breaks
              open_first <- x@open_first
              open_last <- x@open_last
              include_na <- x@include_na
              demprep::make_labels_grouped_int_endpoints(breaks = breaks,
                                                         open_first = open_first,
                                                         open_last = open_last,
                                                         include_na = include_na)
          })

## HAS_TESTS
setMethod("make_labels",
          signature(x = "LabCalendarQuarters"),
          function(x) {
              break_min = x@break_min
              break_max = x@break_max
              open_first <- x@open_first
              open_last <- x@open_last
              include_na <- x@include_na
              demprep::make_labels_calendar_quarters_months(break_min = break_min,
                                                            break_max = break_max,
                                                            open_first = open_first,
                                                            open_last = open_last,
                                                            include_na = include_na,
                                                            unit = "quarter")
          })

## HAS_TESTS
setMethod("make_labels",
          signature(x = "LabCalendarMonths"),
          function(x) {
              break_min = x@break_min
              break_max = x@break_max
              open_first <- x@open_first
              open_last <- x@open_last
              include_na <- x@include_na
              demprep::make_labels_calendar_quarters_months(break_min = break_min,
                                                            break_max = break_max,
                                                            open_first = open_first,
                                                            open_last = open_last,
                                                            include_na = include_na,
                                                            unit = "month")
          })

## HAS_TESTS
setMethod("make_labels",
          signature(x = "LabDurationsQuarters"),
          function(x) {
              break_min = x@break_min
              break_max = x@break_max
              open_last <- x@open_last
              include_na <- x@include_na
              demprep::make_labels_duration_quarters_months(break_min = break_min,
                                                            break_max = break_max,
                                                            open_last = open_last,
                                                            include_na = include_na,
                                                            unit = "quarter")
          })

## HAS_TESTS
setMethod("make_labels",
          signature(x = "LabDurationsMonths"),
          function(x) {
              break_min = x@break_min
              break_max = x@break_max
              open_last <- x@open_last
              include_na <- x@include_na
              demprep::make_labels_duration_quarters_months(break_min = break_min,
                                                            break_max = break_max,
                                                            open_last = open_last,
                                                            include_na = include_na,
                                                            unit = "month")
          })

