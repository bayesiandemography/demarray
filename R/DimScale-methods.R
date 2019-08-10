
## make_dimscale_labels --------------------------------------------------------

setMethod("make_dimscale_labels",
          signature(object = "PointsDuration"),
          function(object) {
              stop("not written yet")
          })


setMethod("make_dimscale_labels",
          signature(object = "IntervalsInteger"),
          function(object) {
              dimvalues <- object@dimvalues
              is_open_left <- object@is_open_left
              is_open_right <- object@is_open_right
              is_age <- object@is_age
              make_dimscale_labels_intervals_integer(dimvalues = dimvalues,
                                                     is_open_left = is_open_left,
                                                     is_open_right = is_open_right,
                                                     is_age = is_age)
          })


setMethod("make_dimscale_labels",
          signature(object = "IntervalsDate"),
          function(object) {
              dimvalues <- object@dimvalues
              time_unit <- object@time_unit
              is_open_left <- object@is_open_left
              is_open_right <- object@is_open_right
              make_dimscale_labels_intervals_date(dimvalues = dimvalues,
                                                  time_unit = time_unit,
                                                  is_open_left = is_open_left,
                                                  is_open_right = is_open_right)
          })

setMethod("make_dimscale_labels",
          signature(object = "IntervalsDuration"),
          function(object) {
              dimvalues <- object@dimvalues
              time_unit <- object@time_unit
              is_open_left <- object@is_open_left
              is_open_right <- object@is_open_right
              make_dimscale_labels_intervals_date(dimvalues = dimvalues,
                                                  time_unit = time_unit,
                                                  is_open_left = is_open_left,
                                                  is_open_right = is_open_right)
          })


## n_level_dimscale ------------------------------------------------------------

setMethod("n_level_dimscale",
          signature(object = "DimScale"),
          function(object) {
              dimvalues <- object@dimvalues
              length(dimvalues)
          })

setMethod("n_level_dimscale",
          signature(object = "Intervals"),
          function(object) {
              dimvalues <- object@dimvalues
              is_open_left <- object@is_open_left
              is_open_right <- object@is_open_right
              length(dimvalues) + is_open_left + is_open_right - 1L
          })


              
