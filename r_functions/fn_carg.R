fn_carg <- function(previous_value, current_value, int_year_diff) {
  # pv = previous value; fv = current value
  return(((current_value / previous_value) ^ (1 / int_year_diff)) - 1)
} # end of fn_carg