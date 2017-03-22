fn_quantile_divs <- function(vct_input, int_num_divisions) {
  vct_divisions <- seq(0,1, by = 1 / int_num_divisions)
  # this gets the break points
  vct_quantile <- quantile(vct_input, probs = vct_divisions, na.rm = TRUE, type = 7)

  return(vct_quantile)
}