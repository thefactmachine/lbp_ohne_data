fn_quantile <- function(vct_input, int_num_divisions) {
  vct_divisions <- seq(0,1, by = 1 / int_num_divisions)
  # this gets the break points
  vct_quantile <- quantile(vct_input, probs = vct_divisions, na.rm = TRUE, type = 7)
  vct_factor <- cut(vct_input, vct_quantile, 
                    labels = 1:int_num_divisions, include.lowest=TRUE)
  return(vct_factor %>% as.numeric())
}