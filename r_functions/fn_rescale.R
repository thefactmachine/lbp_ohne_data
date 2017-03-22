fn_rescale <- function(vct_input, new_min, new_max)  {
  # this takes a vector as input...
  # and rescales the vector such that:
  # 1) its new min values = new_min
  # 2) its new max value = new_max
  lcl_min <- min(vct_input)
  lcl_max <- max(vct_input)
  # this is just a hack of simultaneous equation 
  lcl_transformer <- (1 / (lcl_min - lcl_max)) * (new_min - new_max)
  lcl_offset <- new_max - (lcl_max * lcl_transformer)
  vct_rescaled <- (vct_input * lcl_transformer) + lcl_offset
  return(vct_rescaled)
}
