fn_fuzz <- function (vct_input, flt_pc_fuzz) {
  
  
  # apply a specific percentage "fuzz" to vct_input
  # uses a uniform distribution
  # needs to use absolute numbers initial and t
  # then converts these to back to negative numbers
  vct_fuzzed_val <- sapply(vct_input, function(x) 
    runif(1, abs(x) * (1 - flt_pc_fuzz), 
          abs(x) * (1 + flt_pc_fuzz)))
  
  # the following preserves the orginal sign
  return(vct_fuzzed_val * sign(vct_input))
  
}


