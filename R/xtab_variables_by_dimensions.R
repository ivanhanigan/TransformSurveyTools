
xtab_variables_by_dimensions <- function(dat = NULL,
                                         pid = NULL,
                                         variables = NULL,
                                         dimensions = NULL,
                                         weights = NULL, 
                                         ci_prop = NULL){
d2out_list <- list(0)
for(i in 1:length(variables)){
#  i  = 1

  mainvar  <- variables[i]
  mainvar
  d2out2_list <- list(0) 
  for(j in 1:length(dimensions)){
    dimension <- dimensions[j]
    
    d2out2_list[[j]] <- xtab_by_dimensions(dat = dat,
      pid = pid,
      mainvar = mainvar,
      dimension = dimension,
      weights = weights, 
      ci_prop = ci_prop
      )
  }
  d2out_list[[i]] <- rbindlist(d2out2_list)
  
}

#d2out_list
outout <- rbindlist(d2out_list, fill = T)

return(outout)
}
