
# Aggregation function that handles mixed data types
# returns mean if numeric
# returns mode if the others

aggregate_mix <- function(x){ 
  
  if (is.numeric(x)) { 
    return(mean(x))   } 
  else{
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
}
