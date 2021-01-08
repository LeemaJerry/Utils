
normalize <- function(y) {
  if(is.numeric(y))
  {
    x = y
    scalemin <<- min(x)
    scalemax <<- max(x)
    
    if(scalemin!=scalemax){
      val = ((x - min(x)) / (max(x) - min(x))) * 1

    } else val = x
    
    return(val)
  } else return(y)
  
}
