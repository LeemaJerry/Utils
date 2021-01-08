
normalize <- function(y) {
  if(is.numeric(y))
  {
    x = y
    #global variable assignment
    scalemin <<- min(x)
    scalemax <<- max(x)
    
    if(scalemin!=scalemax){
      val = ((x - min(x)) / (max(x) - min(x))) * 1

    } else val = x
    
    return(val)
  } else return(y)
  
}

denormalize <- function(x) {
  if(is.numeric(x)){
    return((((scalemax-scalemin)*x)+scalemin)/1)
  }else
  {
    return(x)
  }
}
