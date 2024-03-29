nff = function(x = NULL, n = NULL,  plot = TRUE, add = FALSE, main = NULL){
  
  dff = fft(x) # discrete fourier transform
  
  t = seq(from = 1, to = length(x))  # sampled time 
  
  ndff = array(data = 0, dim = c(length(t), 1L)) # new spectrum
  
  ndff[1] = dff[1] # DC component
  # n = no. of harmonics
  if(n != 0){ 
    ndff[2:(n+1)] = dff[2:(n+1)]  # positive frequencies
    ndff[length(ndff):(length(ndff) - n + 1)] = dff[length(x):(length(x) - n + 1)] # negative frequencies
  }
  
  #inverse
  indff = fft(ndff/(length(x)), inverse = TRUE)
  idff = fft(dff/(length(x)), inverse = TRUE)
  
  if(plot){
    if(!add){
      plot(x = t, y = x, pch = 16L, xlab = "Time", ylab = "Measurement",
           main = ifelse(is.null(main), paste(n, "harmonics"), main))
      lines(y = Mod(idff), x = t, col = "blue")
    }
    lines(y = Mod(indff), x = t, col = "red")
  }
  ret = data.frame(time = t, y = Mod(indff)) #transformed df
  return(ret)
}


# Fourier series addition

x = seq(1, (16*60*24))
x = ts(x, frequency = (60*24))
xf = fourier(x, K = 5)
xf_head = cbind.data.frame("time" = seq(1:720), head(xf,720))
data <- xf_head
Molten <- melt(data, id.vars = "time")
library(reshape)
ggplot(Molten, aes(x = time, y = value, colour = variable)) + geom_line(size = 1) +
   labs(x="Period", y="Value")
