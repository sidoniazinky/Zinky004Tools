func1 = function(theta, x) dgamma(x, shape = theta, log = TRUE)

func7 <- function(x, func, interval){
  
  f7 <- function(theta, x)
  {sum(func(theta, x))}
  
  oout<- optimize(f7, maximum = TRUE, interval, x=x)
  return(oout$maximum)
}
x1 <- scan(url("http://www.stat.umn.edu/geyer/3701/data/q1p3.txt"))
result7_gamma <- func7(x1,func1,c(0,3))
result7_gamma