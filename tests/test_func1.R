x<-rnorm(10)
func1 <- function(x){
  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}
func1orig <- function(x){
  return(list(mean(x), var(x), sd(x)))
}
func1(x)
func1orig(x)
stopifnot(func1(x)%in%func1orig(x))

