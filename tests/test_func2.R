x<-rnorm(10)
func2 <- function(x){
  stopifnot(is.numeric(x))
  stopifnot(length(x)!=0)
  stopifnot(is.finite(x))
  stopifnot(!is.na(x))
  stopifnot(!is.nan(x))

  a = sum(x)/length(x)
  b = sum((x-a)^2)/length(x)
  c = sqrt(b)
  return(list(mean=a,var=b,sd=c))
}
func2orig <- function(x){
  return(list(mean(x), var(x), sd(x)))
}
func2(x)
func2orig(x)
stopifnot(func2(x)%in%func2orig(x))
