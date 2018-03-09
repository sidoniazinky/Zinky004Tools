x <- c(1,2,3)
p <- c(.25,.25,.5)
d <- data.frame(x,p)
badx <- c(NA,2,3)
badd <- data.frame(badx,p)
func5 <- function(d) {
    
    stopifnot(is.numeric(d$x))
    stopifnot(is.numeric(d$p))
    
    stopifnot(length(d$x)!=0)
    stopifnot(length(d$p)!=0)
    
    stopifnot(is.finite(d$x))
    stopifnot(is.finite(d$p))
    
    stopifnot(!is.na(d$x))
    stopifnot(!is.na(d$p))
    
    stopifnot(!is.nan(d$x))
    stopifnot(!is.nan(d$p))
    
    stopifnot(all.equal(sum(d$p),1))
    
    a = sum(d$x * d$p)
    b = sum(((d$x - a)^2) * d$p)
    c = sqrt(b)
    return(list(mean=a,var=b,sd=c))
    
}
func5orig <- function(d){
  return(
    list(
      weighted.mean(d$x,d$p),
      sum(p * (x - weighted.mean(d$x,d$p))^2),
      sqrt(sum(p * (x - weighted.mean(d$x,d$p))^2))))
}
func5(badd)
func5(d)
func5orig(d)