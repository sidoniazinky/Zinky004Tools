x <- c(1,2,3)
p <- c(.25,.25,.5)
d <- data.frame(x,p)
func4 <- function(d)
  {
    a = sum(d$x * d$p)
    b = sum(((d$x - a)^2) * d$p)
    c = sqrt(b)
    return(list(mean=a,var=b,sd=c))

  }
func4orig <- function(d){
  return(
    list(
      weighted.mean(d$x,d$p),
      sum(p * (x - weighted.mean(d$x,d$p))^2),
      sqrt(sum(p * (x - weighted.mean(d$x,d$p))^2))))
}
func4(d)
func4orig(d)
