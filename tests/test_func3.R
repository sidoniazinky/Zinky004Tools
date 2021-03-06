options(warn=-1)
x<-rnorm(10)
func3 <- function(x){
  alpha <- pi
  log <- function(alpha)
    sum(dgamma(x, shape = alpha, log = TRUE))
  interval <- mean(x) + c(-1,1) * 3 * sd(x)
  interval <- pmax(mean(x) / 1e3, interval)

  oout<- optimize(log, maximum = TRUE, interval)
  return (oout$maximum)
}
func3(x)
