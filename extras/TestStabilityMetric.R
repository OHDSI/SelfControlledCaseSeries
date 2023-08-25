# Some code to evaluate the new stability metric using simulation
n <- 150
trueRatio <- exp(rnorm(n, mean = 0, sd = 0.1))
maxRatio <- 1.25

e <- runif(n, 500, 1500)
o <- rpois(n, lambda = e*trueRatio)


smoothMax <- function(x, y) {
  return(ifelse(abs(x-y) > 100, pmax(x,y), x + log(1 + exp(y-x))))
}
logLikelihood <- function(x) {
  return(-sum(smoothMax(dpois(o, e*x, log = TRUE), dpois(o, e/x, log = TRUE))))
}
likelihood <- function(x) {
  return(exp(-logLikelihood(x)))
}
vectorLikelihood <- function(x) {
  return(sapply(x, likelihood))
}
x <- seq(1, 10, by = 0.1)
ll <- sapply(x, logLikelihood)
maxX <- x[max(which(!is.na(ll) & !is.infinite(ll)))]
minX <- x[min(which(!is.na(ll) & !is.infinite(ll)))]
xHat <- optim(1.5, logLikelihood, lower = minX, upper = maxX, method = "L-BFGS-B")$par
l0 <- integrate(vectorLikelihood, lower = 1, upper = maxRatio)$value
l1 <- integrate(vectorLikelihood, lower = maxRatio, upper = Inf)$value
llr <- 2*(log(l1) - log(l0))
if (is.nan(llr)) {
  if (xHat > maxRatio) {
    p <- 0
  } else {
    p <- 1
  }
} else {
  p <- pchisq(llr, 1, lower.tail = FALSE)
}

writeLines(sprintf("Estimated ratio: %0.2f, true ratio: %0.2f, p for ratio > %0.2f: %0.2f",
                   xHat, exp(mean(abs(log(trueRatio)))), maxRatio, p))

