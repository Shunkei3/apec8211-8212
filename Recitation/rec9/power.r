set.seed(230983338)
nvec <- seq(1000, 100000, 1000)                     # sample sizes
B <- 5000                                           # number of t-stats per sample size
t.stats <- matrix(nrow=B, ncol=length(nvec), NA+0)  # place to store results
for (n in 1:length(nvec)) {
  for (b in 1:B) {  # Begin Monte Carlo for sample size nvec[n]
    X <- sample(c(0, 1), nvec[n], prob=c(1-0.508, 0.508), replace=TRUE)
    p <- mean(X)
    t.stats[b, n] <- (p - 0.5)/sqrt(p*(1-p)/nvec[n])
  }
}


## Visualize what happens with a particular sample size.
print(nvec[1])
hist(t.stats[ , 1], breaks=100)
abline(v=-c, col="Red2")
abline(v=c, col="Red2")

hist(t.stats[ , 100], breaks=100)
abline(v=-c, col="Red2")
abline(v=c, col="Red2")

## Now calculate power.
alpha <- 0.01
crit <- qnorm(1 - alpha/2)
power <- function(ts, crit) {sum(abs(ts)>crit)/length(ts)}
pow <- apply(t.stats, 2, power, crit)

plot(nvec, pow, type="l", main="Power", xlab="sample size", ylab="power", frame=FALSE)
abline(h=1, lty="dashed")
