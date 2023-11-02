library(sandwich)

## Make up some data
n <- 1000
age.range <- 21:70
ages <- length(age.range)

d <- data.frame(age = sample(age.range, n, replace=TRUE))
d$logY <- 1 + 0.05*d$age + 0.0002*d$age^2 + rnorm(n, sd=0.04*d$age)

#############################################################################
## Regression and variance matrix.
reg <- lm(logY ~ age + I(age^2), data=d)

V <- vcovHC(reg, type="HC3")

#############################################################################
## Find the conf. band for Yhat.
Yhat <- predict(reg, newdata=data.frame(age = age.range))

se.vec <- rep(NA, ages)
for (a in 1:ages) {
    x <- c(1, age.range[a], age.range[a]^2) # R treats as a column vector for matrix mult.
    se.vec[a] <- sqrt( t(x) %*% V %*% x )
}

conf <- data.frame(age = age.range,
                   Yhat, 
                   U = Yhat + qnorm(0.975)*se.vec,
                   L = Yhat + qnorm(0.025)*se.vec)

with(conf, {
    plot(age, Yhat, type="l", lwd=2, col="Red4", 
         ylim=c(0, 7), frame=FALSE,
         main="Predicted Y with 95% confidence band")
    points(age, U, type="l", lty="dashed", col="Red4")
    points(age, L, type="l", lty="dashed", col="Red4")
})                

#############################################################################
## Find the conf. band for marginal effect of age (as derivative)
betahat <- coef(reg)
ME <- betahat["age"] + 2*betahat["I(age^2)"]*age.range

se.vec <- rep(NA, ages)
for (a in 1:ages) {
    x <- c(0, 1, 2*age.range[a]) 
    se.vec[a] <- sqrt( t(x) %*% V %*% x )
}

conf <- data.frame(age = age.range,
                   ME, 
                   U = ME + qnorm(0.975)*se.vec,
                   L = ME + qnorm(0.025)*se.vec)

with(conf, {
    plot(age, ME, type="l", lwd=2, col="Blue4", 
         ylim=c(0, 0.12), frame=FALSE,
         main="ME of age with 95% confidence band")
    points(age, U, type="l", lty="dashed", col="Blue4")
    points(age, L, type="l", lty="dashed", col="Blue4")
})                
