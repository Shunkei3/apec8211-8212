
+ the impact of offering eyeglasses to students in developing countries
+ 



## Monte Carlo simulation

.content-box-green[**Model**]



---

```{r}
set.seed(927834)

N <- 1000 # number of observations
B <- 1000 # number of simulations

b_hat_store <- rep(0, B) # beta hat storage
t_stat_store <- rep(0, B) # t-stat storage

c_value <- qt(0.975, N - 2) # critical value
x <- runif(N, 0, 1) # x (fixed across iterations)

for (i in 1:B){
#--- generate data ---#
het_u <- 3 * rnorm(N, mean = 0, sd = 2 * x) # heteroskedastic error
y <- 1 + het_u # y
data_temp <- data.frame(y = y, x = x)
#--- regression ---#
ols_res <- lm(y ~ x, data = data_temp)
b_hat <- ols_res$coef['x'] # coef estimate on x
b_hat_store[i] <- b_hat # save the coef estimate
vcov_ols <- vcov(ols_res) # get variance covariance matrix
t_stat_store[i] <- b_hat / sqrt(vcov_ols['x', 'x']) # calculate t-stat
}
```

