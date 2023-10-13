---
class: middle

```{r, echo=F, out.width = "100%"}
knitr::include_graphics("sampring.png")
```

???
+ Okay, this is the picture I used last week. I think this is useful to explains what econometric analysis is doing. 
+ In the last lab session, Aron said that the goal of the regression analysis is to understand the relationship between Y and X, or the effect of X on Y.

+ Note that we are tracking this direction. 

+ (redundant) The goal is to understand the effect of X an Y. So, first we guess the relationship in the population. Specifically, think about CEF and define the projection coefficient conceptually. Of course, we never know the true relationship between Y and X and the value of projection coefficient. 

+ Then, using sample data, we estimate the effect of $X$ on $Y$. Okay, here is the problem. The problem is that we really don't know whether how much the estimates are close to the true population parameter.  

+ So, we rely on asymptotic theories such as WLLN and CMP, to make sure the OLS estimator is actually estimating the projection coefficient if we have whole population, like checking consistency of an estimator. This is the first reason to use asymptotic analysis
  
+ Another reason to use asymptotic analysis is that, we want to do inference for the population parameter based on the estimate we get. 
+ You know, an OLS estimate we get is just a result from a single realization of sample. If we take another sample, we could get a different estimate. 
+ To deal with the sampling variability, we need the information about the sampling distribution of OLS estimator, right?
  * Although we never know about the sampling distribution, using asymptotic theorems such as CLT and delta method, we can approximate the sampling distribution, and then we can conduct statistical inference using that approximated sampling distribution. 
  * Although, we don't know whether the approximation is correct or not. In APEC8212, we will spend lots of time about this kind of topic.  

+ The asymptotic analysis is mostly theoretical, but, you know, it's better to know it so that you know what you're doing with econometric model. 
---



---
class: middle

## Implementation in R: Het-robust variance covariance estimator

```{r}
# === Load packages === #
library(data.table)
#this is the package for robust Covariance Matrix Estimators
library(sandwich) 
```

.content-box-green[**General Syntax**]

Here is the general syntax to obtain various types of VCOV (and se) estimates
```{r, eval=F}
# === Het-robust variance covariance matrix ===
vcovHC(regression result, type="type of vcov")

# === Het-robust standard error estimates ===
sqrt(diag(vcovHC(regression result, type="type of vcov")))
```

You need to specify which type of het-robust estimators should be used in `type=` (e.g., `type="HC0"`).




---

class: inverse, center, middle
name: intro

# Variance of Least squares Estimator

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=796px></html>
---
class: middle

.content-box-red[**Important!**]

$$\mathbf{V}_{\hat{\beta}} = Var[\hat{\beta}|\mathbf{X}]=\mathbf{(X^{\prime}X)^{-1}X^{\prime}DX(X^{\prime}X)^{-1}}$$

, where $\mathbf{D}=Var[\mathbf{e|X}]=E[\mathbf{ee^{\prime}|X}]$

<!-- + This is estimated. We can estimate $\mathbf{V}_{\hat{\beta}}$ with various way
  * WLS (FWLS) (If you )
  * Run OLS and report heteroskedasticity corrected standard errors -->

.content-box-green[**Exercise**]
+ Let's derive this formula. 

---
class: middle

.content-box-red[**Important!**]

$$\mathbf{V}_{\hat{\beta}} = Var[\hat{\beta}|\mathbf{X}]=\mathbf{(X^{\prime}X)^{-1}X^{\prime}DX(X^{\prime}X)^{-1}}$$

, where $\mathbf{D}=Var[\mathbf{e|X}]=E[\mathbf{ee^{\prime}|X}]$

<span style="color:blue">We don't not know about $e_i$  $Var[e|X]$. </span>

We need to estimate $Var[e|X]$. 
+ WLS (a special case of FGLS)
  * If you know something about the error 
+ Run OLS and report heteroskedasticity-robust standard errors

---
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

---
Monte