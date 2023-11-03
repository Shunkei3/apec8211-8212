---
class: middle

The Delta Method is also used to derive the asymptotic distribution of some test statistics. 

.content-box-green[**Question:**] 

What is the asymptotic distribution of t-statistic and the Wald statistic?




$$W(\theta)=(\hat{\theta}-\theta)^{\prime}\hat{\mathbf{V}}^{-1}_{\hat{\theta}}(\hat{\theta}-\theta) \xrightarrow{d} ?$$


???

+ We learned that for hypothetical testing, we need a sampling distribution of test statistics. But we never know the sampling distribution of the test statistic, so we need a asymptotic distribution of the test statistics
  * another approach that does not depend on the asymptotic distribution is bootstrapping. 

---
class: middle

The Delta Method is also used to derive the asymptotic distribution of some test statistics. 

<br>

.content-box-green[**Question:**] 

What is the asymptotic distribution of t-statistic and the Wald statistic?

See E 7.12
$$T(\theta)=\frac{\hat{\theta}-\theta}{s(\hat{\theta})} \xrightarrow{d} N(0,1)$$

See E 7.16
$$W(\theta)=(\hat{\theta}-\theta)^{\prime}\hat{\mathbf{V}}^{-1}_{\hat{\theta}}(\hat{\theta}-\theta) \xrightarrow{d} \chi^2_{k}$$

???
+ About t-stat, because $s(\hat{V}_\theta)=\sqrt{\hat{V}_\theta}$, $V_\theta= n V_{\theta}$, and $\sqrt{n}(\hat{\theta}-\theta) \xrightarrow{d} N(0, \V_{\theta})$

<!-- ## Exercise 2: 2022 Final Test Problem 2 (b) (optional)

Hansen and Mackinnon, Nielsen, and Webb note that the HC3 and CV3 variance estimators are “conservative.” What does that mean for a t-test that uses a 5% significance level? -->


Let $r(\beta)=\beta_1+\beta_2$

$$\mathbf{R}
= \nabla r(\beta) 
= \begin{bmatrix} 
  \frac{\partial}{\partial \beta_1} r(\beta) \\
  \frac{\partial}{\partial \beta_2} r(\beta) \\
  \frac{\partial}{\partial \beta_3} r(\beta) \\
  \end{bmatrix} 
= \begin{bmatrix} 1 \\ 1 \\ 0 \end{bmatrix}$$


---

+ I borrowed codes from Lecture note 12 p21
+ We 
  * 1. normal t-test with asymptotic distribution 
  * 2. t-test with bootstrap


```{r}
library(sandwich)
library(data.table)
library(lmtest)

set.seed(34567)
pop_N <- 10000
sample_N <- 200
B <- 4000


mu <- abs(rnorm(pop_N, mean=3, sd=2))
X1 <- rnorm(pop_N, mean=10, sd=2)
X2 <- rbinom(pop_N, 5, 0.5)
Y <- 0.25*X1 + 1*X2 + rnorm(pop_N, sd=2+2*X2) + rchisq(pop_N, 2)

pop_dt <- 
  data.frame(
    X1 = X1,
    X2 = X2,
    Y = Y
  )


sample_id <- sample(1:pop_N, size=sample_N, replace=FALSE)
regData <- pop_dt[sample_id,]

reg <- lm(Y ~ X1 + X2, data=regData)
se <- sqrt(diag(vcovHC(reg, type="HC3")))
t_value <- (coef(reg)-0)/se
c_value <- qnorm(p=1-0.05/2)
t_value > c_value
```

```{r}

tStats_actual <- data.frame(
  t_value_X0 = rep(NA, B),
  t_value_X1 = rep(NA, B),
  t_value_X2 = rep(NA, B)
  )

for(i in 1:B){
  sample_id_m <- sample(1:pop_N, size=sample_N, replace=FALSE)
  regData_m <- pop_dt[sample_id_m,]
  reg_m <- lm(Y ~ X1 + X2, data=regData_m)
  se_m <- sqrt(diag(vcovHC(reg_m, type="HC3")))
  tStats_actual[i,] <- (coef(reg_m)-0)/se_m
}
```



```{r}
# === Paired bootstrap === #
# goal: create bootstrap distribution of t-statistic
tStats <- data.frame(
  t_value_X0 = rep(NA, B),
  t_value_X1 = rep(NA, B),
  t_value_X2 = rep(NA, B)
  )
  
for (b in 1:B) {
  # === Resampling === #
  randomRows <- sample(sample_N, replace=TRUE)
  regData_b <- regData[randomRows, ]
  # === Reestimate === #
  reg_b <- lm(Y~X1+X2, data=regData_b)
  
  # === get t-value with HC standard error  === #
  se_b <- sqrt(diag(vcovHC(reg_b, type="HC3")))
  t_value_b <- (coef(reg_b) - coef(reg))/se_b
  
  # --- Save the t-values --- #
  tStats[b, ] <- t_value_b
}

# each column in `tStats` contains bootstrapped t-values
head(tStats)
c_value_b <- sapply(tStats, function(x) quantile(x, prob=0.975))
t_value > c_value_b
```


```{r}
library(ggplot2)
ggplot()+
  geom_histogram(data=tStats_actual, aes(x=t_value_X0), fill="green", alpha=0.5, bins = 50)+
  geom_histogram(data=tStats, aes(x=t_value_X0), fill="blue", alpha=0.5, bins = 50)

quantile(tStats_actual$t_value_X0)
quantile(tStats$t_value_X0)
qnorm(p=c(0.25))
```

---

# Hypothesis testing: Basics

From Lecture note 11, p7

```{r, echo=F, out.width = "40%"}
knitr::include_graphics("type.png")
```

<br>

.bg-washed-green.b--dark-green.ba.bw2.br3.shadow-2.ph2.mt2[
<b>size of a hypothesis test = Pr(Type I error)</b>

**Verbally**: The probability that we will falsely reject the null hypothesis

<b>power of a test =1-Pr(Type II error)</b>

**Verbally**: The probability that we will correctly reject the null hypothesis
]

<!-- We set the size of the test (called *significance level* $\alpha$) to  -->

<!-- .bg-washed-green.b--dark-green.ba.bw2.br3.shadow-2.ph2.mt2[
**<span style="color:red">Type I error</span>**

]
 -->
???

+ Type I: a false rejection of $H_0$ when $H_0$ is true
+ Type II: a false acceptance of $H_0$ when $H_1$ is true

+ We have two types of error 
+ To evaluate these two types of errors in terms of probability. 