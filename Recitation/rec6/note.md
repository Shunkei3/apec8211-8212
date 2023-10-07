<!-- Suppose that researchers developed a new nitrogen fertilizer which is specifically designed for the environment in Minnesota and they want to see whether it is effective to increase crop yield.

They ran nitrogen experiments on randomly selected 10 crop fields in MN. 

Consider the following DiD model,
$$yield = \beta_0 + \beta_1 D + \beta_2 A + \beta_3 D \times A  + e$$

+ $yield$: (kg/ha)
+ $D$: Treatment dummy 
+ $A$: "After" dummy 

.content-box-green[**Question**]
+ What is the population here?
+ They got $\beta_1 > $. So does the results show the evidence that the new fertilizer is effective to increase crop yield on average in the population?


???
+ there is natural variation in the field where the research will be conducted. The field may have variations in organic matter, soil pH, or moisture, all of which may lead to variations in yield response having nothing to do with the N treatment(s) -->

---

# The plug-in estimator of $\sigma$

$$\sigma^2 = E[(X_E[X])]$$

The pug-in estimator for $\sigma^2$ is 

$$\hat{\sigma}^2 = \frac{1}{n}(X_i - \overline{X}_i)$$


---
## Simulation

sample mean of the outcomes vs use only first outcomes

.content-box-green[**Sample mean:**]

```{r}
set.seed(1234)
# the number of iterations
B = 1000
# sample size 
n = 1000
# true variance
var = 4

# storage
res_dt <- 
  data.table(
    var_hat = rep(0, length.out = B),
    var_mod_hat = rep(0, length.out = B)
  )

for(i in 1:B){
  # generate a sequence of random variables 
  x <- rnorm(n, mean = 0, sd = sqrt(var))
  # x <- rchisq(n, )
  res_dt[i, "var_hat"] <- mean((x-mean(x))^2)
  res_dt[i, "var_mod_hat"] <- sum((x-mean(x))^2)/(n-1)
}

# check the sampling distribution
ggplot(res_dt)+
  geom_density(aes(x=var_hat), fill = "blue", alpha = 0.5)+
  geom_density(aes(x=var_mod_hat), fill = "red", alpha = 0.5)


# bias
var - mean(res_dt$var_hat)
```

---