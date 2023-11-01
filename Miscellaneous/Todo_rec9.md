+ t-statistic and the Wald statistic
	* t-statistic - distance between the two scalers
	* Wald statistic - distance between  the two vectors

+ Practice how to apply Wald test
	* calculate confidence interval

+ Explain Boostrap idea
+ Do Boostrap 
	* it might be interesting to simulate the asymptotic refinement. 

+ **Wild bootstrap DGP**

Key words
+ empirical distribution
	* emplical distribution converges to the true sampling distribution 
	* check the theory behind this 
		- when samp 

+ What is the justification of bootstrapping?
	* for nonlinear estimator 

+ only works in model with additive errors (no probit)
	* what is additive errors?


---
## Difference between F and the Wald test
+ If we are willing to assume that the error are normally distributed (called normal distribution), both t-statistic and F-statistic have small sample property. That is, t-statistic follows t-distribution (with n-k degree of freedom) and F-statistic follows F-distribution with k degree of freedom.

+ Asymptotically, F and the Wald test are equivalent (both asymptotically converge to chi-square distribution.)


## Interesting topics

+ Power of the test
	* Nominal size 
+ The connection between standard error estimation and the inference



---


# For recitation 9

## Topics:
+ test statistics
+ Bootstrapping 
	* Paul's paper


## Main test statistc
+ Inference P34:
	* Wald test of nonlinear hypothesis is not reliable (asymptotically correct though)
		- actual type I error > alpha


## Various tests
+ Wald
	* based on the distance between null and actural value

+ Criterion-based tests
	* minima of the criterion function
	* Example:
		- liklihood-ratio test
		- traditional F-tests
	+ based on the distance between the liklihood function


+ Minimum distance (m.d.) estimation

+ Hausman tests
	* (classic example: test for endoneneity of X1)
	* like OLS vs 2SLS


+ Score tests
	* we look at **derivative**
		- in econometrics, scores usually refers to derivative
	* Lagrange-multiplier tests


## Interval estiamtor
+ Exercise to interprete the confidence interval
+ Practice to build a confidence interval





#