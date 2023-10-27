# For recitation 8
+ Hypothesis tesing in regression 
	* starting from a simple concept for hypothesis testing for sample mean
+ explain the notation (review: inference P23, OLS asymptotics p10)
	* asymptotic variance
	* actual variance (for liear function. In nonlinear case, we don;t have this)
	* variance estimator
	* explian the definition

+ relationship betwen aymptotic variance and actual variance 


+ We covered het-robust se estimates calculation in the last time
+ cover the Delta method
	* real applicaton 


+ What is delta method?
	* It's a concept of the asymptotic theorem
	* The goal is to compute the standard errors for the margins of a regression. 
		- especially powerful to estimate the se for the margin of a nonlinear ettimator like probit or logit. 
				* These are difficult to obtain the theoretical. 
	* The delta method is a general method for deriving the variance of a function of asymptotically normal random variables with known variance. 


+ for linear regression case:
	* 




+ t-statistic and the Wald statistic
	* t-statistic - distance between the two scalers
	* Wald statistic - distance between  the two vectors




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