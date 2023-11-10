```{r, echo=F, eval=F}
# paired, wild, and asymptotic bootstrap

library(sandwich)
library(car)
library(foreach)
library(data.table)
library(car)

gen_data <- function(sample_size){
  # sample_size=100
  # X1 <- rchisq(sample_size, df=10)
  # X2 <- rnorm(sample_size, mean=10, sd=3)
  # X3 <- abs(round(rnorm(sample_size, mean=20, sd=10)))
  # X4 <- rbinom(sample_size, size=1, 0.5)
  # e <- rnorm(sample_size, mean=1, sd=2+2*T+X3)

  # suppose that 
  data <- 
    data.table(
      X1 = rchisq(sample_size, df=10),
      X2 = rnorm(sample_size, mean=10, sd=3),
      X3 = abs(round(rnorm(sample_size, mean=20, sd=10))),
      X4 = rbinom(sample_size, size=1, 0.5),
      e = rnorm(sample_size, mean=1, sd=2+2*T+X3)
    ) %>%
    .[,Y :=  2*X1 + 1*X2 + 1*X3 + 0.5*X3*X4 + 2*X4, + e] %>%
    .[,X3_X4 := X3*X4]
  return(data)
  }


boot_paired <- function(lm_formula, data, restrict){
  # lm_formula=original_fomula; data=regData; restrict=restrict
  paired_w <- 
    foreach(b=1:B, .combine="c")%do%{
    # === Resampling === #
    rendomRows <- sample(nrow(data), replace=TRUE)
    regData_p <- data[rendomRows,]
    # === Run a regression === #
    reg_p <- 
      lm(lm_formula, data=regData_p)
    # === wald test === #
    Wp_test <- 
      linearHypothesis(reg_p, restrict, test="Chisq", vcov=sandwich::vcovHC)
    # the derived value of the Wald statistic is saved
    Wp_test[["Chisq"]][[2]]
    }
  # === return rejection rate === #
  return(paired_w)
}

# wild_boot1 <- function(x_var, var_test, data, restrict){
#   # --- Run a regression with restriction  --- #
#   rest_x_bar <- x_var[!(grepl(var_test, x_var))]
#   restrict_formula <- as.formula(paste0("Y~", paste0(rest_x_bar, collapse = "+")))
#   restrict_reg <- lm(restrict_formula, data=data)
#   # --- save the residual  --- #
#   ehat <- resid(restrict_reg)
#   # --- fitted values derived from the restricted regression --- #
#   yhat <- fitted(restrict_reg)

#   # --- Preparation for wild bootstrap: Update a model --- #
#   # note: y_b is defined with yhat and new residuals
#   wild_formula <- as.formula(paste("y_b ~", paste0(x_var, collapse = "+")))           

#   wild_w <- 
#     foreach (b = 1:B, .combine="c") %do% {
#     # --- create bootstrap residual --- #
#     z_b <- sample(c(-1,1), length(ehat), prob=c(0.5,0.5), replace=TRUE)
#     e_b <- ehat*z_b 
#     # --- Then, define synthetic dependent variable --- #
#     data$y_b <- yhat + e_b
#     # --- Then, estimate coefficients with the original model --- #
#     reg_b <- lm(wild_formula, data=data)
#     # --- Wald test --- #

#     Wb_test <- 
#       linearHypothesis(reg_b, restrict, test="Chisq", vcov=sandwich::vcovHC)
#     # the derived value of the Wald statistic is saved
#     Wb_test$Chisq[2]
#     }
#   return(wild_w)
# }


sim_boot2 <- function(sample_size, x_var, var_test){
  # test run
  # sample_size=300; x_var = c("X1", "X3", "X4", "X3_X4"); var_test = c("X3", "X3_X4")
  
  # === Generate data === #
  regData <- gen_data(sample_size)
  # === Run a regression === #
  original_fomula <- as.formula(paste("Y~", paste0(x_var, collapse = "+")))
  original_reg <- lm(original_fomula, data=regData)
  
  beta_names <- names(coef(original_reg))
  restrict <- paste0(var_test,"=0")

  # beta_hat <- coef(original_reg) 
  # w_restrict <- paste0(beta_names[grep(var_test, beta_names)],"=", c(beta_hat[["T"]], beta_hat[["I(T * X3)"]]))
  # === Asymptotic inference === #
  w_test <- 
    linearHypothesis(original_reg, restrict, test="Chisq", vcov=sandwich::vcovHC)
  w <- w_test[["Chisq"]][[2]] 
  reject_gaussian <- w > qchisq(p=.95, df=length(restrict)) 

  # === Paired bootstrap === #
  # paired_w <- 
  #   boot_paired(lm_formula=original_fomula, data=regData, restrict=w_restrict)
  # reject_paired <- w > quantile(paired_w, probs=0.95)[[1]]

  # === Wild bootstrap === #
  # wild_w <- 
  #   wild_boot(x_var=x_var, var_test=var_test, data=regData, restrict=w_restrict)
  # reject_wild <- w > quantile(wild_w, probs=0.95)[[1]]
  wild_w <- 
    boottest(
      original_reg,
      B = 4999,
      param = var_test,
      R = c(1, 1),
      r = 0
    )
  reject_wild <- w > quantile(wild_w, probs=0.95)[[1]]
  
  return(
    data.table(
      sample_size = sample_size,
      reject_gaussian = reject_gaussian,
      reject_paired =reject_paired,
      reject_wild = reject_wild
      )
    )
}


sim_boot <- function(sample_size, x_var, var_test){
  # test run
  # sample_size=300; x_var = c("X1", "X3", "X3_X4"); var_test = "T"
  
  # === Generate data === #
  regData <- gen_data(sample_size)
  # === Run a regression === #
  original_fomula <- as.formula(paste("Y~", paste0(x_var, collapse = "+")))
  original_reg <- lm(original_fomula, data=regData)
  
  beta_names <- names(coef(original_reg))
  restrict <- paste0(beta_names[grep(var_test, beta_names)],"=0")

  beta_hat <- coef(original_reg) 
  w_restrict <- paste0(beta_names[grep(var_test, beta_names)],"=", c(beta_hat[["T"]], beta_hat[["I(T * X3)"]]))
  # === Asymptotic inference === #
  w_test <- 
    linearHypothesis(original_reg, restrict, test="Chisq", vcov=sandwich::vcovHC)
  w <- w_test[["Chisq"]][[2]] 
  reject_gaussian <- w > qchisq(p=.95, df=length(restrict)) 

  # === Paired bootstrap === #
  paired_w <- 
    boot_paired(lm_formula=original_fomula, data=regData, restrict=w_restrict)
  reject_paired <- w > quantile(paired_w, probs=0.95)[[1]]

  # === Wild bootstrap === #
  # wild_w <- 
  #   wild_boot(x_var=x_var, var_test=var_test, data=regData, restrict=w_restrict)
  # reject_wild <- w > quantile(wild_w, probs=0.95)[[1]]
  wild_w <- 
    boottest(
      original_reg,
      B = 4999,
      param = c("treatment", "ideology1"),
      R = c(1, 1),
      r = 0
    )
  reject_wild <- w > quantile(wild_w, probs=0.95)[[1]]
  
  return(
    data.table(
      sample_size = sample_size,
      reject_gaussian = reject_gaussian,
      reject_paired =reject_paired,
      reject_wild = reject_wild
      )
    )
}

# library(parallel)

B=4999
test <- 
  sim_boot(sample_size=100, x_var = c("X1", "T", "I(T*X3)"), var_test = "T")

library(future)
library(doFuture)
plan(multisession, workers=availableCores()-1)
library(doRNG)
registerDoFuture()
registerDoRNG(seed=82122020)


results <-
  foreach(N=c(100, 200, 500, 1000), .combine='rbind') %:%
      foreach(i=1:5, .combine="rbind")%dopar%{
        sim_boot(sample_size=N, x_var = c("X1", "T", "I(T*X3)"), var_test = "T")
      }

sapply(test, function(x) sum(x)/1000)


results <- 
  lapply(c(100, 200, 500, 1000), 
    function(N){
      foreach(i=1:5, .combine="rbind")%dopar%{
        sim_boot(sample_size=N, x_var = c("X1", "T", "I(T*X3)"), var_test = "T")
      }
    }
  )


results_all <- rbindlist(results)



```