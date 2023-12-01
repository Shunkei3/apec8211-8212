
```{r, eval=T}
# school_ids_dummies <- names(reg_dt)[-c(1:4)]
# model <- as.formula(paste0("sdchi04s2~trttown+", paste(school_ids_dummies, collapse="+")))
# reg <- lm(model, data=reg_dt)
# se_cl <- sqrt(diag(vcovCL(reg, cluster= ~ townid, type = "HC3")))
# t_value_cl <- coef(reg)[["trttown"]]/se_cl[["trttown"]]

# === Unrestricted regression === #
reg <- lm(sdchi04s2 ~ trttown, data=reg_dt)
se_cl <- sqrt(diag(vcovCL(reg, cluster= ~ townid, type = "HC3")))
t_value_cl <- coef(reg)[["trttown"]]/se_cl[["trttown"]]


# === Random fixed effects === #
# library(lme4)
# reg <- lmer(sdchi04s2 ~ trttown + (1 | schoolid), data = reg_dt)
# coef_beta <- unique(coef(reg)$schoolid[,"trttown"])
# se_cl <- sqrt(diag(vcovCL(reg, cluster= ~ townid, type = "HC3")))

```

<br>

Next, regress $Y$ on $X_{-j}$ (i.e., imposing $H_0$) to get $\beta_{-j}$ and the residuals $\hat{e}_g$.

```{r, eval=T}
# === Restricted regression: Regress Y on X_{-j}   === #
model_restrict <- as.formula(paste0("sdchi04s2~", paste(school_ids_dummies, collapse="+")))
reg_restrict <- lm(model_restrict, data=reg_dt)

# save the residuals and cluster id (townid) for later
y_hat <- fitted(reg_restrict)
e_hat <- resid(reg_restrict)

reg_dt[,`:=`(
	y_hat = fitted(reg_restrict),
	e_hat = resid(reg_restrict)
	)]
```


```{r, eval=F}



g_size <- length(unique(reg_dt$townid))

B=10
t_wild_b <- 
	foreach{i=1:B, .combine="c"}%do%{
		# /*===== Step 1 =====*/
		z_b <- sample(c(-1,1), g_size, prob=c(0.5,0.5), replace=TRUE)

		z_dt <- 
			data.frame(
				townid=unique(reg_dt$townid),
				z_b=z_b
			)

		# --- Merge e_hat_dt and e_hat_b ---
		reg_dt_b <- left_join(reg_dt, z_dt, by="townid")

		# --- Multiply the residuals by Rademacher random variable  --- #
		# reg_dt_b <- reg_dt_b[, e_b := z_b*e_hat]
		e_b <- reg_dt_b$e_hat*reg_dt_b$z_b

		# /*===== Step 2 =====*/
		reg_dt_b <- reg_dt_b[, y_b := y_hat + e_b]
		
		# /*===== Step 3 =====*/
		model <- update(model, y_b ~ .) 
		reg_b <- lm(model, data=reg_dt_b)

		# /*===== Step 4 =====*/
		se_b <- sqrt(diag(vcovCL(reg_b, cluster= ~ schoolid, type = "HC1")))
		t_b <- coef(reg_b)[["trttown"]]/se_b[["trttown"]]
		t_b
	}


# get_p_wild_Boot <- function(data){
	# data=reg_dt
library(foreach)

B = 10




ls_t_values <- 
	foreach(i=1:B, .combine=c) %do%{
	# /*===== Step 1 =====*/
	# resample cluster ids with size G
	townid_b <- sample(unique(town_id), G, replace=TRUE)
	# Generate scalars (-1 or 1) with size G
	z_g <- sample(c(-1,1), G, prob=c(0.5,0.5), replace=TRUE)

	z_g_dt <- 
		data.table(
			townid = townid_b,
			z_g = z_g
		)
	# --- Merge e_hat_dt and e_hat_b ---
	reg_dt_b <- left_join(z_g_dt, reg_dt, by="townid", relationship = "many-to-many")
	# --- Multiply residuals by Rademacher random variable  --- #
	reg_dt_b$e_b <- reg_dt_b$z_g*reg_dt_b$e_hat

	# /*===== Step 2 =====*/
	reg_dt_b$y_b <- reg_dt_b$y_hat + reg_dt_b$e_b

	# /*===== Step 3 =====*/
	model <- update(model, y_b ~ .) 
	reg_b <- lm(model, data=reg_dt_b)

	# /*===== Step 4 =====*/
	se_b <- sqrt(diag(vcovCL(reg_b, cluster= ~ townid, type = "HC1")))
	t_b <- coef(reg_b)[["trttown"]]/se_b[["trttown"]]

	# return(t_b)
}


test <- get_p_wild_Boot(reg_dt)

```




<span style="color:blue">**Step 1**:</span>
$$\begin{equation}
\text {Create }\hat{e}^{b}_{g}=z_g \hat{e}_g, \text{ where }
D_{it} = 
\begin{cases}
      - 1 & \text{ with probability 1/2}\\
      1 & \text{ with probability 1/2}\\
\end{cases}    
\end{equation}$$

```{r, eval=T}
# cluster size
town_id <- reg_dt[,townid]
G <- length(unique(town_id))
# resample cluster ids with size G
townid_b <- sample(unique(town_id), G, replace=TRUE)
# Generate scalars (-1 or 1) with size G
z_g <- sample(c(-1,1), G, prob=c(0.5,0.5), replace=TRUE)

z_g_dt <- 
	data.table(
		townid = townid_b,
		z_g = z_g
		)

# --- Merge e_hat_dt and e_hat_b ---
# reg_dt_b <- merge(z_g_dt, reg_dt, by="townid")
reg_dt_b <- left_join(z_g_dt, reg_dt, by="townid", relationship = "many-to-many")

# --- Multiply residuals by Rademacher random variable  --- #
reg_dt_b[,e_b := z_g*e_hat]
```

<br>

<span style="color:blue">**Step 2:**</span>

Define $$\color{red}{Y^b}=X\hat{\beta}_{-j}+\hat{e}^{b}_{g} \quad \text{Bootstrap DGP imposing }H_0$$

```{r, eval=T}
reg_dt_b[,y_b := y_hat + e_b]
```

<br>

<span style="color:blue">**Step 3:**</span>

Estimate $$Y^{b}=X\beta+\varepsilon$$

```{r, eval=T}
model <- update(model, y_b ~ .) 
reg_b <- lm(model, data=reg_dt_b)
```

<br>

<span style="color:blue">**Step 4:**</span>

Calculate $$t^{b}=\hat{\beta}^{b}_{j}/SE^{b}_{\hat{\beta}_j}$$

```{r}
se_b <- sqrt(diag(vcovCL(reg_b, cluster= ~ townid, type = "HC1")))
t_b <- coef(reg_b)[["trttown"]]/se_b[["trttown"]]
```


### WCR Bootstrap (A single iteration for explanation purpose)

We repeat Step 1 ~ Step 4 many times. 







<br>

<span style="color:blue">**Step 4:**</span>

Calculate $t^{b}=$

## Explanation: Run the Wild bootstrap


What if we disregard clustered error and use the restricted Wild bootstrap?

