#This is a set of functions which relate to multiple linear regression.
#ols.multi()is a function for ordinary least squars method.
#Inputs of ols.multi(): a vector of dependent varible and a matrix of dependent variables.
#Output of ols.multi(): a list containing coeffients(beta),predicted value of the dependent(y-hat), residuals,
#                        estimated variance of residuals, variance-covariance matrix of beta, R squared, adjusted 
#                        R squared, t-statistic, F-statistic, Durbin-Watson statistic, Q-statistic, Q'-statistic.
#reg_backward() is a function that performs the backward stepwise regression.
#Arguments in some functions contains "olslist", which indicates that this argument should be a list that conforms with
#the format of the output of the OLS.multi() function.
#Author: Junyuan Ma
ols.multi <- function(dep, indep) {
	# Beta
	beta <- solve(t(indep) %*% indep) %*% t(indep) %*% dep
	# Dimensions
	nreg <- ncol(indep)
	nobs <- length(dep)
	# y-hat
	dep_hat <- indep %*% beta
	# Residual
	res <- dep - dep_hat
	# R Squared
	ESS <- t(dep_hat - mean(dep)) %*% (dep_hat - mean(dep))
	RSS <- t(res) %*% res
	TSS <- t(dep - mean(dep)) %*% (dep - mean(dep))
	Rsqr <- ESS / TSS
	adj_Rsqr <- 1 - (nobs - 1) * (1 - Rsqr) / (nobs - nreg)
	# Estimatin of variance of residuals
	s_hat <- as.numeric((t(res) %*% res) / (nobs - nreg))
	var_cov_beta <- s_hat * solve(t(indep) %*% indep)
	# t-statistic
	t_test <- beta / (sqrt(diag(var_cov_beta)))
	# F-statistic
	F_test <- (ESS / (nreg - 1)) / (RSS / (nobs - nreg))
	# Durbin-Watson
	DW <- 0
	# Box-Price's Q & Ljung-Box's Q'
	for (i in 2:nobs) {
		DW <- DW + (res[i] - res[i - 1]) ** 2 / t(res) %*% res
	}
	Q_sta <- 0
	Q_pri <- 0
	for (j in 1:(nobs - 1)) {
		rho <- 0
		for (i in (j + 1):nobs) {
			rho <- rho + res[i] * res[i - j] / (t(res) %*% res)
		}
		Q_sta <- Q_sta + nobs * rho ** 2
		Q_pri <- Q_pri+nobs*(nobs-2)*rho/(nobs-j)
}
	# Return results in a list
	return(list(beta = beta, predicted_dependent = dep_hat, residual = res, variance_of_residuals = s_hat,
						variance_covariance_of_beta = var_cov_beta, R_squared = Rsqr, adjusted_R_squred = adj_Rsqr,
						t_statistic = t_test, F_statistic = F_test, Durbin_Watson = DW, Q_statistic=Q_sta, Q_prime=Q_pri))
}
#Backward stepwise regression
reg_backward <- function(dep, indep, sig = .1) {
	continue <- TRUE
	repeat {
		result <- ols.multi(dep, indep)
		p_t <- 2 * pt(-1 * abs(result$t_statistic), length(dep) - dim(indep)[2])
		ttest <- p_t > sig
		continue <- as.logical(sum(ttest))
		if (continue != TRUE) {
			break
		} else { indep <- indep[, -1 * which.max(p_t)] }
		}
	return(result)
}
# Generate the plot of residuals
resplot <- function(olslistname) {
	return(plot(olslistname$residual, type = "l", col = "blue",xlab="Observations",ylab="Residuals"))
}

# Breusch-Pagan test
bptest <- function(olslistname,dep,indep) {
	return(ols.multi(olslistname$residual ** 2, indep)$R_squared * length(dep))
}

# White test using F-statistic
whitetest <- function(olslistname,indep){
	regressor_white <- cbind(indep, indep[, 2:dim(indep)[2]] ** 2)
	for (i in 2:(dim(indep)[2] - 1)) {
		for (j in (i + 1):dim(indep)[2]) {
			regressor_white <- cbind(regressor_white, indep[, i] * indep[, j])
		}
	}
	colnames(regressor_white) <- 1:(dim(regressor_white)[2])
	return(ols.multi(olslistname$residual ** 2, indep)$F_statistic)
}

	# Jarque-Bera Test
jbtest <- function(olslistname, dep) {
	skew_monthly <- sum(olslistname$residual - mean(olslistname$residual)) ** 3 / length(dep) / ((sum(olslistname$residual - mean(olslistname$residual)) ** 2 / length(dep)) ** 1.5)
	kurt_monthly <- sum(olslistname$residual - mean(olslistname$residual)) ** 4 / length(dep) / ((sum(olslistname$residual - mean(olslistname$residual)) ** 2 / length(dep)) ** 2)
	return(length(NoDur_ex_monthly) * (skew_monthly ** 2 / 6 + (kurt_monthly - 3) ** 2 / 24))
}