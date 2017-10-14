rm(list = ls())
library(readxl)
library(tseries)
source(file = "Functions.R")
#Import portfolio data
ind_port_monthly <- read.csv(file = "12_industry_portfolios.CSV", skip = 11, nrow = 1094, header = TRUE)
ind_port_annual <- read.csv(file = "12_industry_portfolios.CSV", skip = 2207, nrow = 90, header = TRUE)

# Set column names for date column
colnames(ind_port_monthly)[1] <- "Date"
colnames(ind_port_annual)[1] <- "Date"

# Choose the return of the sector to study
nondurable_monthly <- subset(ind_port_monthly, Date > 197212 & Date <= 201112)[, "NoDur"]
nondurable_annual <- subset(ind_port_annual, Date > 1972 & Date <= 2011)[, "NoDur"]

# Import the risk-free rate
riskfree_monthly <- subset(read_excel("GW_predictor_data.xlsx", sheet = "Monthly"), yyyymm > 197212 & yyyymm <= 201112)[, "Rfree"]
riskfree_annual <- subset(read_excel("GW_predictor_data.xlsx", sheet = "Annual"), yyyy > 1972 & yyyy <= 2011)[, "Rfree"]

# Calculate excess return
NoDur_ex_monthly <- nondurable_monthly - riskfree_monthly * 100
NoDur_ex_annual <- nondurable_annual - riskfree_annual * 100

# Build the regressor matrix
regressor_monthly <- as.matrix(cbind(rep(1, length(NoDur_ex_monthly)), subset(read_excel("macro_Data.xlsx", sheet = "macrodata"), year > 1972)[, c("PI", "IP: nondble matls", "IP: fuels", "U: all", "Emp: Govt", "PMI", "Orders: cons gds", "M2 (real)", "Fed Funds", "S&P 500")],
	subset(read_excel("macro_Data.xlsx", sheet = "financialdata"), year > 1972)[, c("SMB", "HML", "Mkt-RF")]))
regressor_annual <- as.matrix(cbind(rep(1, length(NoDur_ex_monthly)),read_excel("annual.xlsx")[,2:14]))
colnames(regressor_monthly)[1] <- "Constant"
rownames(regressor_monthly) <- (1:dim(regressor_monthly)[1])
colnames(regressor_annual)[1] <- "Constant"
rownames(regressor_annual) <- (1:dim(regressor_monthly)[1])


# Current Model Monthly
current_monthly <- reg_backward(NoDur_ex_monthly, regressor_monthly)
resplot(current_monthly)
current_monthly_bp <- bptest(current_monthly, NoDur_ex_monthly, regressor_monthly)
current_monthly_white <- whitetest(current_monthly, regressor_monthly)
current_monthly_jb <- jbtest(current_monthly, NoDur_ex_monthly)
write.csv(list(current_monthly, current_monthly_bp, current_monthly_white, current_monthly_jb), file = "Current Monthly.csv")

# Current Model Annual
current_annual <- reg_backward(NoDur_ex_annual, regressor_annual)
resplot(current_annual)
current_annual_bp <- bptest(current_annual, NoDur_ex_annual, regressor_annual)
current_annual_white <- whitetest(current_annual, regressor_annual)
current_annual_jb <- jbtest(current_annual, NoDur_ex_annual)
write.csv(list(current_annual, current_annual_bp, current_annual_white, current_annual_jb), file = "Current Annual.csv")

# Predictive Model Monthly
pre_monthly <- reg_backward(NoDur_ex_monthly[2:length(NoDur_ex_monthly),], regressor_monthly[1:(dim(regressor_monthly)[1]-1),])
resplot(pre_monthly)
pre_monthly_bp <- bptest(pre_monthly, NoDur_ex_monthly[2:length(NoDur_ex_monthly),], regressor_monthly[1:(dim(regressor_monthly)[1] - 1),])
pre_monthly_white <- whitetest(pre_monthly, regressor_monthly[1:(dim(regressor_monthly)[1] - 1),])
pre_monthly_jb <- jbtest(pre_monthly, NoDur_ex_monthly[2:length(NoDur_ex_monthly),])
write.csv(list(pre_monthly, pre_monthly_bp, pre_monthly_white, pre_monthly_jb), file = "Predictive Monthly.csv")

# Predictive Model Annual
pre_annual <- reg_backward(NoDur_ex_annual[2:length(NoDur_ex_monthly),], regressor_annual[1:(dim(regressor_monthly)[1] - 1),])
resplot(pre_annual)
pre_annual_bp <- bptest(pre_annual, NoDur_ex_annual[2:length(NoDur_ex_monthly),], regressor_annual[1:(dim(regressor_monthly)[1] - 1),])
pre_annual_white <- whitetest(pre_annual, regressor_annual[1:(dim(regressor_monthly)[1] - 1),])
pre_annual_jb <- jbtest(pre_annual, NoDur_ex_annual[2:length(NoDur_ex_monthly),])
write.csv(list(pre_annual, pre_annual_bp, pre_annual_white, pre_annual_jb), file = "Predictive Annual.csv")

## Perform the regression
#continue <- TRUE
#sig <- 0.05
##result <- ols.multi(NoDur_ex_monthly,regressor_monthly)
#repeat{
	#result <- ols.multi(NoDur_ex_monthly, regressor_monthly)
	#p_t <- 2 * pt(-1 * abs(result$t_statistic), length(NoDur_ex_monthly) - dim(regressor_monthly)[2])
	#ttest <- p_t > sig
	#continue <- as.logical(sum(ttest))
	#if (continue != TRUE) {
		#break
	#} else { regressor_monthly <- regressor_monthly[, -1* which.max(p_t)] }
#}
#print(result)

## Generate the plot of residuals
#plot(result$residual, type = "l", col = "blue")

## Breusch-Pagan test
#bptest <- ols.multi(result$residual ** 2, regressor_monthly)$R_squared * length(NoDur_ex_monthly)
#print(bptest)

## White test
#regressor_white <- cbind(regressor_monthly, regressor_monthly[, 2:dim(regressor_monthly)[2]] ** 2)
#for (i in 2:(dim(regressor_monthly)[2] - 1)) {
	#for (j in (i + 1):dim(regressor_monthly)[2]) {
		#regressor_white <- cbind(regressor_white, regressor_monthly[,i] * regressor_monthly[,j])
	#}
#}
#colnames(regressor_white) <- 1:(dim(regressor_white)[2])
#whitetest_f <- ols.multi(result$residual ** 2, regressor_white)$F_statistic

## Jarque-Bera Test
#skew_monthly <- sum(result$residual - mean(result$residual)) ** 3 / length(NoDur_ex_monthly) / ((sum(result$residual - mean(result$residual))**2 / length(NoDur_ex_monthly)) ** 1.5)
#kurt_monthly <- sum(result$residual - mean(result$residual)) ** 4 / length(NoDur_ex_monthly) / ((sum(result$residual - mean(result$residual)) ** 2 / length(NoDur_ex_monthly)) ** 2)
#JB_monthly <- length(NoDur_ex_monthly)*(skew_monthly**2/6+(kurt_monthly-3)**2/24)