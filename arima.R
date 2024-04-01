# import packages
library(forecast)
library(tseries)

# import data
data <- read.csv('Data_Chocolate_1.csv')
attach(data)

# sort data by Time
data <- data[order(data$Time), ]


##########################
# UK
##########################

country <- "UK"

data_c <- subset(data, Region==country, select=c('Time', 'Sales..units.'))
country_all <- ts(data_c$Sales..units,frequency=12,start=c(2010,1))
plot(country_all, main=country, ylab="sales")
# plot(log(country_all))

# remove data before 2012
country_3y <- ts(data_c$Sales..units[25:60],frequency=12,start=c(2012,1))
plot(country_3y, main=country, ylab="sales")

# keep 3 months as testing
data_train <- window(country_3y, start=c(2012,1), end=c(2014,9))
data_test <- window(country_3y, start=c(2014,10), end=c(2014,12))

# (1) stationary
# ADF test
aTSA::adf.test(data_train)

# KPSS test
kpss_test_result <- kpss.test(data_train)
print(kpss_test_result)

# (2) ACF/PACF test
sales <- data_train
acf(sales, main=country)
pacf(sales, main=country)

# (3) AR model
arima_model <- auto.arima(sales)
summary(arima_model)
# ARIMA(1,0,0)

# (4) fit ARIMA model
arimafit <- arima(x = sales, order = c(1,0,0), include.mean = FALSE)

# (5) Residual check
residuals <- resid(arimafit)

# ACF & PACF 
acf(residuals, lag.max=20, main=country)
pacf(residuals, lag.max=20, main=country)

# Normality test
shapiro.test(residuals)

# Q-Q plot 
qqnorm(residuals, main=country)
qqline(residuals, main=country)

# homoscedasticity
plot(residuals, type='l', ylab='Residuals', xlab='Observation', main=country)

# standard deviation of standardized residuals
lines(sqrt(abs(residuals)), col='red')
legend('topright', legend=c('Residuals', 'sqrt(|Residuals|)'), col=c('black', 'red'), lty=1:1)

# histogram of residual
hist(residuals, main=country, xlab='Residuals', ylab='Frequency', col='lightblue', border='black')


# (6) predict
# predicted value for next 4 months
Ycast <- stats::predict(arimafit,n.ahead = 4)

# predicted value for training data
fitted_values <- fitted(arimafit)

# result table
time_index <- 25:61
sales_units <- c(country_3y, NA)
pred_values <- c(fitted_values, Ycast$pred)
table <- data.frame(time_index=time_index, sales_units=sales_units, pred_values=pred_values)

# plot results
plot.ts(data_train,xlim=c(2012,2015), xaxt="n", main=country, ylab="sales")
axis(1, at = 2012:2015, labels = 2012:2015)
points(data_test,type="l",lty=2)
points(Ycast$pred,col="blue",type = "l",pch=18)
legend("topleft", legend=c("training", 'testing', 'prediction'), col=c("black", "black", "blue"), lty=1:2)

# (7) model validation
# MAPE
MAPE <- mean(abs(data_test - Ycast$pred[1:3])/data_test) * 100
MAPE




##########################
# US
##########################

country <- "US"

data_c <- subset(data, Region==country, select=c('Time', 'Sales..units.'))
country_all <- ts(data_c$Sales..units,frequency=12,start=c(2010,1))
plot(country_all, main=country, ylab="sales")
# plot(log(country_all))

# remove data before 2012
country_3y <- ts(data_c$Sales..units[25:60],frequency=12,start=c(2012,1))
plot(country_3y, main=country, ylab="sales")

# keep 3 months as testing
data_train <- window(country_3y, start=c(2012,1), end=c(2014,9))
data_test <- window(country_3y, start=c(2014,10), end=c(2014,12))

# (1) stationary
# ADF test
aTSA::adf.test(data_train)

# KPSS test
kpss_test_result <- kpss.test(data_train)
print(kpss_test_result)

# (2) ACF/PACF test
sales <- data_train
acf(sales, main=country)
pacf(sales, main=country)

# (3) AR model
arima_model <- auto.arima(sales)
summary(arima_model)
# ARIMA(1,0,0)

# (4) fit ARIMA model
arimafit <- arima(x = sales, order = c(1,0,0), include.mean = FALSE)

# (5) Residual check
residuals <- resid(arimafit)

# ACF & PACF 
acf(residuals, lag.max=20, main=country)
pacf(residuals, lag.max=20, main=country)

# Normality test
shapiro.test(residuals)

# Q-Q plot 
qqnorm(residuals, main=country)
qqline(residuals, main=country)

# homoscedasticity
plot(residuals, type='l', ylab='Residuals', xlab='Observation', main=country)

# standard deviation of standardized residuals
lines(sqrt(abs(residuals)), col='red')
legend('topright', legend=c('Residuals', 'sqrt(|Residuals|)'), col=c('black', 'red'), lty=1:1)

# histogram of residual
hist(residuals, main=country, xlab='Residuals', ylab='Frequency', col='lightblue', border='black')

# (6) predict
# predicted value for next 4 months
Ycast <- stats::predict(arimafit,n.ahead = 4)

# predicted value for training data
fitted_values <- fitted(arimafit)

# plot results
plot.ts(data_train,xlim=c(2012,2015), xaxt="n", main=country, ylab="sales")
axis(1, at = 2012:2015, labels = 2012:2015)
points(data_test,type="l",lty=2)
points(Ycast$pred,col="blue",type = "l",pch=18)
legend("topleft", legend=c("training", 'testing', 'prediction'), col=c("black", "black", "blue"), lty=1:2)

# MAPE
MAPE <- mean(abs(data_test - Ycast$pred[1:3])/data_test) * 100
MAPE

# result table
time_index <- 25:61
sales_units <- c(country_3y, NA)
pred_values <- c(fitted_values, Ycast$pred)
table <- data.frame(time_index=time_index, sales_units=sales_units, pred_values=pred_values)




##########################
# Spain
##########################

country <- "Spain"

data_c <- subset(data, Region==country, select=c('Time', 'Sales..units.'))
country_all <- ts(data_c$Sales..units,frequency=12,start=c(2010,1))
plot(country_all, main=country, ylab="sales")
# plot(log(country_all))

# remove data before 2012
country_3y <- ts(data_c$Sales..units[25:60],frequency=12,start=c(2012,1))
plot(country_3y, main=country, ylab="sales")

# keep 3 months as testing
data_train <- window(country_3y, start=c(2012,1), end=c(2014,9))
data_test <- window(country_3y, start=c(2014,10), end=c(2014,12))

# (1) stationary
# ADF test
aTSA::adf.test(data_train)

# KPSS test
kpss_test_result <- kpss.test(data_train)
print(kpss_test_result)

# (2) ACF/PACF test
sales <- data_train
acf(sales, main=country)
pacf(sales, main=country)

# (3) AR model
arima_model <- auto.arima(sales)
summary(arima_model)
# ARIMA(1,0,0)

# (4) fit ARIMA model
arimafit <- arima(x = sales, order = c(1,0,0), include.mean = FALSE)

# (5) Residual check
residuals <- resid(arimafit)

# ACF & PACF 
acf(residuals, lag.max=20, main=country)
pacf(residuals, lag.max=20, main=country)

# Normality test
shapiro.test(residuals)

# Q-Q plot 
qqnorm(residuals, main=country)
qqline(residuals, main=country)

# homoscedasticity
plot(residuals, type='l', ylab='Residuals', xlab='Observation', main=country)

# standard deviation of standardized residuals
lines(sqrt(abs(residuals)), col='red')
legend('topright', legend=c('Residuals', 'sqrt(|Residuals|)'), col=c('black', 'red'), lty=1:1)

# histogram of residual
hist(residuals, main=country, xlab='Residuals', ylab='Frequency', col='lightblue', border='black')

# (6) predict
# predicted value for next 4 months
Ycast <- stats::predict(arimafit,n.ahead = 4)

# predicted value for training data
fitted_values <- fitted(arimafit)

# plot results
plot.ts(data_train,xlim=c(2012,2015), xaxt="n", main=country, ylab="sales")
axis(1, at = 2012:2015, labels = 2012:2015)
points(data_test,type="l",lty=2)
points(Ycast$pred,col="blue",type = "l",pch=18)
legend("topleft", legend=c("training", 'testing', 'prediction'), col=c("black", "black", "blue"), lty=1:2)

# MAPE
MAPE <- mean(abs(data_test - Ycast$pred[1:3])/data_test) * 100
MAPE

# result table
time_index <- 25:61
sales_units <- c(country_3y, NA)
pred_values <- c(fitted_values, Ycast$pred)
table <- data.frame(time_index=time_index, sales_units=sales_units, pred_values=pred_values)




##########################
# SG
##########################

country <- "Singapore"

data_c <- subset(data, Region==country, select=c('Time', 'Sales..units.'))
country_all <- ts(data_c$Sales..units,frequency=12,start=c(2010,1))
plot(country_all, main=country, ylab="sales")
# plot(log(country_all))

# remove data before 2012
country_3y <- ts(data_c$Sales..units[25:60],frequency=12,start=c(2012,1))
plot(country_3y, main=country, ylab="sales")

# keep 3 months as testing
data_train <- window(country_3y, start=c(2012,1), end=c(2014,9))
data_test <- window(country_3y, start=c(2014,10), end=c(2014,12))

# (1) stationary
# ADF test
aTSA::adf.test(data_train)

# KPSS test
kpss_test_result <- kpss.test(data_train)
print(kpss_test_result)

# (2) ACF/PACF test
sales <- data_train
acf(sales, main=country)
pacf(sales, main=country)

# (3) AR model
arima_model <- auto.arima(sales)
summary(arima_model)
# ARIMA(1,0,0)

# (4) fit ARIMA model
arimafit <- arima(x = sales, order = c(1,0,0), include.mean = FALSE)

# (5) Residual check
residuals <- resid(arimafit)

# ACF & PACF 
acf(residuals, lag.max=20, main=country)
pacf(residuals, lag.max=20, main=country)

# Normality test
shapiro.test(residuals)

# Q-Q plot 
qqnorm(residuals, main=country)
qqline(residuals, main=country)

# homoscedasticity
plot(residuals, type='l', ylab='Residuals', xlab='Observation', main=country)

# standard deviation of standardized residuals
lines(sqrt(abs(residuals)), col='red')
legend('topright', legend=c('Residuals', 'sqrt(|Residuals|)'), col=c('black', 'red'), lty=1:1)

# histogram of residual
hist(residuals, main=country, xlab='Residuals', ylab='Frequency', col='lightblue', border='black')

# (6) predict
# predicted value for next 4 months
Ycast <- stats::predict(arimafit,n.ahead = 4)

# predicted value for training data
fitted_values <- fitted(arimafit)

# plot results
plot.ts(data_train,xlim=c(2012,2015), xaxt="n", main=country, ylab="sales")
axis(1, at = 2012:2015, labels = 2012:2015)
points(data_test,type="l",lty=2)
points(Ycast$pred,col="blue",type = "l",pch=18)
legend("topleft", legend=c("training", 'testing', 'prediction'), col=c("black", "black", "blue"), lty=1:2)

# MAPE
MAPE <- mean(abs(data_test - Ycast$pred[1:3])/data_test) * 100
MAPE

# result table
time_index <- 25:61
sales_units <- c(country_3y, NA)
pred_values <- c(fitted_values, Ycast$pred)
table <- data.frame(time_index=time_index, sales_units=sales_units, pred_values=pred_values)





##########################
# Mexico
##########################

country <- "Mexico"

data_c <- subset(data, Region==country, select=c('Time', 'Sales..units.'))
country_all <- ts(data_c$Sales..units,frequency=12,start=c(2010,1))
plot(country_all, main=country, ylab="sales")
# plot(log(country_all))

# remove data before 2012
country_3y <- ts(data_c$Sales..units[25:60],frequency=12,start=c(2012,1))
plot(country_3y, main=country, ylab="sales")

# keep 3 months as testing
data_train <- window(country_3y, start=c(2012,1), end=c(2014,9))
data_test <- window(country_3y, start=c(2014,10), end=c(2014,12))

# (1) stationary
# ADF test
aTSA::adf.test(data_train)

# KPSS test
kpss_test_result <- kpss.test(data_train)
print(kpss_test_result)

# (2) ACF/PACF test
sales <- data_train
acf(sales, main=country)
pacf(sales, main=country)

# (3) AR model
arima_model <- auto.arima(sales)
summary(arima_model)
# ARIMA(1,0,0)

# (4) fit ARIMA model
arimafit <- arima(x = sales, order = c(1,0,0), include.mean = FALSE)

# (5) Residual check
residuals <- resid(arimafit)

# ACF & PACF 
acf(residuals, lag.max=20, main=country)
pacf(residuals, lag.max=20, main=country)

# Normality test
shapiro.test(residuals)

# Q-Q plot 
qqnorm(residuals, main=country)
qqline(residuals, main=country)

# homoscedasticity
plot(residuals, type='l', ylab='Residuals', xlab='Observation', main=country)

# standard deviation of standardized residuals
lines(sqrt(abs(residuals)), col='red')
legend('topright', legend=c('Residuals', 'sqrt(|Residuals|)'), col=c('black', 'red'), lty=1:1)

# histogram of residual
hist(residuals, main=country, xlab='Residuals', ylab='Frequency', col='lightblue', border='black')

# (6) predict
# predicted value for next 4 months
Ycast <- stats::predict(arimafit,n.ahead = 4)

# predicted value for training data
fitted_values <- fitted(arimafit)

# plot results
plot.ts(data_train,xlim=c(2012,2015), xaxt="n", main=country, ylab="sales")
axis(1, at = 2012:2015, labels = 2012:2015)
points(data_test,type="l",lty=2)
points(Ycast$pred,col="blue",type = "l",pch=18)
legend("topleft", legend=c("training", 'testing', 'prediction'), col=c("black", "black", "blue"), lty=1:2)

# MAPE
MAPE <- mean(abs(data_test - Ycast$pred[1:3])/data_test) * 100
MAPE

# result table
time_index <- 25:61
sales_units <- c(country_3y, NA)
pred_values <- c(fitted_values, Ycast$pred)
table <- data.frame(time_index=time_index, sales_units=sales_units, pred_values=pred_values)