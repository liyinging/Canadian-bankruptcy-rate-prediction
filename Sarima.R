library(tseries)
library(forecast)
library(ModelMetrics)
library(lmtest)

setwd('~/classes/msds604/project')

# https://github.com/zzhangusf/Time-Series-Analysis-of-Canadian-National-Bankruptcy-Rates
# https://github.com/jaimeps/time-series-bankruptcy
# https://github.com/qianmx/TimeSeries-Canadian-National-Bankruptcy-Rate
# https://github.com/cmchu/canadian-bankrupty-rates
# https://github.com/chenxi-ge/Bankruptcy-Time-Series
# https://github.com/tracyliu233/Bankruptcy-Rate-Prediction
# https://github.com/beimingliu/Time_Series_Prediction_Canadian_Bankruptcy_Rate
# https://github.com/aprilyichenwang/predicting_Canadian_bankruptcy_rate/blob/master/TimeSeries-FinalProject.pdf
# https://github.com/fchamma/canadian_bankruptcy
# https://github.com/YiQ-Zhao/Canadian-Bankruptcy-Time-Series

# https://ernestk-git.github.io

# https://lqian5.github.io/Portfolio/
# https://lqian5.github.io/Portfolio/documents/time_series_report.pdf

# Canadian Bankruptcy Rate Prediction: R, Box-Jenkins, SARIMAX, Holt-Winters, VARX

# ensemble learning: https://petolau.github.io/Ensemble-of-trees-for-forecasting-time-series/
# https://otexts.org/fpp2/VAR.html

bank <- read.csv(file = 'train.csv')
head(bank,5)
nrow(bank) # 336: from 1987.1 to 2014.12, including 27 years

# Split into train data and test data
train <- bank[1:312,]
test <- bank[313:336,]

##################################################################################
######################### FIT MODEL WITH SINGLE VARIABLE #########################
################################## SARIMA MODEL ##################################
##################################################################################
# train.ts <- ts(train, start = c(1987, 1), end = c(2012, 12), frequency = 12)
# head(train.ts, 5)
# unemp.ts <- ts(train$Unemployment_Rate, start = c(1987, 1), end = c(2012, 12), frequency = 12)
# pop.ts <- ts(train$Population, start = c(1987, 1), end = c(2012, 12), frequency = 12)
bank.ts <- ts(train$Bankruptcy_Rate, start = c(1987, 1), end = c(2012, 12), frequency = 12)
# hpi.ts <- ts(train$House_Price_Index, start = c(1987, 1), end = c(2012, 12), frequency = 12)
bank.test.ts <- ts(test$Bankruptcy_Rate, start = c(2013,1), end = c(2014,12), frequency = 12)
plot(bank.ts)

# boxcox transformation
lambda.1 <- BoxCox.lambda(bank.ts)
bank.trans <- BoxCox(bank.ts, lambda = lambda.1)
plot(bank.trans)
# transformation of test data
bank.test.trans <- BoxCox(bank.test.ts, lambda.1)

# check for trend&seasonality
adf.test(bank.trans, k = 1) # p < 0.01, indicating stationary?
ndiffs(bank.trans) # 1
bank.ord.diff <- diff(bank.trans, lag = 1)
adf.test(bank.ord.diff) # p < 0.01, indicating stationary

nsdiffs(bank.trans) # 1
adf.test(bank.ord.diff, k = 12) # p = 0.02806, indicating seasonality, needs seasonally differenced
bank.sea.diff <- diff(bank.ord.diff, lag = 12)
adf.test(bank.sea.diff) # p <= 0.01, indicating stationary

# Determine p, q, P, Q
acf(bank.sea.diff, lag.max = 72) # q <= 4; Q =1
pacf(bank.sea.diff, lag.max = 72) # p <= 4; P <= 4
# Fit model
for (q in 1:3){
  for (p in 1:3){
    for (P in 1:2){
      for (Q in 1:3){
        tryCatch({ 
          sarima.model <- arima(bank.trans, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), period = 12))
          pred <- forecast(object = sarima.model, h = 72, level = 0.95, 
                           lambda = lambda.1, biasadj = TRUE)$mean
          # rm <- rmse(bank.test, pred)
          acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
          print(paste(p, q, P, Q, sarima.model$loglik, sarima.model$aic, 
                      sarima.model$sigma2, acc[2]))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
        }
    }
  }
}


for (q in 1:3){
  for (p in 1:3){
    for (P in 1:2){
      for (Q in 3:6){
        tryCatch({ 
          sarima.model <- arima(bank.trans, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), period = 12))
          pred <- forecast(object = sarima.model, h = 72, level = 0.95, 
                           lambda = lambda.1, biasadj = TRUE)$mean
          # rm <- rmse(bank.test, pred)
          acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
          print(paste(p, q, P, Q, sarima.model$loglik, sarima.model$aic, 
                      sarima.model$sigma2, acc[2]))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
      }
    }
  }
}

# fit p=2, q=1, P=2, Q=3
sarima.model.1 <- arima(bank.trans, order = c(2, 1, 1), 
                        seasonal = list(order = c(2, 1, 3), period = 12))
pred.1 <- forecast(sarima.model.1, bank.test.trans, h = 24, level = 0.95,
                   lambda = lambda.1, biasadj = TRUE)
acc.1 <- accuracy(pred.1, bank.test.ts, d = 1, D = 1)
acc.1 # 0.2687662

# auto arima
sarima.auto.model.1 <- auto.arima(bank.trans, max.d = 1, max.D = 1, 
                                  max.p = 6, max.P = 6, max.q = 6, max.Q = 6)
sarima.auto.model.1

pred.auto.1 <- forecast(sarima.auto.model.1, bank.test.ts, h = 24, level = 0.95, 
                        lambda = lambda.1, biasadj = TRUE)
acc.auto.1 <- accuracy(pred.auto.1, bank.test.ts, d =0, D = 1)
acc.auto.1 # 0.30087100

##########################################
# Fit model
for (q in 1:3){
  for (p in 1:3){
    for (P in 1:2){
      for (Q in 1:3){
        tryCatch({ 
          sarima.model <- Arima(bank.ts, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), frequency = 12), 
                                lambda = 'auto')
          pred <- forecast(object = sarima.model, h = 72, level = 0.95)$mean
          # rm <- rmse(bank.test, pred)
          acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
          print(paste(p, q, P, Q, sarima.model$loglik, sarima.model$aic, 
                      sarima.model$sigma2, acc[2]))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
      }
    }
  }
}




