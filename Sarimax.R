

#############
## SARIMAX ##
#############

library(tseries)
library(forecast)
library(ModelMetrics)
library(lmtest)

setwd('~/classes/msds604/project')

bank <- read.csv(file = 'train.csv')
head(bank,5)
nrow(bank) 

# Split into train data and test data
train <- bank[1:312,]
test <- bank[313:336,]

unemp.ts <- ts(train$Unemployment_Rate, start = c(1987, 1), end = c(2012, 12), frequency = 12)
pop.ts <- ts(train$Population, start = c(1987, 1), end = c(2012, 12), frequency = 12)
bank.ts <- ts(train$Bankruptcy_Rate, start = c(1987, 1), end = c(2012, 12), frequency = 12)
hpi.ts <- ts(train$House_Price_Index, start = c(1987, 1), end = c(2012, 12), frequency = 12)

bank.test.ts <- ts(test$Bankruptcy_Rate, start = c(2013,1), end = c(2014,12), frequency = 12)

# boxcox transformation
lambda.1 <- BoxCox.lambda(bank.ts)
bank.trans <- BoxCox(bank.ts, lambda = lambda.1)
unemp.trans <- BoxCox(unemp.ts, lambda = lambda.1)

# Fit model for unemployment
for (q in 1:3){
  for (p in 1:2){
    for (P in 1:3){
      for (Q in 2:4){
        tryCatch({ 
          sarima.model <- arima(bank.trans, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), period = 12),
                                xreg = data.frame(unemp = BoxCox(unemp.ts, lambda = lambda.1)))
          pred <- forecast(object = sarima.model, h = 72, level = 0.95, 
                           lambda = lambda.1, biasadj = TRUE,
                           xreg = data.frame(BoxCox(bank$Unemployment_Rate[313:336],lambda = lambda.1)))$mean
          # rm <- rmse(bank.test, pred)
          acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
          print(paste(p, q, P, Q, acc[2]))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
      }
    }
  }
}

# Fit model for population
for (q in 1:3){
  for (p in 1:2){
    for (P in 1:3){
      for (Q in 2:4){
        tryCatch({ 
          sarima.model <- arima(bank.trans, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), period = 12),
                                xreg = data.frame(unemp = BoxCox(pop.ts, lambda = lambda.1)))
          pred <- forecast(object = sarima.model, h = 72, level = 0.95, 
                           lambda = lambda.1, biasadj = TRUE,
                           xreg = data.frame(BoxCox(bank$Population[313:336],lambda = lambda.1)))$mean
          # rm <- rmse(bank.test, pred)
          acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
          print(paste(p, q, P, Q, acc[2]))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
      }
    }
  }
}

# Fit model for hpi
for (q in 1:3){
  for (p in 1:2){
    for (P in 1:3){
      for (Q in 2:4){
        tryCatch({ 
          sarima.model <- arima(bank.trans, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), period = 12),
                                xreg = data.frame(unemp = BoxCox(hpi.ts, lambda = lambda.1)))
          pred <- forecast(object = sarima.model, h = 72, level = 0.95, 
                           lambda = lambda.1, biasadj = TRUE,
                           xreg = data.frame(BoxCox(bank$House_Price_Index[313:336],lambda = lambda.1)))$mean
          # rm <- rmse(bank.test, pred)
          acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
          print(paste(p, q, P, Q, acc[2]))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
      }
    }
  }
}

########################################################################################
# 01 unemployment and Population
for (q in 1:3){
  for (p in 1:2){
    for (P in 1:3){
      for (Q in 2:4){
        tryCatch({ 
          sarima.model <- arima(bank.trans, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), period = 12),
                                xreg = data.frame(unemp = BoxCox(unemp.ts, lambda = lambda.1),
                                                  pop = BoxCox(pop.ts, lambda = lambda.1)))
          pred <- forecast(object = sarima.model, h = 72, level = 0.95, 
                           lambda = lambda.1, biasadj = TRUE,
                           xreg = data.frame(BoxCox(bank$Unemployment_Rate[313:336], lambda = lambda.1),
                                             BoxCox(bank$Population[313:336],lambda = lambda.1)))$mean
          # rm <- rmse(bank.test, pred)
          acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
          print(paste(p, q, P, Q, acc[2]))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
      }
    }
  }
}

# 02 unemployment and hpi
for (q in 1:3){
  for (p in 1:2){
    for (P in 1:3){
      for (Q in 2:4){
        tryCatch({ 
          sarima.model <- arima(bank.trans, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), period = 12),
                                xreg = data.frame(unemp = BoxCox(unemp.ts, lambda = lambda.1),
                                                  hpi = BoxCox(hpi.ts, lambda = lambda.1)))
          pred <- forecast(object = sarima.model, h = 72, level = 0.95, 
                           lambda = lambda.1, biasadj = TRUE,
                           xreg = data.frame(BoxCox(bank$Unemployment_Rate[313:336], lambda = lambda.1),
                                             BoxCox(bank$House_Price_Index[313:336],lambda = lambda.1)))$mean
          # rm <- rmse(bank.test, pred)
          acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
          print(paste(p, q, P, Q, acc[2]))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
      }
    }
  }
}

# 03 Population and hpi
for (q in 1:3){
  for (p in 1:2){
    for (P in 1:3){
      for (Q in 2:4){
        tryCatch({ 
          sarima.model <- arima(bank.trans, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), period = 12),
                                xreg = data.frame(hpi = BoxCox(hpi.ts, lambda = lambda.1),
                                                  pop = BoxCox(pop.ts, lambda = lambda.1)))
          pred <- forecast(object = sarima.model, h = 24, level = 0.95, 
                           lambda = lambda.1, biasadj = TRUE,
                           xreg = data.frame(BoxCox(bank$House_Price_Index[313:336], lambda = lambda.1),
                                             BoxCox(bank$Population[313:336],lambda = lambda.1)))$mean
          # rm <- rmse(bank.test, pred)
          acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
          print(paste(p, q, P, Q, acc[2]))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
      }
    }
  }
}

########################################################################################
# with three exogenous variables
for (q in 1:3){
  for (p in 1:2){
    for (P in 1:3){
      for (Q in 2:4){
        tryCatch({ 
          sarima.model <- arima(bank.trans, order = c(p, 1, q), 
                                seasonal = list(order = c(P, 1, Q), period = 12),
                                xreg = data.frame(unemp = BoxCox(unemp.ts, lambda = lambda.1),
                                                  pop = BoxCox(pop.ts, lambda = lambda.1),
                                                  hpi = BoxCox(hpi.ts, lambda = lambda.1)))
          pred <- forecast(object = sarima.model, h = 24, level = 0.95, 
                           lambda = lambda.1, biasadj = TRUE,
                           xreg = data.frame(BoxCox(bank$Unemployment_Rate[313:336], lambda = lambda.1),
                                             BoxCox(bank$Population[313:336],lambda = lambda.1),
                                             BoxCox(bank$House_Price_Index[313:336], lambda = lambda.1)))$mean
          # rm <- rmse(bank.test, pred)
          acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
          print(paste(p, q, P, Q, acc[2]))},
          error = function(e){
            cat("Error :", conditionMessage(e), "\n")
          })
      }
    }
  }
}

sarima.model <- arima(bank.trans, order = c(1, 1, 2), 
                      seasonal = list(order = c(3, 1, 3), period = 12),
                      xreg = data.frame(unemp = BoxCox(unemp.ts, lambda = lambda.1),
                                        pop = BoxCox(pop.ts, lambda = lambda.1),
                                        hpi = BoxCox(hpi.ts, lambda = lambda.1)))

new.test <- read.csv("test.csv")
unemp.ts <- ts(new.test$Unemployment_Rate, start = c(2015, 1), frequency = 12)
pop.ts <- ts(new.test$Population, start = c(2015, 1), frequency = 12)
# bank.ts <- ts(new.test$Bankruptcy_Rate, start = c(2015, 1), frequency = 12)
hpi.ts <- ts(new.test$House_Price_Index, start = c(2015, 1), frequency = 12)

unemp.log <- BoxCox(unemp.ts, lambda = lambda.1)
hpi.log <- BoxCox(hpi.ts, lambda = lambda.1)
# bank.log <- BoxCox(bank.ts, lambda = lambda.1)
pop.log <- BoxCox(pop.ts, lambda = lambda.1)
# boxcox transformation
lambda.1 <- BoxCox.lambda(bank.ts)
bank.trans <- BoxCox(bank.ts, lambda = lambda.1)
unemp.trans <- BoxCox(unemp.ts, lambda = lambda.1)

pred.test <- forecast(object = sarima.model, h = 60, level = 0.95, 
                 lambda = lambda.1, biasadj = TRUE,
                 xreg = data.frame(unemp.log,pop.log,hpi.log))$mean


pred <- forecast(object = sarima.model, h = 24, level = 0.95, 
                 lambda = lambda.1, biasadj = TRUE,
                 xreg = data.frame(BoxCox(bank$Unemployment_Rate[313:336], lambda = lambda.1),
                                   BoxCox(bank$Population[313:336],lambda = lambda.1),
                                   BoxCox(bank$House_Price_Index[313:336], lambda = lambda.1)))$mean

acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
acc
sqrt(mean((pred-bank.test.ts)^2))
rmse(pred, bank.test.ts)
#####

acc <- accuracy(pred, bank.test.ts, d = 1, D = 1)
acc
sqrt(mean((pred-bank.test.ts)^2))

rmse(pred, bank.test.ts)


