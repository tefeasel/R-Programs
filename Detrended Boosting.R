library(dplyr)
library(ggplot2)
library(haven)
library(ggalt)
library(caret)
library(forecast)

#Time series model to separate trend
ts <- ts(res2$use, freq = 12)
decompose_ts <- stl(ts, s.window = 13, robust = TRUE)$time.series
autoplot(decompose_ts)

trend <- ts(decompose_ts[,2])

#Extract Xregs
xreg <- as.matrix(res2[,5:7])
fit_trend <- auto.arima(trend, xreg = xreg)
fit_trend
fcst_trend <- as.vector(forecast(fit_trend, 23, xreg = xreg_fcst2[,1:3])$mean)

detrended <- rowSums(decompose_ts[, c(1,3)])

train <- data.frame(detrended, res2[,8:23])

#Prepare test data
test <- data.frame(xreg_fcst2[,4:19])

#Models
grid <- expand.grid(ntree = 1000, mtry = c(2, 3, 4))
set.seed(1)
rf <- train(detrended ~ .,
            data = train,
            method = "rf",
            tuneLength = 3,
            trConrol = "none",
            importance = TRUE)
rf

set.seed(1)
xgbGrid <- expand.grid(eta = .1, max_depth = 2, colsample_bytree = .7, subsample = .5, nrounds = 7800,
                       gamma = .25, min_child_weight = 1)
xgb <- train(detrended ~ .,
             data = train,
             method = "xgbTree",
             trControl = trainControl(method = "cv", number = 5),
             tuneGrid = xgbGrid, verbose = TRUE)
xgb$bestTune
getTrainPerf(xgb)

tuneplot <- function(x, probs = .90) {
        ggplot(x) +
                coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
                theme_bw()
}

tuneplot(xgb)


plot(varImp(xgb))

#Get predictions
test <- as.matrix(test)
predict <- data.frame(pred = (predict(xgb, test) + fcst_trend))
predict$date <- seq(as.Date("2017-02-01"), by = "month", length.out = 23)
