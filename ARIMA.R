library(lubridate)
library(dplyr)
library(haven)
library(forecast)

setwd("")

data <- read_sas("")
rvr <- read_sas("")

rvr <- subset(rvr, JURIS == "APV" & REVCLS == 1)

data <- data %>%
          filter(JURIS == "" & revcls == 1 & YEAR >= 2011) %>%
          select(YEAR, MONTH, bcdd65, bhdd55, KWH, CUST, DAYS) %>%
          mutate(use = KWH/CUST)

features <- data[,c("DAYS")]
auto <- auto.arima(data$use, xreg = features )
auto
predict <- forecast(auto, xreg = features)
predicted <- predict$mean
predicted <- data.frame(predicted)
