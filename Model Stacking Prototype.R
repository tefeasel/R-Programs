#Load Packages
packages <- c("plyr", "dplyr", "haven", "lubridate", "tidyr", "ggplot2", "caret", "corrplot", "e1071",
              "ModelMetrics", "xgboost")

install <- function(packages){ 
  for(i in packages) {
    if(!(i %in% installed.packages()[, "Package"])) {
      pack <- i
      install.packages(pack, dependencies = TRUE)
    }
    else 
      print(paste(i, "is already installed", sep = " "))
  }
}
  
#install(packages) #Run install function
lapply(packages, require, character.only = TRUE)

jur <- ""
jur2 <- ""
st <- ""
geo <- ""
yr <-  #Forecast start
lastact <- ""

reg <- read_sas("") %>%
       filter(state == st & juris == jur2) 
macro <- read_sas("") %>%
         dplyr::rename(YEAR = year, MONTH = month) %>%
         select(YEAR, MONTH, cpiu, defgdp, mfus, l_us, rgdp)
aep <- read_sas(paste("", sep = "")) %>%
        filter(JURIS == jur & REVCLS == 1) %>%
        select(YEAR, MONTH, CUST) 

setwd("")
budg <- read_sas(paste("_", geo, "_", sep = "")) %>%
  filter((JURIS == jur) & (YEAR >= yr) & (REVCLS == 1)) %>%
  select(YEAR, MONTH, bcust)

all <- inner_join(reg, macro, by = c("MONTH", "YEAR"))

sum(aep$CUST < 0)
plot(aep$YEAR, aep$CUST, type = "p")

all <- inner_join(all, aep, by = c("MONTH", "YEAR")) %>%
       mutate(cpiu = cpiu/100,
              defgdp = defgdp/100,
              FYPA = FYPA/defgdp,
              gdp_pc = FGDP_A/FPOPA,
              date = as.Date(date, origin = "1960-01-01"),
              MONTH = as.factor(MONTH),
              YEAR = as.factor(YEAR),
              UNEMP = FLBUA/FLBFA,
              CUST = ifelse(date == "2015-06-30", lag(CUST, n = 1L), CUST)) %>%
              select(-cpiu, -defgdp, -mfus) %>%
        filter(date >= "2005-01-31")

dummys <- dummyVars(~MONTH + YEAR, data = all)
dummys
all2 <- cbind.data.frame(all, as.data.frame(predict(dummys, all)))

#Check Drivers with Customers

ggplot(all2, aes(x = date, y = CUST)) +
        geom_line()

ggplot(all2, aes(x = date, y = FLBFA)) +
        geom_line()

#Try to reduce redundant variables from the Moody's data
cor_dep <- cor(all2$CUST, all2[c(8:77, 90, 92)])
cor_dep2 <- t(as.data.frame(cor_dep[, colSums(cor_dep) > .8]))

high_cor <- colnames(cor_dep2)

correlations <- cor(all2[, names(all2) %in% high_cor])
redundant <- findCorrelation(correlations, cutoff = .95)

high_cor2 <- high_cor[-redundant]

all_reduced1 <- cbind(all2[,c(1, 4, 5, 8, 28, 89, 92:103)], all2[, names(all2) %in% high_cor])
all_reduced1$trend <- seq(1:nrow(all_reduced1))

keep_parse <- names(all_reduced1[, c(5:17, 23)])
keep_hc <- names(all_reduced1[, c(6:17, 19:21, 23)])
#FPOPA & Wholesale trade for APW 1

#Build train and test sets

train_parse <- subset(all_reduced1, date < lastact)
train_parse <- subset(train_parse, date > "1999-01-31")
train_date <- train_parse$date
train_parse <- train_parse[, which(names(train_parse) %in% keep_parse)]

test_parse <- subset(all_reduced1, date >= lastact)
test_month <- test_parse$MONTH
test_year <- test_parse$YEAR
test_date <- test_parse$date
test_parse <- test_parse[, which(names(test_parse) %in% keep_parse)]

#Set 2
train_hc <- subset(all_reduced1, date < lastact)
train_hc <- subset(train_hc, date > "1999-01-31")
train_hc <- train_hc[, which(names(train_hc) %in% keep_hc)]


test_hc <- subset(all_reduced1, date >= lastact)
test_hc <- test_hc[, which(names(test_hc) %in% keep_hc)]

# Parsimonious LM
set.seed(1)
cntrl <- trainControl(method = "timeslice", initialWindow = 96, horizon = 18, fixedWindow = TRUE)
lm <- train(CUST ~ ., 
            data = train_parse,
            method = "gamSpline",
            tuneLength = 10,
            trControl = cntrl)
lm

# Lasso
set.seed(1)
lasso <- train(CUST ~ .,
               data = train_hc,
               method = "lasso",
               tuneLength = 5)
lasso

feat1_lm <- predict(lm, train_parse)
feat2_lasso <- predict(lasso, train_hc)

feat1test_lm <- predict(lm, test_parse)
feat2test_lasso <- predict(lasso, test_hc)

forecast <- data.frame(YEAR = as.numeric(as.character(test_year)), MONTH = as.numeric(as.character(test_month)), lm = feat1test_lm, lasso = feat2test_lasso)

#Check growth rates
check <- left_join(aep, forecast, by = c("YEAR", "MONTH")) %>%
        group_by(YEAR) %>%
        summarise(customers = mean(CUST),
                  m1 = mean(lm),
                  m2 = mean(lasso)) %>%
        mutate(cust = ifelse(YEAR < yr, (customers/lag(customers))-1, NA),
               customers = ifelse(YEAR < yr, customers, NA),
               m1 = ifelse(YEAR < yr, customers, m1),
               m2 = ifelse(YEAR < yr, customers, m2),
               model1 = (m1/lag(m1))-1,
               model2 = (m2/lag(m2))-1,
               diff = ifelse(YEAR <= yr, m1-lag(m1), NA)) %>%
        select(YEAR, m1, model1, diff, m2, model2) %>%
        filter(YEAR >= 2010)
check

stack_train <- data.frame(CUST = train_parse$CUST, lm = feat1_lm, lasso = feat2_lasso)
stack_test <- data.frame(CUST = test_parse$CUST, lm = feat1test_lm, lasso = feat2test_lasso)

#Final model
set.seed(1)
final <- train(CUST ~ .,
               data = stack_train,
               method = "gaussprLinear",
               tuneLength = 5,
               trControl = trainControl(method = "cv", number = 5))
final

fit <- predict(final, newdata = stack_train)
fit2 <- data.frame(CUST = stack_train$CUST, pred = fit)
fit2$resid <- (fit2$CUST - fit2$pred)/fit2$CUST
fit2$date <- train_date

#Check fitted vs. predicted
ggplot(fit2, aes(x = date, y = resid)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent)

final_pred <- predict(final, newdata = stack_test)

forecast_final <- data.frame(YEAR = as.numeric(as.character(test_year)), MONTH = as.numeric(as.character(test_month)), pred = final_pred)

#Add factors:
forecast_final <- mutate(forecast_final,
                         pred = pred)

#Check growth rates
check_final <- left_join(aep, forecast_final, by = c("YEAR", "MONTH")) %>%
        group_by(YEAR) %>%
        summarise(customers = mean(CUST),
                  final = mean(pred)) %>%
        mutate(customers = ifelse(YEAR < yr, customers, NA),
               final = ifelse(YEAR < yr, customers, final),
               chg = (final/lag(final))-1) %>%
        select(YEAR, final, chg) %>%
        filter(YEAR >= 2010)
check_final

stack_test$pred <- forecast_final$pred
stack_test$diff <- stack_test$pred - stack_test$CUST

stack_test$MONTH <- as.numeric(test_month)
stack_test$YEAR <- test_year
stack_test$YEAR <- as.numeric(as.character(stack_test$YEAR))
stack_test$date <- test_date

budg$YEAR <- as.numeric(budg$YEAR)
budg$MONTH <- as.numeric(budg$MONTH)

test2 <- inner_join(stack_test, budg, by = c("YEAR", "MONTH"))
test2$bdiff <- test2$bcust - test2$CUST
test2$lm_diff <- stack_test$lm - stack_test$CUST
test2$lasso_diff <- stack_test$lasso - stack_test$CUST

#Budget Difference
mean(abs(test2$bdiff[13:nrow(test2)]))

#Linear Model Difference
mean(abs(test2$lm_diff[13:nrow(test2)]))

#Lasso Difference
mean(abs(test2$lasso_diff[13:nrow(test2)]))

#Stacked model difference
mean(abs(test2$diff[13:nrow(test2)]))

#Graph
ggplot(test2, aes(x = date)) +
        geom_line(aes(y = pred, col = "stacked"), size = 1.1) +
        geom_line(aes(y = CUST, col = "actual"), size = 1.4) +
        geom_line(aes(y = bcust, col = "budget"), size = 1.1) +
        geom_line(aes(y = lm, col = "lm")) +
        geom_line(aes(y = lasso, col = "lasso"))
ggtitle("Comparison to Actual Customer Counts") 

#Stats by year
stats <- test2 %>%
        group_by(YEAR) %>%
        mutate(b_mape = (abs(bdiff)/CUST),
               stack_mape = (abs(diff)/CUST),
               lm_mape = (abs(lm_diff)/CUST),
               lasso_mape = (abs(lasso_diff)/CUST)) %>%
        summarise_all(funs(mean)) %>%
        select(YEAR, CUST, bdiff, diff, lm_diff, lasso_diff, b_mape, stack_mape, lm_mape, lasso_mape)
stats