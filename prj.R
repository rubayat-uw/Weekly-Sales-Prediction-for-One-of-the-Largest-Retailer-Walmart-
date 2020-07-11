library(base)
library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)
library(lubridate)
library(rpart)
library(rattle)
library(car)
library(caret)
library(neuralnet)
library(corrplot)
library(rpart.plot)
library(forecast)
library(fma)
library(randomForest)
#Loading files to work with
train    <- read.csv(file.choose(),header = TRUE)
stores   <- read.csv(file.choose(),header = TRUE)
features <- read.csv(file.choose(),header = TRUE)
test   <- read.csv(file.choose(),header = TRUE)
source('F:/Souradip Dey/MSCI 718/project/walmart/Final COde/train.R', echo=TRUE)

source('C:/UWaterloo/Semester/Winter 2018/MSCI 718/Project/New folder/test.R', echo=TRUE)
train<-prjTrain(train)
test<-prjTest(test)

#train$Weekly_Sales<-log(train$Weekly_Sales)
index <- createDataPartition(train$Weekly_Sales,list = FALSE,p=0.8)
train.train <-train[index,]
train.test <- train[-index,]
head(train.train)
aggregate(train.train[,"Weekly_Sales"], by=train.train[,c("Store"), drop=FALSE], mean)



##linear regression
R_train=train[1:100,]
R_test=train[101:143,]
Data <- subset( R_train, select = -c(Date,Type,CPI,Store,Size))
R_fit <- lm(Weekly_Sales ~., data=Data)
Data_test <- subset( R_test, select = -c(Date,Type,CPI,Store,Size))
predict_fit_confidence1 <- predict(R_fit, R_test)


accuracy(R_fit$fitted.values,R_train$Weekly_Sales)
plot(R_train$Weekly_Sales,col="blue",type='l',xlab="Time(Week) ",ylab="Weekly Sales",main="Weekly Sales of Store 1 Train Set")
lines(R_fit$fitted.values,col="red",type='l')
legend("topright",lty=1,col=c(1,2),legend = c("Actual","Predicted"))

accuracy(predict_fit_confidence1,R_test$Weekly_Sales)
plot(R_test$Weekly_Sales,col="blue",type='l',xlab="Time(Week) ",ylab="Weekly Sales",main="Weekly Sales of Store 1 Test Set")
lines(predict_fit_confidence1,col="red",type='l')
legend("topright",lty=1,col=c(1,2),legend = c("Actual","Predicted"))


sd(predict_fit_confidence1)
sd(R_fit$residuals)
qqnorm(R_fit$residuals)
summary(R_fit)


#fit <- lm(Weekly_Sales ~.-Type-CPI, data=train.train[,!colnames(train.train) %in% c("Date")])
##test$Weekly_Sales<-0
#summary()
#aggregate(train.test[,"Weekly_Sales"], by=train.test[,c("Store"), drop=FALSE], mean)
#fit1 <- lm(Weekly_Sales ~.-Type-CPI, data=train.test[,!colnames(train.test) %in% c("Date")])

##fit$xlevels[["Date"]] <- union(fit$xlevels[["Date"]], levels(test$Date))
#predict_fit_confidence1 <- predict(fit, test)

#plot(fit$fitted.values,col='red',typ='l')
#lines(train.train$Weekly_Sales)

#accuracy(fit$fitted.values,train.train$Weekly_Sales)

#Model: Random Forest
#rf<- randomForest(Weekly_Sales ~.-Type-CPI, data=train.train[,!colnames(train.train) %in% c("Date")],ntree=1)


R_rf <- randomForest(Weekly_Sales ~., data=Data,ntree=1000)

predict_rf<-predict(R_rf, R_test)
accuracy(R_rf$predicted,R_train$Weekly_Sales)
accuracy(predict_rf,R_test$Weekly_Sales)

residula_rf=abs(R_train$Weekly_Sales-R_rf$predicted)

#sd(residula_rf)
qqnorm(residula_rf)
#sd(predict_rf)
#qqnorm(residula_rf)

plot(R_train$Weekly_Sales,col="blue",type='l',xlab="Time(Week) ",ylab="Weekly Sales",main="Weekly Sales of Store 1 Train Set")
lines(R_rf$predicted,col="red",type='l')
legend("topright",lty=1,col=c(1,2),legend = c("Actual","Predicted"))

plot(R_test$Weekly_Sales,col="blue",type='l',xlab="Time(Week) ",ylab="Weekly Sales",main="Weekly Sales of Store 1 Test Set")
lines(predict_rf,col="red",type='l')
legend("topright",lty=1,col=c(1,2),legend = c("Actual","Predicted"))


plot(predict_rf,col='red',typ='l')
lines(R_test$Weekly_Sales)
summary(R_rf)

#ARIMA
ts_Train<-ts(train$Weekly_Sales[1:143],start=c(2010,5),frequency = 52)

acf(ts_Train)
pacf(ts_Train)

#fit_arima <-auto.arima(ts_Train)
fit_arima<-Arima(ts_Train, order=c(2,1,1), seasonal=c(0,1,0), lambda =0)
#fit_arima<-arima(ts_Train, order = c(1, 1, 1))

arima_forecast<-forecast(fit_arima,h=39)
Acf(residuals(fit_arima))
#Box.test(residuals(fit_arima), lag=24, fitdf=4, type="Ljung")

plot(ts_Train,col='blue',ylab="Weekly Sales",main="Weekly Sales of Store 1")
lines(fit_arima$fitted,col="red")
legend("topright",lty=1,col=c(1,2),legend = c("Actual","Predicted"))

plot(arima_forecast)
plot(arima_forecast,xlab="Time",ylab="Weekly Sales",main="Forecast of Store 1 using ARIMA Model")

plot(decompose(ts_Train))
acf(ts_Train,lag=52)
pacf(ts_Train,lag=52)
  
qqnorm(fit_arima$residuals)

accuracy(fit_arima$fitted,ts_Train)
sd(fit_arima$residuals)

summary(fit_arima)


# one-hot-encoding features
train1 = as.data.frame(train)
ohe_feats = c('Type', 'IsHoliday')
dummies = dummyVars(~ Type + IsHoliday , data = train1)
df_all_ohe <- as.data.frame(predict(dummies, newdata = train1))
df_all_combined <- cbind(train1[,-c(which(colnames(train1) %in% ohe_feats))],df_all_ohe)

train_ <- as.data.table(df_all_combined)
train_$Date <- NULL
train_$Store<-NULL

maxs <- apply(train_, 2, max) 
mins <- apply(train_, 2, min)
scaled <- as.data.frame(scale(train_ , center = mins, scale = maxs - mins))


n <- names(train_)
f <- as.formula(paste("Weekly_Sales ~", paste(n[!n %in% "Weekly_Sales"], collapse = " + ")))
nn <- neuralnet(f,data=train_)
plot(nn)
