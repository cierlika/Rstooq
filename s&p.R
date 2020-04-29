X <- X_spx_d[X_spx_d$Data>='1950-01-01',]

plot(log(X$Zamkniecie), type = "l")


X$Y <- NaN

Data<-as.data.frame(seq(as.Date("1950/1/1"), as.Date("2020/4/16"), "days"))
names(Data)<-"Data"
names(X)

library(imputeTS)

X_all<-merge(Data, X, by = "Data", all.x = TRUE)

X_all$Zamkniecie<-na_locf(X_all$Zamkniecie)
X_all$Otwarcie<-na_locf(X_all$Otwarcie)
X_all$Najwyzszy<-na_locf(X_all$Najwyzszy)
X_all$Najnizszy<-na_locf(X_all$Najnizszy)
X_all$Wolumen<-as.numeric(X_all$Wolumen)
X_all$Wolumen<-na_locf(X_all$Wolumen)
X_all$Y[1:25667]<-ifelse(X_all$Zamkniecie[8:25674]>X_all$Zamkniecie[1:25667],1,0)


X_all$Month <- strftime(X_all$Data, "%m")
X_all$Day <- strftime(X_all$Data, "%d")
X_all$WeekDay<- weekdays(X_all$Data)

#X_all$Y<-as.factor(X_all$Y)
X_all$WeekDay<-as.factor(X_all$WeekDay)
X_all$Day<-as.factor(X_all$Day)
X_all$Month<-as.factor(X_all$Month)

X_all<-X_all[1:25667,]

for(unique_value in unique(X_all$Month)){
  
  
  X_all[paste("month", unique_value, sep = ".")] <- ifelse(X_all$Month == unique_value, 1, 0)
  
}

for(unique_value in unique(X_all$Day)){
  
  
  X_all[paste("day", unique_value, sep = ".")] <- ifelse(X_all$Day == unique_value, 1, 0)
  
}

for(unique_value in unique(X_all$WeekDay)){
  
  
  X_all[paste("weekday", unique_value, sep = ".")] <- ifelse(X_all$WeekDay == unique_value, 1, 0)
  
}

X_all<-X_all[, !(colnames(X_all) %in% c("Month", "Day", "WeekDay", "month.01", "day.01", "weekday.niedziela"))]

#####################

sma<-function(X, name, length){
  
  to_transform<-as.data.frame(X[, paste(name)])
  transformed<-rep(NaN, dim(to_transform)[1])
  
  for(i in 1:dim(to_transform)[1]){
    
    transformed[(i+length)] <- mean(to_transform[i:(i+length),1])
    
  }
  
  transformed[1:dim(to_transform)[1]]
  X_temp <- cbind(X, transformed[1:dim(to_transform)[1]])
  names(X_temp)[dim(X)[2]+1] <- paste("sma", length, name, sep = "_")
  
  return(X_temp)
}


name<- "Zamkniecie"
length<-1095
X_all<-sma(X_all, name, length)


plot(X_all$sma_365_Zamkniecie, type = "l")
lines(X_all$sma_1095_Zamkniecie, col = "red")

X_all<-X_all[is.na(X_all$sma_1095_Zamkniecie) == FALSE,]

names(X_all)

X_all$sma_7_to_Zamkniecie<-X_all$sma_7_Zamkniecie/X_all$Zamkniecie
X_all$sma_14_to_Zamkniecie<-X_all$sma_14_Zamkniecie/X_all$Zamkniecie
X_all$sma_20_to_Zamkniecie<-X_all$sma_20_Zamkniecie/X_all$Zamkniecie
X_all$sma_30_to_Zamkniecie<-X_all$sma_30_Zamkniecie/X_all$Zamkniecie
X_all$sma_50_to_Zamkniecie<-X_all$sma_50_Zamkniecie/X_all$Zamkniecie
X_all$sma_60_to_Zamkniecie<-X_all$sma_60_Zamkniecie/X_all$Zamkniecie
X_all$sma_90_to_Zamkniecie<-X_all$sma_90_Zamkniecie/X_all$Zamkniecie
X_all$sma_120_to_Zamkniecie<-X_all$sma_120_Zamkniecie/X_all$Zamkniecie
X_all$sma_180_to_Zamkniecie<-X_all$sma_180_Zamkniecie/X_all$Zamkniecie
X_all$sma_365_to_Zamkniecie<-X_all$sma_365_Zamkniecie/X_all$Zamkniecie
X_all$sma_730_to_Zamkniecie<-X_all$sma_730_Zamkniecie/X_all$Zamkniecie
X_all$sma_1095_to_Zamkniecie<-X_all$sma_1095_Zamkniecie/X_all$Zamkniecie


X_all$sma_1095_to_sma_730<-X_all$sma_1095_Zamkniecie/X_all$sma_730_Zamkniecie
X_all$sma_730_to_sma_365<-X_all$sma_730_Zamkniecie/X_all$sma_365_Zamkniecie
X_all$sma_365_to_sma_180<-X_all$sma_365_Zamkniecie/X_all$sma_180_Zamkniecie
X_all$sma_180_to_sma_120<-X_all$sma_180_Zamkniecie/X_all$sma_120_Zamkniecie
X_all$sma_120_to_sma_90<-X_all$sma_120_Zamkniecie/X_all$sma_90_Zamkniecie
X_all$sma_90_to_sma_60<-X_all$sma_90_Zamkniecie/X_all$sma_60_Zamkniecie
X_all$sma_60_to_sma_50<-X_all$sma_60_Zamkniecie/X_all$sma_50_Zamkniecie
X_all$sma_50_to_sma_30<-X_all$sma_50_Zamkniecie/X_all$sma_30_Zamkniecie
X_all$sma_30_to_sma_20<-X_all$sma_30_Zamkniecie/X_all$sma_20_Zamkniecie
X_all$sma_20_to_sma_14<-X_all$sma_20_Zamkniecie/X_all$sma_14_Zamkniecie
X_all$sma_14_to_sma_7<-X_all$sma_14_Zamkniecie/X_all$sma_7_Zamkniecie

hist(X_all$sma_90_to_sma_60, breaks = 100)

train<-X_all[1:24000,2:89]
test<-X_all[24001:24572,2:89]

train$Y<-as.factor(train$Y)
test$Y<-as.factor(test$Y)
summary(train)
train$Y<-factor(train$Y)
test$Y<-factor(test$Y)

nodesize<- c(2, 5, 10, 20, 30, 50, 100, 200)
acc<-{}
auc_m<-{}
for (i in 1:8){
  
  fit_rf <- randomForest(Y~., data = train, ntree = 300, nodesize = nodesize)
  plot(fit_rf)
  
  varImpPlot(fit_rf)
  
  hist(predict(fit_rf, test, type = "prob")[,2], breaks = 50)
  preds<-predict(fit_rf, test, type = "response")
  
  table(test$Y, preds)
  table_acc <- table(test$Y, preds)
  acc[i]<-(table_acc[1,1]+table_acc[2,2])/sum(table_acc)
  
  library(pROC)
  
  auc_m[i]<-auc(test$Y, predict(fit_rf, test, type = "prob")[,2])
}


fit_rf <- randomForest(Y~., data = train, ntree = 300, nodesize = 5)
plot(fit_rf)

varImpPlot(fit_rf)

partialPlot(fit_rf, train, Wolumen)

hist(predict(fit_rf, train, type = "prob")[,2], breaks = 50)
preds<-predict(fit_rf, test, type = "response")

table(test$Y, preds)
table_acc <- table(test$Y, preds)
(table_acc[1,1]+table_acc[2,2])/sum(table_acc)

library(pROC)

auc(test$Y, predict(fit_rf, test, type = "prob")[,2])

fit1 <- glm(Y~., data = train, family = binomial())
fit2 <- glm(Y ~ 1, data = train, family = binomial())
fit3<-stepAIC(fit1,direction="backward")
fit4<-stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))
fit5<-stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2))

summary(fit5)

hist(predict.glm(fit5, test, type = "response"), breaks = 50)

preds<-ifelse(predict.glm(fit5, test, type = "response") > 0.5, 1, 0)

table(test$Y, preds)
table_acc <- table(test$Y, preds)
(table_acc[1,1]+table_acc[2,2])/sum(table_acc)

library(pROC)

auc(test$Y, predict.glm(fit5, test, type = "response"))
roc <- roc(test$Y, predict.glm(fit5, test, type = "response"))
plot(roc)
