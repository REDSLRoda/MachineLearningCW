library(tidyverse)
library(fpp2)
library(readxl)
library(Metrics)
library(MLmetrics)
library(neuralnet)
library(tsDyn)
library(vars)
library(quantmod)
library(forecast)
library(nets)
library(dplyr)

#load data
Considerdata <- read.csv("C:/Users/Achintha Rodrigo/Desktop/CW/uow_consumption(1).csv")
colnames(Considerdata) <- c("date","18:00","19:00","20:00")
summary(Considerdata)
boxplot(Considerdata[,4])#plot before normalizing data

#The data that is been read
head(Considerdata)
#examining what is the data type used for the columns of data
glimpse(Considerdata)
#Size of the x label
dim(Considerdata)
#searching the data collection for any null values at any location
which(is.na.data.frame(Considerdata))
#normalization function
normalization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Scaling data for 20th hour
dataScaling <- as.data.frame(Considerdata[,4])
dataScaling

dataScaled <- as.data.frame(lapply(dataScaling, normalization))
View(dataScaled)
summary(dataScaled)
boxplot(dataScaled[1]) #plot after normalization

#data used for Training and testing
test <- dataScaled[381:470,]
test
train <- dataScaled[1:380,]
train

#Creating a function to reverse the normalization function
renormalizing <- function(x, min, max) { 
  return( (max - min)*x + min )
}

#t1 used for I/O matrix 
TrainingT1 <- bind_cols(S_previous = lag(train,1),
                     S_present = train)

TestingT1 <- bind_cols(S_previous = lag(test,1),
                    S_current = test)

TrainingT1
#t2 used for I/O matrix 
TrainingT2 <- bind_cols(S_previous2 = lag(train,2),
                     S_previous = lag(train,1),
                     S_current = train)

TestingT2 <- bind_cols(S_previous2 = lag(test,2),
                    S_previous = lag(test,1),
                    S_current = test)


#t3 used for I/O matrix 
TrainingT3 <- bind_cols(S_previous3 = lag(train,3),
                     S_previous2 = lag(train,2),
                     S_previous = lag(train,1),
                     S_current = train)

TestingT3 <- bind_cols(S_previous3 = lag(test,3),
                    S_previous2 = lag(test,2),
                    S_previous = lag(test,1),
                    S_current = test)


#t4 used for I/O matrix 
TrainingT4 <- bind_cols(S_previous4 = lag(train,4),
                     S_previous3 = lag(train,3),
                     S_previous2 = lag(train,2),
                     S_previous = lag(train,1),
                     S_current = train)

TestingT4<- bind_cols(S_previous4 = lag(test,4),
                    S_previous3 = lag(test,3),
                    S_previous2 = lag(test,2),
                    S_previous = lag(test,1),
                    S_current = test)


#t7 used for I/O matrix 
TrainingT7 <- bind_cols(S_previous6 = lag(train,7),
                     S_previous6 = lag(train,6),
                     S_previous5 = lag(train,5),
                     S_previous4 = lag(train,4),
                     S_previous3 = lag(train,3),
                     S_previous2 = lag(train,2),
                     S_previous = lag(train,1),
                     S_current = train)

TestingT7 <- bind_cols(S_previous6 = lag(test,7),
                    S_previous6 = lag(test,6),
                    S_previous5 = lag(test,5),
                    S_previous4 = lag(test,4),
                    S_previous3 = lag(test,3),
                    S_previous2 = lag(test,2),
                    S_previous = lag(test,1),
                    S_current = test)

#t1 
TrainingT1 <- TrainingT1[complete.cases(TrainingT1),]
TrainingT1
TestingT1 <- TestingT1[complete.cases(TestingT1),]
TestingT1


#t2 
TrainingT2 <- TrainingT2[complete.cases(TrainingT2),]
TrainingT2
TestingT2 <- TestingT2[complete.cases(TestingT2),]
TestingT2 

#t3
TrainingT3 <- TrainingT3[complete.cases(TrainingT3),]
TrainingT3 
TestingT3<- TestingT7[complete.cases(TestingT3),]
TestingT3


#t4
TrainingT4 <- TrainingT4[complete.cases(TrainingT4),]
TrainingT4 

TestingT4 <- TestingT4[complete.cases(TestingT4),]
TestingT4

#7
TrainingT7 <- TrainingT7[complete.cases(TrainingT7),]
TrainingT7
TestingT7<- TestingT7[complete.cases(TestingT7),]
TestingT7

#naming the colums used for t1 
colnames(TrainingT1) <- c("previousDay","PresentDay")
colnames(TestingT1) <- c("previousDay","PresentDay")
TrainingT1$PresentDay
#naming the colums used for t2
colnames(TrainingT2) <- c("DayEarlier2","previousDay","PresentDay")
colnames(TestingT2) <- c("DayEarlier2","previousDay","PresentDay")
TrainingT2$PresentDay

#naming the colums used for t3 
colnames(TrainingT3) <- c("DayEarlier3","DayEarlier2","previousDay","PresentDay")
colnames(TestingT3) <- c("DayEarlier3","DayEarlier2","previousDay","PresentDay")
TestingT3$PresentDay

#naming the colums used for t4 
colnames(TrainingT4) <- c("DayEarlier4","DayEarlier3","DayEarlier2","previousDay","PresentDay")
colnames(TestingT4) <- c("DayEarlier4","DayEarlier3","DayEarlier2","previousDay","PresentDay")
TestingT4$PresentDay

#naming the colums used for t7 
colnames(TrainingT7) <- c("DayEarlier7","DayEarlier6", "DayEarlier5","DayEarlier4","DayEarlier3","DayEarlier2","previousDay","PresentDay")
colnames(TestingT7) <- c("DayEarlier7","DayEarlier6", "DayEarlier5","DayEarlier4","DayEarlier3","DayEarlier2","previousDay","PresentDay")
TestingT7$DayEarlier6


#Creating Neural Network models with AR

#First t1 NN Model number 2 hidden layers 
NNmodel1<- neuralnet(PresentDay ~ previousDay,hidden = 2, 
                data = TrainingT1, linear.output = TRUE)

plot(NNmodel1)
NNmodel1$result.matrix
NNmodel1$weights

#Model Performance
TrainingT1[1:2]
t1_Model_result <- compute(NNmodel1, TrainingT1[1])
predicted1<- t1_Model_result$net.result
predicted1
#re-normalizing the predicted values.
renomalize1 <- renormalizing(predicted1 , min(dataScaling), max(dataScaling))
renomalize1<- round(renomalize1,1)
renomalize1

expectedrenomalize1 <- renormalizing(TestingT1,min(dataScaling),max(dataScaling))
expectedrenomalize1 <- round(expectedrenomalize1,1)
expectedrenomalize1
endResult1 <- cbind(expectedrenomalize1,renomalize1[2])
colnames(endResult1) <- c("Expected","Predicted")
endResult1
summary(NNmodel1$result.matrix)


plot(endResult1$Expected , ylab = "Predicted vs Expected", type="l", col="red" )
par(new=TRUE)
plot(endResult1$Predicted, ylab = " ", yaxt="n", type="l", col="green" ,main='Expected
Value Vs Predicted Value Graph')
legend("topleft",
       c("Predicted","Expected"),
       fill=c("green","red")
)

#RMSE
RMSE(endResult1$Expected,endResult1$Predicted)
#MSE
MSE(endResult1$Expected,endResult1$Predicted)
#MAPE
mape(endResult1$Expected,endResult1$Predicted)


#Second NN for t2 Model with hidden layers 
NNmodel2 <- neuralnet(PresentDay ~ DayEarlier2 + previousDay,hidden = 2, 
                            data = TrainingT2, linear.output = TRUE)
#Plotting NNmodel2
plot(NNmodel2)
NNmodel2$result.matrix
NNmodel2$weights



#Model Performance
TrainingT2[1:3]
t2_Model_result <- compute(NNmodel2, TrainingT2[1:2])
predicted2<- t2_Model_result$net.result
predicted2
#re-normalizing the predicted values.
renomalize2 <- renormalizing(predicted2 , min(dataScaling), max(dataScaling))
renomalize2<- round(renomalize2,1)
renomalize2

expectedrenomalize2 <- renormalizing(TestingT2,min(dataScaling),max(dataScaling))
expectedrenomalize2 <- round(expectedrenomalize2,1)
expectedrenomalize2
endResult2 <- cbind(expectedrenomalize2,renomalize2[3])
colnames(endResult2) <- c("Expected","Predicted")
endResult2
summary(NNmodel2$result.matrix)



plot(endResult2$Expected , ylab = "Predicted vs Expected", type="l", col="red" )
par(new=TRUE)
plot(endResult2$Predicted, ylab = " ", yaxt="n", type="l", col="green" ,main='Expected
Value Vs Predicted Value Graph')
legend("topleft",
       c("Predicted","Expected"),
       fill=c("green","red")
)


#RMSE
RMSE(endResult2$Expected,endResult2$Predicted)
#MSE
MSE(endResult2$Expected,endResult2$Predicted)
#MAPE
mape(endResult2$Expected,endResult2$Predicted)


#third NN for t3 Model with hidden layers
NNmodel3<- neuralnet(PresentDay ~ DayEarlier3 + DayEarlier2 + previousDay,hidden = 2, 
                            data = TrainingT3, linear.output = TRUE)
#Plotting NNmodel2
plot(NNmodel3)
NNmodel3$result.matrix
NNmodel3$weights



#Model Performance
TrainingT3[1:4]
t3_Model_result <- compute(NNmodel3, TrainingT3[1:3])
predicted3<- t3_Model_result$net.result
predicted3
#re-normalizing the predicted values.
renomalize3 <- renormalizing(predicted3 , min(dataScaling), max(dataScaling))
renomalize3<- round(renomalize3,1)
renomalize3

expectedrenomalize3 <- renormalizing(TestingT3,min(dataScaling),max(dataScaling))
expectedrenomalize3 <- round(expectedrenomalize3,1)
expectedrenomalize3
endResult3 <- cbind(expectedrenomalize3,renomalize3[4])
colnames(endResult3) <- c("Expected","Predicted")
endResult3
summary(NNmodel2$result.matrix)



plot(endResult3$Expected , ylab = "Predicted vs Expected", type="l", col="red" )
par(new=TRUE)
plot(endResult3$Predicted, ylab = " ", yaxt="n", type="l", col="green" ,main='Expected
Value Vs Predicted Value Graph')
legend("topleft",
       c("Predicted","Expected"),
       fill=c("green","red")
)


#RMSE
RMSE(endResult3$Expected,endResult3$Predicted)
#MSE
MSE(endResult3$Expected,endResult3$Predicted)
#MAPE
mape(endResult3$Expected,endResult3$Predicted)

#Fourth NN for t4 Model with hidden layers
NNmodel4<- neuralnet(PresentDay ~ DayEarlier4 + DayEarlier3 + DayEarlier2 + previousDay,hidden = 2, 
                     data = TrainingT4, linear.output = TRUE)
#Plotting NNmodel4
plot(NNmodel4)
NNmodel4$result.matrix
NNmodel4$weights



#Model Performance
TrainingT4[1:5]
t4_Model_result <- compute(NNmodel4, TrainingT4[1:4])
predicted4<- t4_Model_result$net.result
predicted4
#re-normalizing the predicted values.
renomalize4 <- renormalizing(predicted4 , min(dataScaling), max(dataScaling))
renomalize4<- round(renomalize4,1)
renomalize4

expectedrenomalize4 <- renormalizing(TestingT4,min(dataScaling),max(dataScaling))
expectedrenomalize4 <- round(expectedrenomalize4,1)
expectedrenomalize4
endResult4 <- cbind(expectedrenomalize4,renomalize4[5])
colnames(endResult4) <- c("Expected","Predicted")
endResult4
summary(NNmodel4$result.matrix)
sqrt(mean((endResult4$Expected - endResult4$Predicted)^2))


plot(endResult4$Expected , ylab = "Predicted vs Expected", type="l", col="red" )
par(new=TRUE)
plot(endResult4$Predicted, ylab = " ", yaxt="n", type="l", col="green" ,main='Expected
Value Vs Predicted Value Graph')
legend("topleft",
       c("Predicted","Expected"),
       fill=c("green","red")
)


#RMSE
RMSE(endResult4$Expected,endResult4$Predicted)
#MSE
MSE(endResult4$Expected,endResult4$Predicted)
#MAPE
mape(endResult4$Expected,endResult4$Predicted)

# NN for t7 Model with hidden layers
NNmodel7<- neuralnet(PresentDay ~ DayEarlier7 + DayEarlier6 + DayEarlier5 + DayEarlier4 + DayEarlier3 + DayEarlier2 + previousDay,hidden = 2, 
                     data = TrainingT7, linear.output = TRUE)
#Plotting NNmodel7
plot(NNmodel7)
NNmodel7$result.matrix
NNmodel7$weights



#Model Performance
TrainingT7[1:8]
t7_Model_result <- compute(NNmodel7, TrainingT7[1:7])
predicted7<- t7_Model_result$net.result
predicted7
#re-normalizing the predicted values.
renomalize7 <- renormalizing(predicted7 , min(dataScaling), max(dataScaling))
renomalize7<- round(renomalize7,1)
renomalize7

expectedrenomalize7 <- renormalizing(TestingT7,min(dataScaling),max(dataScaling))
expectedrenomalize7 <- round(expectedrenomalize7,1)
expectedrenomalize7
endResult7 <- cbind(expectedrenomalize7,renomalize7[8])
colnames(endResult7) <- c("Expected","Predicted")
endResult7
summary(NNmodel7$result.matrix)


plot(endResult7$Expected , ylab = "Predicted vs Expected", type="l", col="red" )
par(new=TRUE)
plot(endResult7$Predicted, ylab = " ", yaxt="n", type="l", col="green" ,main='Expected
Value Vs Predicted Value Graph')
legend("topleft",
       c("Predicted","Expected"),
       fill=c("green","red")
)


#RMSE
RMSE(endResult7$Expected,endResult7$Predicted)
#MSE
MSE(endResult7$Expected,endResult7$Predicted)
#MAPE
mape(endResult7$Expected,endResult7$Predicted)


########################################

