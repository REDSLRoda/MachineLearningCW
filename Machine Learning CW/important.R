library(readxl)
library(tidyverse)
library(neuralnet)

#load data
ConsiderdataNarx <- read.csv("C:/Users/Achintha Rodrigo/Desktop/CW/uow_consumption(1).csv")
colnames(ConsiderdataNarx) <- c("date","18:00","19:00","20:00")

#The data that is been read
head(ConsiderdataNarx)

dataNarx <- as.data.frame(ConsiderdataNarx[2:4])
#Creating Neural Network models with NARX
summary(ConsiderdataNarx[2:4])
boxplot(ConsiderdataNarx[2:4])#plot before normalizing data
dataNarx 
normalization <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#Normalizing
dataNarxAfter <- as.data.frame(lapply(dataNarx , normalization))
#data used for Training and testing
test <- dataNarxAfter[381:470,]
test
train <- dataNarxAfter[1:380,]
train

#Creating a function to reverse the normalization function
renormalizing <- function(x, min, max) { 
  return( (max - min)*x + min )
}

#t1 used for I/O matrix 
TrainingT1 <- bind_cols(S_previous = lag(train,1),
                        S_present = train)
dim(TrainingT1)
TrainingT1
TestingT1 <- bind_cols(S_previous = lag(test,1),
                       S_present = test)
dim(TestingT1)
#t2 used for I/O matrix 
TrainingT2 <- bind_cols(S_previous2 = lag(train,2),
                        S_previous = lag(train,1),
                        S_present = train)
dim(TrainingT2)
TrainingT2

TestingT2 <- bind_cols(S_previous2 = lag(test,2),
                       S_previous = lag(test,1),
                       S_present = test)

dim(TestingT2)
#t3 used for I/O matrix 
TrainingT3 <- bind_cols(S_previous3 = lag(train,3),
                        S_previous2 = lag(train,2),
                        S_previous = lag(train,1),
                        S_present = train)
dim(TrainingT3)
TrainingT3
TestingT3 <- bind_cols(S_previous3 = lag(test,3),
                       S_previous2 = lag(test,2),
                       S_previous = lag(test,1),
                       S_present = test)

dim(TestingT3)
#t4 used for I/O matrix 
TrainingT4 <- bind_cols(S_previous4 = lag(train,4),
                        S_previous3 = lag(train,3),
                        S_previous2 = lag(train,2),
                        S_previous = lag(train,1),
                        S_present = train)
dim(TrainingT4)
TrainingT4
TestingT4<- bind_cols(S_previous4 = lag(test,4),
                      S_previous3 = lag(test,3),
                      S_previous2 = lag(test,2),
                      S_previous = lag(test,1),
                      S_present = test)

dim(TestingT4)
#t7 used for I/O matrix 
TrainingT7 <- bind_cols(S_previous6 = lag(train,7),
                        S_previous6 = lag(train,6),
                        S_previous5 = lag(train,5),
                        S_previous4 = lag(train,4),
                        S_previous3 = lag(train,3),
                        S_previous2 = lag(train,2),
                        S_previous = lag(train,1),
                        S_present = train)
dim(TrainingT7)
TrainingT7
TestingT7 <- bind_cols(S_previous6 = lag(test,7),
                       S_previous6 = lag(test,6),
                       S_previous5 = lag(test,5),
                       S_previous4 = lag(test,4),
                       S_previous3 = lag(test,3),
                       S_previous2 = lag(test,2),
                       S_previous = lag(test,1),
                       S_present = test)
dim(TestingT7)
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

#get t1
colnames(TrainingT1) <- c("Before18t1","Before19t1","Before20t1",
                          "present18","present19","present20")


colnames(TestingT1) <- c("Before18t1","Before19t1","Before20t1",
                         "present18","present19","present20")

#selcting present day 18  and 19 hour values
TrainingT1<- subset(TrainingT1, select = -c(present18, present19))
TestingT1 <- subset(TestingT1, select = -c(present18, present19))

dim(TrainingT1)

#get t2
colnames(TrainingT2) <- c("Before18t2","Before19t2","Before20t2",
                          "Before18t1","Before19t1","Before20t1",
                          "present18","present19","present20")

colnames(TestingT2) <- c("Before18t2","Before19t2","Before20t2",
                         "Before18t1","Before19t1","Before20t1",
                         "present18","present19","present20")
TrainingT2$present20

#selcting present day 18  and 19 hour values
TrainingT2 <- subset(TrainingT2, select = -c(present18, present19))
TestingT2 <- subset(TestingT2, select = -c(present18, present19))

dim(TrainingT2)

#get t3
colnames(TrainingT3) <- c("Before18t3","Before19t3","Before20t3",
                          "Before18t2","Before19t2","Before20t2",
                          "Before18t1","Before19t1","Before20t1",
                          "present18","present19","present20")

colnames(TestingT3) <- c("Before18t3","Before19t3","Before20t3",
                         "Before18t2","Before19t2","Before20t2",
                         "Before18t1","Before19t1","Before20t1",
                         "present18","present19","present20")

#selcting present day 18  and 19 hour values
TrainingT3 <- subset(TrainingT3, select = -c(present18, present19))
TestingT3 <- subset(TestingT3, select = -c(present18, present19))

dim(TrainingT3)


#get t4
colnames(TrainingT4) <- c("Before18t4","Before19t4","Before20t4",
                          "Before18t3","Before19t3","Before20t3",
                          "Before18t2","Before19t2","Before20t2",
                          "Before18t1","Before19t1","Before20t1",
                          "present18","present19","present20")

colnames(TestingT4) <- c("Before18t4","Before19t4","Before20t4",
                         "Before18t3","Before19t3","Before20t3",
                         "Before18t2","Before19t2","Before20t2",
                         "Before18t1","Before19t1","Before20t1",
                         "present18","present19","present20")

#selcting present day 18  and 19 hour values
TrainingT4 <- subset(TrainingT4, select = -c(present18, present19))
TestingT4 <- subset(TestingT4, select = -c(present18, present19))

dim(TrainingT4)

#get t7
colnames(TrainingT7) <- c("Before18t7","Before19t7","Before20t7",
                          "Before18t6","Before19t6","Before20t6",
                          "Before18t5","Before19t5","Before20t5",
                          "Before18t4","Before19t4","Before20t4",
                          "Before18t3","Before19t3","Before20t3",
                          "Before18t2","Before19t2","Before20t2",
                          "Before18t1","Before19t1","Before20t1",
                          "present18","present19","present20")

colnames(TestingT7) <- c("Before18t7","Before19t7","Before20t7",
                         "Before18t6","Before19t6","Before20t6",
                         "Before18t5","Before19t5","Before20t5",
                         "Before18t4","Before19t4","Before20t4",
                         "Before18t3","Before19t3","Before20t3",
                         "Before18t2","Before19t2","Before20t2",
                         "Before18t1","Before19t1","Before20t1",
                         "present18","present19","present20")

#selcting present day 18  and 19 hour values
TrainingT7 <- subset(TrainingT7, select = -c(present18, present19))
TestingT7 <- subset(TestingT7, select = -c(present18, present19))

dim(TrainingT7)

#Neural Network t1
NARKNNt1 <- neuralnet(present20 ~ Before18t1 + Before19t1+ Before20t1,
                      hidden = 4, data = TrainingT1, linear.output = TRUE)

plot(NARKNNt1)

#Neural Network t2
NARKNNt2 <- neuralnet(present20 ~ Before18t2+ Before19t2 + Before20t2
                      + Before18t1 + Before19t1+ Before20t1,
                      hidden = 4, data = TrainingT2, linear.output = TRUE)

plot(NARKNNt2)

#Neural Network t3
NARKNNt3 <- neuralnet(present20 ~ Before18t3 + Before19t3 + Before20t3 +
                        Before18t2+ Before19t2 + Before20t2 + 
                        Before18t1 + Before19t1+ Before20t1,
                      hidden = 4, data = TrainingT3, linear.output = TRUE)

plot(NARKNNt3)

#Neural Network t4
NARKNNt4 <- neuralnet(present20 ~ Before18t4 + Before19t4 + Before20t4 +
                        Before18t3 + Before19t3 + Before20t3 +
                        Before18t2+ Before19t2 + Before20t2 + 
                        Before18t1 + Before19t1+ Before20t1,
                      hidden = 4, data = TrainingT4, linear.output = TRUE)

plot(NARKNNt4)

#Model t7
NARKNNt7 <- neuralnet(present20 ~ Before18t7 + Before19t7 + Before20t7 +
                        Before18t6 + Before19t6 + Before20t6 + 
                        Before18t5 + Before19t5 + Before20t5 + 
                        Before18t4 + Before19t4 + Before20t4 + 
                        Before18t3 + Before19t3 + Before20t3 + 
                        Before18t2 + Before19t2 + Before20t2 +
                        Before18t1 + Before19t1 + Before20t1,hidden = 4,
                      data = TrainingT7, linear.output = TRUE)

plot(NARKNNt7)

#Compute result
# t1
Narxresult_t1 <- compute(NARKNNt1,TestingT1[1:3])
predicted1 <- Narxresult_t1$net.result

predicted1

# t2
Narxresult_t2 <- compute(NARKNNt2,TestingT2[1:6])
predicted2 <- Narxresult_t2$net.result

predicted2

# t3
Narxresult_t3 <- compute(NARKNNt3,TestingT3[1:9])
predicted3 <- Narxresult_t3$net.result

predicted3

# t4
Narxresult_t4 <- compute(NARKNNt4,TestingT4[1:12])
predicted4 <- Narxresult_t4$net.result

predicted4

# t7
Narxresult_t7 <- compute(NARKNNt7,TestingT7[1:21])
predicted7 <- Narxresult_t7$net.result

predicted7


#renomalizing
#t1
renomalize1 <- renormalizing(predicted1,min(dataNarx),max(dataNarx))
renomalize1 <- round(renomalize1,1)
renomalize1

expectedrenomalize1 <-  renormalizing(TestingT1,min(dataNarx),max(dataNarx))
expectedrenomalize1 <- round(expectedrenomalize1,1)
expectedrenomalize1

#For 2
renomalize2 <- renormalizing(predicted2,min(dataNarx),max(dataNarx))
renomalize2 <- round(renomalize2,1)
renomalize2

expectedrenomalize2 <- renormalizing(TestingT2,min(dataNarx),max(dataNarx))
expectedrenomalize2 <- round(expectedrenomalize2,1)
expectedrenomalize2

#For 3
renomalize3 <- renormalizing(predicted3,min(dataNarx),max(dataNarx))
renomalize3 <- round(renomalize3,1)
renomalize3

expectedrenomalize3 <- renormalizing(TestingT3,min(dataNarx),max(dataNarx))
expectedrenomalize3 <- round(expectedrenomalize3,1)
expectedrenomalize3

#For 4
renomalize4 <- renormalizing(predicted4,min(dataNarx),max(dataNarx))
renomalize4 <- round(renomalize4,1)
renomalize4

expectedrenomalize4<- renormalizing(TestingT4,min(dataNarx),max(dataNarx))
expectedrenomalize4<- round(expectedrenomalize4,1)
expectedrenomalize4

#For 7
renomalize7 <- renormalizing(predicted7,min(dataNarx),max(dataNarx))
renomalize7 <- round(renomalize7,1)
renomalize7

expectedrenomalize7 <- renormalizing(TestingT7,min(dataNarx),max(dataNarx))
expectedrenomalize7 <- round(expectedrenomalize7,1)
expectedrenomalize7


#end result
#get t1
endResult1 <- cbind(expectedrenomalize1[4],renomalize1)
colnames(endResult1) <- c("Expected","Predicted")
endResult1

#get t2
endResult2 <- cbind(expectedrenomalize2[7],renomalize2)
colnames(endResult2) <- c("Expected","Predicted")
endResult2

#get t3
endResult3 <- cbind(expectedrenomalize3[10],renomalize3)
colnames(endResult3) <- c("Expected","Predicted")
endResult3

#get t4
endResult4 <- cbind(expectedrenomalize4[13],renomalize4)
colnames(endResult4) <- c("Expected","Predicted")
endResult4

#get t7
endResult7 <- cbind(expectedrenomalize7[22],renomalize7)
colnames(endResult7) <- c("Expected","Predicted")
endResult7

#For t1
RMSE(endResult1$Expected,endResult1$Predicted)
#For t2
RMSE(endResult2$Expected,endResult2$Predicted)
#For t3
RMSE(endResult3$Expected,endResult3$Predicted)
#For t4
RMSE(endResult4$Expected,endResult4$Predicted)
#For t7
RMSE(endResult7$Expected,endResult7$Predicted)

#For t1
MSE(endResult1$Expected,endResult1$Predicted)
#For t2
MSE(endResult2$Expected,endResult2$Predicted)
#For t3
MSE(endResult3$Expected,endResult3$Predicted)
#For t4
MSE(endResult4$Expected,endResult4$Predicted)
#For t7
MSE(endResult7$Expected,endResult7$Predicted)


#For t1
mape(endResult1$Expected,endResult1$Predicted)
#For t2
mape(endResult2$Expected,endResult2$Predicted)
#For t3
mape(endResult3$Expected,endResult3$Predicted)
#For t4
mape(endResult4$Expected,endResult4$Predicted)
#For t7
mape(endResult7$Expected,endResult7$Predicted)
