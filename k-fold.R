
rm(list=ls())
library(Hmisc)
library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(mgcv)
require(mgcv)

# Asal data: https://github.com/dutangc/CASdatasets/tree/master/pkg/data

# Data Pra-Processing -----------------------------------------------------

setwd("C:/Users/Kheisya Aurel/Documents/SKRIPSI/Datasets")
load("~/SKRIPSI/Datasets/ausprivauto0405.rda")

mydata = ausprivauto0405
summary(mydata)
nrow(mydata) # 67856
# Variabel respon: ClaimOcc, ClaimNb, IndividualClaim, ClaimAmount
# Variabel prediktor: VehValue (num), VehAge (ch), VehBody (ch), Gender (ch), DrivAge (ch)
sapply(mydata,class)

# Memeriksa Apakah Ada Missing Value  
(CekMissVal <- data.frame(MissVal = sapply(mydata, function(x) sum(is.na(x)))))
# Tidak Ada Missing Value
is_duplicated <- duplicated(mydata)
duplicated_rows <- mydata[is_duplicated, ]
df <- mydata[!duplicated(mydata), ]
nrow(df) # 66650 data
#duplicate <- mydata[duplicated(mydata) | duplicated(mydata, fromLast = TRUE), ] # melihat baris yg duplikat

# IndividualClaim = ClaimAmount/ClaimNb
ClaimAmount <- df$ClaimAmount
ClaimNb <- df$ClaimNb
(n = nrow(df))
IndividualClaim = NULL 
for (i in 1:n){
  if (ClaimAmount[i] > 0){
    IndividualClaim[i] = ClaimAmount[i]/ClaimNb[i]
  } else {
    IndividualClaim[i] = 0
  }
}

data <- cbind(df, IndividualClaim)
summary(data)
describe(data)
nrow(data)
dim(data)

# Shuffle data 
mydata <- data[sample(nrow(data)), ]

# Jumlah fold
n <- 10

# Buat nomor fold untuk setiap baris data setelah di shuffle
folds <- cut(seq(1, nrow(mydata)), breaks=n, labels=FALSE)

MSE01_list <- numeric(n)
MSE02_list <- numeric(n)
MSE03_list <- numeric(n)
MSE04_list <- numeric(n)
MSE1_list <- numeric(n)
MSE2_list <- numeric(n)
MSE3_list <- numeric(n)
MSE4_list <- numeric(n)

# K-Fold Cross Validation GLM
for(i in 1:n){
  # data training dan testing berdasarkan fold
  testIndexes <- which(folds == i, arr.ind=TRUE)
  testing <- data[testIndexes, ]
  training <- data[-testIndexes, ]
  training01 <- subset(training, ClaimAmount != 0)
  
  model1 <- glm(ClaimNb~VehValue+VehAge+VehBody+Gender+DrivAge, family = poisson(link = "log"), data = training, offset = log(Exposure))
  model2 <- glm(ClaimAmount~VehValue+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training01)
  model3 <- gam(ClaimNb~s(VehValue, bs="cr", k=10)+VehAge+VehBody+Gender+DrivAge, family = poisson(link = "log"), 
                data = training, offset = log(Exposure), method="REML")
  model4 <- gam(ClaimAmount~s(VehValue, bs="cr", k=10)+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training01, method="REML")
  
  # Evaluasi Training:
  predictions01 <- model1$fitted.values
  predictions02 <- model2$fitted.values
  predictions03 <- model3$fitted.values
  predictions04 <- model4$fitted.values
  
  MSE01 <- mean((training$ClaimNb - predictions01)^2)
  MSE02 <- mean((training01$ClaimAmount - predictions02)^2)
  MSE03 <- mean((training$ClaimNb - predictions03)^2)
  MSE04 <- mean((training01$ClaimAmount - predictions04)^2)
  
  # Evaluasi Testing:
  predictions1 <- predict(model1, testing, type="response")
  predictions2 <- predict(model2, testing, type="response")
  predictions3 <- predict(model3, testing, type="response")
  predictions4 <- predict(model4, testing, type="response")
  
  MSE1 <- mean((testing$ClaimNb - predictions1)^2)
  MSE2 <- mean((testing$ClaimAmount - predictions2)^2)
  MSE3 <- mean((testing$ClaimNb - predictions3)^2)
  MSE4 <- mean((testing$ClaimAmount - predictions4)^2)
  
  # MSE untuk setiap fold
  # training
  MSE01_list[i] <- MSE01
  MSE02_list[i] <- MSE02
  MSE03_list[i] <- MSE03
  MSE04_list[i] <- MSE04
  # testing
  MSE1_list[i] <- MSE1
  MSE2_list[i] <- MSE2
  MSE3_list[i] <- MSE3
  MSE4_list[i] <- MSE4
  
}

# Print hasil MSE:
MSE_result <- data.frame(
  Fold = 1:n,
  ClaimNb_GLM_Training = MSE01_list,
  ClaimNb_GAM_Training = MSE03_list,
  ClaimAmount_GLM_Training = MSE02_list,
  ClaimAmount_GAM_Training = MSE04_list,
  ClaimNb_GLM = MSE1_list,
  ClaimNb_GAM = MSE3_list,
  ClaimAmount_GLM = MSE2_list,
  ClaimAmount_GAM = MSE4_list
)
MSE_result

