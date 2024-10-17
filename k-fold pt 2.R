
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


# Variasi Data
library(moments)

# Data 1
Index_1 <- which(folds == 1, arr.ind=TRUE)
testing_1 <- data[Index_1, ]
training_1 <- data[-Index_1, ]
training_01 <- subset(training_1, ClaimAmount != 0)

a1 <- as.data.frame(table(training_1$ClaimNb))
b1 <- as.data.frame(table(testing_1$ClaimNb))

# summary: min, 1st qu., median, mean, 3rd qu., max
# IQR (interquartile range): q3-q1 (selisih antara persentil 75 dan persentil 25)
# Menghitung skewness dan kurtosis untuk x1 dan x2
summary_CN1 <- summary(testing_1$ClaimNb)
summary_CO1 <- summary(testing_1$ClaimOcc)
summary_CA1 <- summary(testing_1$ClaimAmount)
skewness_CN1 <- skewness(testing_1$ClaimNb)
kurtosis_CN1 <- kurtosis(testing_1$ClaimNb)
skewness_CO1 <- skewness(testing_1$ClaimOcc)
kurtosis_CO1 <- kurtosis(testing_1$ClaimOcc)
skewness_CA1 <- skewness(testing_1$ClaimAmount)
kurtosis_CA1 <- kurtosis(testing_1$ClaimAmount)
var_CN1 <- var(testing_1$ClaimNb)
var_CO1 <- var(testing_1$ClaimOcc)
var_CA1 <- var(testing_1$ClaimAmount)

summary.testing_1 <- data.frame(
  Statistic = c(names(summary_CN1), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN1, var_CN1, skewness_CN1, kurtosis_CN1),
  ClaimOcc = c(summary_CO1, var_CO1, skewness_CO1, kurtosis_CO1),
  ClaimAmount = c(summary_CA1, var_CA1, skewness_CA1, kurtosis_CA1)
)

summary_CN1 <- summary(training_1$ClaimNb)
summary_CO1 <- summary(training_1$ClaimOcc)
summary_CA1 <- summary(training_1$ClaimAmount)
skewness_CN1 <- skewness(training_1$ClaimNb)
kurtosis_CN1 <- kurtosis(training_1$ClaimNb)
skewness_CO1 <- skewness(training_1$ClaimOcc)
kurtosis_CO1 <- kurtosis(training_1$ClaimOcc)
skewness_CA1 <- skewness(training_1$ClaimAmount)
kurtosis_CA1 <- kurtosis(training_1$ClaimAmount)
var_CN1 <- var(training_1$ClaimNb)
var_CO1 <- var(training_1$ClaimOcc)
var_CA1 <- var(training_1$ClaimAmount)

summary.training_1 <- data.frame(
  Statistic = c(names(summary_CN1), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN1, var_CN1, skewness_CN1, kurtosis_CN1),
  ClaimOcc = c(summary_CO1, var_CO1, skewness_CO1, kurtosis_CO1),
  ClaimAmount = c(summary_CA1, var_CA1, skewness_CA1, kurtosis_CA1)
)

summary_CN1 <- summary(training_01$ClaimNb)
summary_CO1 <- summary(training_01$ClaimOcc)
summary_CA1 <- summary(training_01$ClaimAmount)
skewness_CN1 <- skewness(training_01$ClaimNb)
kurtosis_CN1 <- kurtosis(training_01$ClaimNb)
skewness_CO1 <- skewness(training_01$ClaimOcc)
kurtosis_CO1 <- kurtosis(training_01$ClaimOcc)
skewness_CA1 <- skewness(training_01$ClaimAmount)
kurtosis_CA1 <- kurtosis(training_01$ClaimAmount)
var_CN1 <- var(training_01$ClaimNb)
var_CO1 <- var(training_01$ClaimOcc)
var_CA1 <- var(training_01$ClaimAmount)

summary.training_01 <- data.frame(
  Statistic = c(names(summary_CN1), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN1, var_CN1, skewness_CN1, kurtosis_CN1),
  ClaimOcc = c(summary_CO1, var_CO1, skewness_CO1, kurtosis_CO1),
  ClaimAmount = c(summary_CA1, var_CA1, skewness_CA1, kurtosis_CA1)
)

data1 <- cbind(summary.testing_1,summary.training_1,summary.training_01)

# Data 2
Index_2 <- which(folds == 2, arr.ind=TRUE)
testing_2 <- data[Index_2, ]
training_2 <- data[-Index_2, ]
training_02 <- subset(training_2, ClaimAmount != 0)

a2 <- as.data.frame(table(training_2$ClaimNb))
b2 <- as.data.frame(table(testing_2$ClaimNb))

# summary: min, 1st qu., median, mean, 3rd qu., max
# IQR (interquartile range): q3-q1 (selisih antara persentil 75 dan persentil 25)
# Menghitung skewness dan kurtosis untuk x1 dan x2
summary_CN2 <- summary(testing_2$ClaimNb)
summary_CO2 <- summary(testing_2$ClaimOcc)
summary_CA2 <- summary(testing_2$ClaimAmount)
skewness_CN2 <- skewness(testing_2$ClaimNb)
kurtosis_CN2 <- kurtosis(testing_2$ClaimNb)
skewness_CO2 <- skewness(testing_2$ClaimOcc)
kurtosis_CO2 <- kurtosis(testing_2$ClaimOcc)
skewness_CA2 <- skewness(testing_2$ClaimAmount)
kurtosis_CA2 <- kurtosis(testing_2$ClaimAmount)
var_CN2 <- var(testing_2$ClaimNb)
var_CO2 <- var(testing_2$ClaimOcc)
var_CA2 <- var(testing_2$ClaimAmount)

summary.testing_2 <- data.frame(
  Statistic = c(names(summary_CN2), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN2, var_CN2, skewness_CN2, kurtosis_CN2),
  ClaimOcc = c(summary_CO2, var_CO2, skewness_CO2, kurtosis_CO2),
  ClaimAmount = c(summary_CA2, var_CA2, skewness_CA2, kurtosis_CA2)
)

summary_CN2 <- summary(training_2$ClaimNb)
summary_CO2 <- summary(training_2$ClaimOcc)
summary_CA2 <- summary(training_2$ClaimAmount)
skewness_CN2 <- skewness(training_2$ClaimNb)
kurtosis_CN2 <- kurtosis(training_2$ClaimNb)
skewness_CO2 <- skewness(training_2$ClaimOcc)
kurtosis_CO2 <- kurtosis(training_2$ClaimOcc)
skewness_CA2 <- skewness(training_2$ClaimAmount)
kurtosis_CA2 <- kurtosis(training_2$ClaimAmount)
var_CN2 <- var(training_2$ClaimNb)
var_CO2 <- var(training_2$ClaimOcc)
var_CA2 <- var(training_2$ClaimAmount)

summary.training_2 <- data.frame(
  Statistic = c(names(summary_CN2), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN2, var_CN2, skewness_CN2, kurtosis_CN2),
  ClaimOcc = c(summary_CO2, var_CO2, skewness_CO2, kurtosis_CO2),
  ClaimAmount = c(summary_CA2, var_CA2, skewness_CA2, kurtosis_CA2)
)

summary_CN2 <- summary(training_02$ClaimNb)
summary_CO2 <- summary(training_02$ClaimOcc)
summary_CA2 <- summary(training_02$ClaimAmount)
skewness_CN2 <- skewness(training_02$ClaimNb)
kurtosis_CN2 <- kurtosis(training_02$ClaimNb)
skewness_CO2 <- skewness(training_02$ClaimOcc)
kurtosis_CO2 <- kurtosis(training_02$ClaimOcc)
skewness_CA2 <- skewness(training_02$ClaimAmount)
kurtosis_CA2 <- kurtosis(training_02$ClaimAmount)
var_CN2 <- var(training_02$ClaimNb)
var_CO2 <- var(training_02$ClaimOcc)
var_CA2 <- var(training_02$ClaimAmount)

summary.training_02 <- data.frame(
  Statistic = c(names(summary_CN2), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN2, var_CN2, skewness_CN2, kurtosis_CN2),
  ClaimOcc = c(summary_CO2, var_CO2, skewness_CO2, kurtosis_CO2),
  ClaimAmount = c(summary_CA2, var_CA2, skewness_CA2, kurtosis_CA2)
)

data2 <- cbind(summary.testing_2,summary.training_2,summary.training_02)

# Data 3
Index_3 <- which(folds == 3, arr.ind=TRUE)
testing_3 <- data[Index_3, ]
training_3 <- data[-Index_3, ]
training_03 <- subset(training_3, ClaimAmount != 0)

a3 <- as.data.frame(table(training_3$ClaimNb))
b3 <- as.data.frame(table(testing_3$ClaimNb))

# summary: min, 1st qu., median, mean, 3rd qu., max
# IQR (interquartile range): q3-q1 (selisih antara persentil 75 dan persentil 25)
# Menghitung skewness dan kurtosis untuk x1 dan x2
summary_CN3 <- summary(testing_3$ClaimNb)
summary_CO3 <- summary(testing_3$ClaimOcc)
summary_CA3 <- summary(testing_3$ClaimAmount)
skewness_CN3 <- skewness(testing_3$ClaimNb)
kurtosis_CN3 <- kurtosis(testing_3$ClaimNb)
skewness_CO3 <- skewness(testing_3$ClaimOcc)
kurtosis_CO3 <- kurtosis(testing_3$ClaimOcc)
skewness_CA3 <- skewness(testing_3$ClaimAmount)
kurtosis_CA3 <- kurtosis(testing_3$ClaimAmount)
var_CN3 <- var(testing_3$ClaimNb)
var_CO3 <- var(testing_3$ClaimOcc)
var_CA3 <- var(testing_3$ClaimAmount)

summary.testing_3 <- data.frame(
  Statistic = c(names(summary_CN3), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN3, var_CN3, skewness_CN3, kurtosis_CN3),
  ClaimOcc = c(summary_CO3, var_CO3, skewness_CO3, kurtosis_CO3),
  ClaimAmount = c(summary_CA3, var_CA3, skewness_CA3, kurtosis_CA3)
)

summary_CN3 <- summary(training_3$ClaimNb)
summary_CO3 <- summary(training_3$ClaimOcc)
summary_CA3 <- summary(training_3$ClaimAmount)
skewness_CN3 <- skewness(training_3$ClaimNb)
kurtosis_CN3 <- kurtosis(training_3$ClaimNb)
skewness_CO3 <- skewness(training_3$ClaimOcc)
kurtosis_CO3 <- kurtosis(training_3$ClaimOcc)
skewness_CA3 <- skewness(training_3$ClaimAmount)
kurtosis_CA3 <- kurtosis(training_3$ClaimAmount)
var_CN3 <- var(training_3$ClaimNb)
var_CO3 <- var(training_3$ClaimOcc)
var_CA3 <- var(training_3$ClaimAmount)

summary.training_3 <- data.frame(
  Statistic = c(names(summary_CN3), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN3, var_CN3, skewness_CN3, kurtosis_CN3),
  ClaimOcc = c(summary_CO3, var_CO3, skewness_CO3, kurtosis_CO3),
  ClaimAmount = c(summary_CA3, var_CA3, skewness_CA3, kurtosis_CA3)
)

summary_CN3 <- summary(training_03$ClaimNb)
summary_CO3 <- summary(training_03$ClaimOcc)
summary_CA3 <- summary(training_03$ClaimAmount)
skewness_CN3 <- skewness(training_03$ClaimNb)
kurtosis_CN3 <- kurtosis(training_03$ClaimNb)
skewness_CO3 <- skewness(training_03$ClaimOcc)
kurtosis_CO3 <- kurtosis(training_03$ClaimOcc)
skewness_CA3 <- skewness(training_03$ClaimAmount)
kurtosis_CA3 <- kurtosis(training_03$ClaimAmount)
var_CN3 <- var(training_03$ClaimNb)
var_CO3 <- var(training_03$ClaimOcc)
var_CA3 <- var(training_03$ClaimAmount)

summary.training_03 <- data.frame(
  Statistic = c(names(summary_CN3), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN3, var_CN3, skewness_CN3, kurtosis_CN3),
  ClaimOcc = c(summary_CO3, var_CO3, skewness_CO3, kurtosis_CO3),
  ClaimAmount = c(summary_CA3, var_CA3, skewness_CA3, kurtosis_CA3)
)

data3 <- cbind(summary.testing_3,summary.training_3,summary.training_03)

# Data 4
Index_4 <- which(folds == 4, arr.ind=TRUE)
testing_4 <- data[Index_4, ]
training_4 <- data[-Index_4, ]
training_04 <- subset(training_4, ClaimAmount != 0)

a4 <- as.data.frame(table(training_4$ClaimNb))
b4 <- as.data.frame(table(testing_4$ClaimNb))

# summary: min, 1st qu., median, mean, 3rd qu., max
# IQR (interquartile range): q3-q1 (selisih antara persentil 75 dan persentil 25)
# Menghitung skewness dan kurtosis untuk x1 dan x2
summary_CN4 <- summary(testing_4$ClaimNb)
summary_CO4 <- summary(testing_4$ClaimOcc)
summary_CA4 <- summary(testing_4$ClaimAmount)
skewness_CN4 <- skewness(testing_4$ClaimNb)
kurtosis_CN4 <- kurtosis(testing_4$ClaimNb)
skewness_CO4 <- skewness(testing_4$ClaimOcc)
kurtosis_CO4 <- kurtosis(testing_4$ClaimOcc)
skewness_CA4 <- skewness(testing_4$ClaimAmount)
kurtosis_CA4 <- kurtosis(testing_4$ClaimAmount)
var_CN4 <- var(testing_4$ClaimNb)
var_CO4 <- var(testing_4$ClaimOcc)
var_CA4 <- var(testing_4$ClaimAmount)

summary.testing_4 <- data.frame(
  Statistic = c(names(summary_CN4), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN4, var_CN4, skewness_CN4, kurtosis_CN4),
  ClaimOcc = c(summary_CO4, var_CO4, skewness_CO4, kurtosis_CO4),
  ClaimAmount = c(summary_CA4, var_CA4, skewness_CA4, kurtosis_CA4)
)

summary_CN4 <- summary(training_4$ClaimNb)
summary_CO4 <- summary(training_4$ClaimOcc)
summary_CA4 <- summary(training_4$ClaimAmount)
skewness_CN4 <- skewness(training_4$ClaimNb)
kurtosis_CN4 <- kurtosis(training_4$ClaimNb)
skewness_CO4 <- skewness(training_4$ClaimOcc)
kurtosis_CO4 <- kurtosis(training_4$ClaimOcc)
skewness_CA4 <- skewness(training_4$ClaimAmount)
kurtosis_CA4 <- kurtosis(training_4$ClaimAmount)
var_CN4 <- var(training_4$ClaimNb)
var_CO4 <- var(training_4$ClaimOcc)
var_CA4 <- var(training_4$ClaimAmount)

summary.training_4 <- data.frame(
  Statistic = c(names(summary_CN4), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN4, var_CN4, skewness_CN4, kurtosis_CN4),
  ClaimOcc = c(summary_CO4, var_CO4, skewness_CO4, kurtosis_CO4),
  ClaimAmount = c(summary_CA4, var_CA4, skewness_CA4, kurtosis_CA4)
)

summary_CN4 <- summary(training_04$ClaimNb)
summary_CO4 <- summary(training_04$ClaimOcc)
summary_CA4 <- summary(training_04$ClaimAmount)
skewness_CN4 <- skewness(training_04$ClaimNb)
kurtosis_CN4 <- kurtosis(training_04$ClaimNb)
skewness_CO4 <- skewness(training_04$ClaimOcc)
kurtosis_CO4 <- kurtosis(training_04$ClaimOcc)
skewness_CA4 <- skewness(training_04$ClaimAmount)
kurtosis_CA4 <- kurtosis(training_04$ClaimAmount)
var_CN4 <- var(training_04$ClaimNb)
var_CO4 <- var(training_04$ClaimOcc)
var_CA4 <- var(training_04$ClaimAmount)

summary.training_04 <- data.frame(
  Statistic = c(names(summary_CN4), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN4, var_CN4, skewness_CN4, kurtosis_CN4),
  ClaimOcc = c(summary_CO4, var_CO4, skewness_CO4, kurtosis_CO4),
  ClaimAmount = c(summary_CA4, var_CA4, skewness_CA4, kurtosis_CA4)
)

data4 <- cbind(summary.testing_4,summary.training_4,summary.training_04)

# Data 5
Index_5 <- which(folds == 5, arr.ind=TRUE)
testing_5 <- data[Index_5, ]
training_5 <- data[-Index_5, ]
training_05 <- subset(training_5, ClaimAmount != 0)

a5 <- as.data.frame(table(training_5$ClaimNb))
b5 <- as.data.frame(table(testing_5$ClaimNb))

# summary: min, 1st qu., median, mean, 3rd qu., max
# IQR (interquartile range): q3-q1 (selisih antara persentil 75 dan persentil 25)
# Menghitung skewness dan kurtosis untuk x1 dan x2
summary_CN5 <- summary(testing_5$ClaimNb)
summary_CO5 <- summary(testing_5$ClaimOcc)
summary_CA5 <- summary(testing_5$ClaimAmount)
skewness_CN5 <- skewness(testing_5$ClaimNb)
kurtosis_CN5 <- kurtosis(testing_5$ClaimNb)
skewness_CO5 <- skewness(testing_5$ClaimOcc)
kurtosis_CO5 <- kurtosis(testing_5$ClaimOcc)
skewness_CA5 <- skewness(testing_5$ClaimAmount)
kurtosis_CA5 <- kurtosis(testing_5$ClaimAmount)
var_CN5 <- var(testing_5$ClaimNb)
var_CO5 <- var(testing_5$ClaimOcc)
var_CA5 <- var(testing_5$ClaimAmount)

summary.testing_5 <- data.frame(
  Statistic = c(names(summary_CN5), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN5, var_CN5, skewness_CN5, kurtosis_CN5),
  ClaimOcc = c(summary_CO5, var_CO5, skewness_CO5, kurtosis_CO5),
  ClaimAmount = c(summary_CA5, var_CA5, skewness_CA5, kurtosis_CA5)
)

summary_CN5 <- summary(training_5$ClaimNb)
summary_CO5 <- summary(training_5$ClaimOcc)
summary_CA5 <- summary(training_5$ClaimAmount)
skewness_CN5 <- skewness(training_5$ClaimNb)
kurtosis_CN5 <- kurtosis(training_5$ClaimNb)
skewness_CO5 <- skewness(training_5$ClaimOcc)
kurtosis_CO5 <- kurtosis(training_5$ClaimOcc)
skewness_CA5 <- skewness(training_5$ClaimAmount)
kurtosis_CA5 <- kurtosis(training_5$ClaimAmount)
var_CN5 <- var(training_5$ClaimNb)
var_CO5 <- var(training_5$ClaimOcc)
var_CA5 <- var(training_5$ClaimAmount)

summary.training_5 <- data.frame(
  Statistic = c(names(summary_CN5), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN5, var_CN5, skewness_CN5, kurtosis_CN5),
  ClaimOcc = c(summary_CO5, var_CO5, skewness_CO5, kurtosis_CO5),
  ClaimAmount = c(summary_CA5, var_CA5, skewness_CA5, kurtosis_CA5)
)

summary_CN5 <- summary(training_05$ClaimNb)
summary_CO5 <- summary(training_05$ClaimOcc)
summary_CA5 <- summary(training_05$ClaimAmount)
skewness_CN5 <- skewness(training_05$ClaimNb)
kurtosis_CN5 <- kurtosis(training_05$ClaimNb)
skewness_CO5 <- skewness(training_05$ClaimOcc)
kurtosis_CO5 <- kurtosis(training_05$ClaimOcc)
skewness_CA5 <- skewness(training_05$ClaimAmount)
kurtosis_CA5 <- kurtosis(training_05$ClaimAmount)
var_CN5 <- var(training_05$ClaimNb)
var_CO5 <- var(training_05$ClaimOcc)
var_CA5 <- var(training_05$ClaimAmount)

summary.training_05 <- data.frame(
  Statistic = c(names(summary_CN5), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN5, var_CN5, skewness_CN5, kurtosis_CN5),
  ClaimOcc = c(summary_CO5, var_CO5, skewness_CO5, kurtosis_CO5),
  ClaimAmount = c(summary_CA5, var_CA5, skewness_CA5, kurtosis_CA5)
)

data5 <- cbind(summary.testing_5,summary.training_5,summary.training_05)

# Data 6
Index_6 <- which(folds == 6, arr.ind=TRUE)
testing_6 <- data[Index_6, ]
training_6 <- data[-Index_6, ]
training_06 <- subset(training_6, ClaimAmount != 0)

a6 <- as.data.frame(table(training_6$ClaimNb))
b6 <- as.data.frame(table(testing_6$ClaimNb))

# summary: min, 1st qu., median, mean, 3rd qu., max
# IQR (interquartile range): q3-q1 (selisih antara persentil 75 dan persentil 25)
# Menghitung skewness dan kurtosis untuk x1 dan x2
summary_CN6 <- summary(testing_6$ClaimNb)
summary_CO6 <- summary(testing_6$ClaimOcc)
summary_CA6 <- summary(testing_6$ClaimAmount)
skewness_CN6 <- skewness(testing_6$ClaimNb)
kurtosis_CN6 <- kurtosis(testing_6$ClaimNb)
skewness_CO6 <- skewness(testing_6$ClaimOcc)
kurtosis_CO6 <- kurtosis(testing_6$ClaimOcc)
skewness_CA6 <- skewness(testing_6$ClaimAmount)
kurtosis_CA6 <- kurtosis(testing_6$ClaimAmount)
var_CN6 <- var(testing_6$ClaimNb)
var_CO6 <- var(testing_6$ClaimOcc)
var_CA6 <- var(testing_6$ClaimAmount)

summary.testing_6 <- data.frame(
  Statistic = c(names(summary_CN6), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN6, var_CN6, skewness_CN6, kurtosis_CN6),
  ClaimOcc = c(summary_CO6, var_CO6, skewness_CO6, kurtosis_CO6),
  ClaimAmount = c(summary_CA6, var_CA6, skewness_CA6, kurtosis_CA6)
)

summary_CN6 <- summary(training_6$ClaimNb)
summary_CO6 <- summary(training_6$ClaimOcc)
summary_CA6 <- summary(training_6$ClaimAmount)
skewness_CN6 <- skewness(training_6$ClaimNb)
kurtosis_CN6 <- kurtosis(training_6$ClaimNb)
skewness_CO6 <- skewness(training_6$ClaimOcc)
kurtosis_CO6 <- kurtosis(training_6$ClaimOcc)
skewness_CA6 <- skewness(training_6$ClaimAmount)
kurtosis_CA6 <- kurtosis(training_6$ClaimAmount)
var_CN6 <- var(training_6$ClaimNb)
var_CO6 <- var(training_6$ClaimOcc)
var_CA6 <- var(training_6$ClaimAmount)

summary.training_6 <- data.frame(
  Statistic = c(names(summary_CN6), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN6, var_CN6, skewness_CN6, kurtosis_CN6),
  ClaimOcc = c(summary_CO6, var_CO6, skewness_CO6, kurtosis_CO6),
  ClaimAmount = c(summary_CA6, var_CA6, skewness_CA6, kurtosis_CA6)
)

summary_CN6 <- summary(training_06$ClaimNb)
summary_CO6 <- summary(training_06$ClaimOcc)
summary_CA6 <- summary(training_06$ClaimAmount)
skewness_CN6 <- skewness(training_06$ClaimNb)
kurtosis_CN6 <- kurtosis(training_06$ClaimNb)
skewness_CO6 <- skewness(training_06$ClaimOcc)
kurtosis_CO6 <- kurtosis(training_06$ClaimOcc)
skewness_CA6 <- skewness(training_06$ClaimAmount)
kurtosis_CA6 <- kurtosis(training_06$ClaimAmount)
var_CN6 <- var(training_06$ClaimNb)
var_CO6 <- var(training_06$ClaimOcc)
var_CA6 <- var(training_06$ClaimAmount)

summary.training_06 <- data.frame(
  Statistic = c(names(summary_CN6), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN6, var_CN6, skewness_CN6, kurtosis_CN6),
  ClaimOcc = c(summary_CO6, var_CO6, skewness_CO6, kurtosis_CO6),
  ClaimAmount = c(summary_CA6, var_CA6, skewness_CA6, kurtosis_CA6)
)

data6 <- cbind(summary.testing_6,summary.training_6,summary.training_06)

# Data 7
Index_7 <- which(folds == 7, arr.ind=TRUE)
testing_7 <- data[Index_7, ]
training_7 <- data[-Index_7, ]
training_07 <- subset(training_7, ClaimAmount != 0)

a7 <- as.data.frame(table(training_7$ClaimNb))
b7 <- as.data.frame(table(testing_7$ClaimNb))

# summary: min, 1st qu., median, mean, 3rd qu., max
# IQR (interquartile range): q3-q1 (selisih antara persentil 75 dan persentil 25)
# Menghitung skewness dan kurtosis untuk x1 dan x2
summary_CN7 <- summary(testing_7$ClaimNb)
summary_CO7 <- summary(testing_7$ClaimOcc)
summary_CA7 <- summary(testing_7$ClaimAmount)
skewness_CN7 <- skewness(testing_7$ClaimNb)
kurtosis_CN7 <- kurtosis(testing_7$ClaimNb)
skewness_CO7 <- skewness(testing_7$ClaimOcc)
kurtosis_CO7 <- kurtosis(testing_7$ClaimOcc)
skewness_CA7 <- skewness(testing_7$ClaimAmount)
kurtosis_CA7 <- kurtosis(testing_7$ClaimAmount)
var_CN7 <- var(testing_7$ClaimNb)
var_CO7 <- var(testing_7$ClaimOcc)
var_CA7 <- var(testing_7$ClaimAmount)

summary.testing_7 <- data.frame(
  Statistic = c(names(summary_CN7), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN7, var_CN7, skewness_CN7, kurtosis_CN7),
  ClaimOcc = c(summary_CO7, var_CO7, skewness_CO7, kurtosis_CO7),
  ClaimAmount = c(summary_CA7, var_CA7, skewness_CA7, kurtosis_CA7)
)

summary_CN7 <- summary(training_7$ClaimNb)
summary_CO7 <- summary(training_7$ClaimOcc)
summary_CA7 <- summary(training_7$ClaimAmount)
skewness_CN7 <- skewness(training_7$ClaimNb)
kurtosis_CN7 <- kurtosis(training_7$ClaimNb)
skewness_CO7 <- skewness(training_7$ClaimOcc)
kurtosis_CO7 <- kurtosis(training_7$ClaimOcc)
skewness_CA7 <- skewness(training_7$ClaimAmount)
kurtosis_CA7 <- kurtosis(training_7$ClaimAmount)
var_CN7 <- var(training_7$ClaimNb)
var_CO7 <- var(training_7$ClaimOcc)
var_CA7 <- var(training_7$ClaimAmount)

summary.training_7 <- data.frame(
  Statistic = c(names(summary_CN7), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN7, var_CN7, skewness_CN7, kurtosis_CN7),
  ClaimOcc = c(summary_CO7, var_CO7, skewness_CO7, kurtosis_CO7),
  ClaimAmount = c(summary_CA7, var_CA7, skewness_CA7, kurtosis_CA7)
)

summary_CN7 <- summary(training_07$ClaimNb)
summary_CO7 <- summary(training_07$ClaimOcc)
summary_CA7 <- summary(training_07$ClaimAmount)
skewness_CN7 <- skewness(training_07$ClaimNb)
kurtosis_CN7 <- kurtosis(training_07$ClaimNb)
skewness_CO7 <- skewness(training_07$ClaimOcc)
kurtosis_CO7 <- kurtosis(training_07$ClaimOcc)
skewness_CA7 <- skewness(training_07$ClaimAmount)
kurtosis_CA7 <- kurtosis(training_07$ClaimAmount)
var_CN7 <- var(training_07$ClaimNb)
var_CO7 <- var(training_07$ClaimOcc)
var_CA7 <- var(training_07$ClaimAmount)

summary.training_07 <- data.frame(
  Statistic = c(names(summary_CN7), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN7, var_CN7, skewness_CN7, kurtosis_CN7),
  ClaimOcc = c(summary_CO7, var_CO7, skewness_CO7, kurtosis_CO7),
  ClaimAmount = c(summary_CA7, var_CA7, skewness_CA7, kurtosis_CA7)
)

data7 <- cbind(summary.testing_7,summary.training_7,summary.training_07)

# Data 8
Index_8 <- which(folds == 8, arr.ind=TRUE)
testing_8 <- data[Index_8, ]
training_8 <- data[-Index_8, ]
training_08 <- subset(training_8, ClaimAmount != 0)

a8 <- as.data.frame(table(training_8$ClaimNb))
b8 <- as.data.frame(table(testing_8$ClaimNb))

# summary: min, 1st qu., median, mean, 3rd qu., max
# IQR (interquartile range): q3-q1 (selisih antara persentil 75 dan persentil 25)
# Menghitung skewness dan kurtosis untuk x1 dan x2
summary_CN8 <- summary(testing_8$ClaimNb)
summary_CO8 <- summary(testing_8$ClaimOcc)
summary_CA8 <- summary(testing_8$ClaimAmount)
skewness_CN8 <- skewness(testing_8$ClaimNb)
kurtosis_CN8 <- kurtosis(testing_8$ClaimNb)
skewness_CO8 <- skewness(testing_8$ClaimOcc)
kurtosis_CO8 <- kurtosis(testing_8$ClaimOcc)
skewness_CA8 <- skewness(testing_8$ClaimAmount)
kurtosis_CA8 <- kurtosis(testing_8$ClaimAmount)
var_CN8 <- var(testing_8$ClaimNb)
var_CO8 <- var(testing_8$ClaimOcc)
var_CA8 <- var(testing_8$ClaimAmount)

summary.testing_8 <- data.frame(
  Statistic = c(names(summary_CN8), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN8, var_CN8, skewness_CN8, kurtosis_CN8),
  ClaimOcc = c(summary_CO8, var_CO8, skewness_CO8, kurtosis_CO8),
  ClaimAmount = c(summary_CA8, var_CA8, skewness_CA8, kurtosis_CA8)
)

summary_CN8 <- summary(training_8$ClaimNb)
summary_CO8 <- summary(training_8$ClaimOcc)
summary_CA8 <- summary(training_8$ClaimAmount)
skewness_CN8 <- skewness(training_8$ClaimNb)
kurtosis_CN8 <- kurtosis(training_8$ClaimNb)
skewness_CO8 <- skewness(training_8$ClaimOcc)
kurtosis_CO8 <- kurtosis(training_8$ClaimOcc)
skewness_CA8 <- skewness(training_8$ClaimAmount)
kurtosis_CA8 <- kurtosis(training_8$ClaimAmount)
var_CN8 <- var(training_8$ClaimNb)
var_CO8 <- var(training_8$ClaimOcc)
var_CA8 <- var(training_8$ClaimAmount)

summary.training_8 <- data.frame(
  Statistic = c(names(summary_CN8), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN8, var_CN8, skewness_CN8, kurtosis_CN8),
  ClaimOcc = c(summary_CO8, var_CO8, skewness_CO8, kurtosis_CO8),
  ClaimAmount = c(summary_CA8, var_CA8, skewness_CA8, kurtosis_CA8)
)

summary_CN8 <- summary(training_08$ClaimNb)
summary_CO8 <- summary(training_08$ClaimOcc)
summary_CA8 <- summary(training_08$ClaimAmount)
skewness_CN8 <- skewness(training_08$ClaimNb)
kurtosis_CN8 <- kurtosis(training_08$ClaimNb)
skewness_CO8 <- skewness(training_08$ClaimOcc)
kurtosis_CO8 <- kurtosis(training_08$ClaimOcc)
skewness_CA8 <- skewness(training_08$ClaimAmount)
kurtosis_CA8 <- kurtosis(training_08$ClaimAmount)
var_CN8 <- var(training_08$ClaimNb)
var_CO8 <- var(training_08$ClaimOcc)
var_CA8 <- var(training_08$ClaimAmount)

summary.training_08 <- data.frame(
  Statistic = c(names(summary_CN8), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN8, var_CN8, skewness_CN8, kurtosis_CN8),
  ClaimOcc = c(summary_CO8, var_CO8, skewness_CO8, kurtosis_CO8),
  ClaimAmount = c(summary_CA8, var_CA8, skewness_CA8, kurtosis_CA8)
)

data8 <- cbind(summary.testing_8,summary.training_8,summary.training_08)

# Data 9 
Index_9 <- which(folds == 9, arr.ind=TRUE)
testing_9 <- data[Index_9, ]
training_9 <- data[-Index_9, ]
training_09 <- subset(training_9, ClaimAmount != 0)

a9 <- as.data.frame(table(training_9$ClaimNb))
b9 <- as.data.frame(table(testing_9$ClaimNb))

# summary: min, 1st qu., median, mean, 3rd qu., max
# IQR (interquartile range): q3-q1 (selisih antara persentil 75 dan persentil 25)
# Menghitung skewness dan kurtosis untuk x1 dan x2
summary_CN9 <- summary(testing_9$ClaimNb)
summary_CO9 <- summary(testing_9$ClaimOcc)
summary_CA9 <- summary(testing_9$ClaimAmount)
skewness_CN9 <- skewness(testing_9$ClaimNb)
kurtosis_CN9 <- kurtosis(testing_9$ClaimNb)
skewness_CO9 <- skewness(testing_9$ClaimOcc)
kurtosis_CO9 <- kurtosis(testing_9$ClaimOcc)
skewness_CA9 <- skewness(testing_9$ClaimAmount)
kurtosis_CA9 <- kurtosis(testing_9$ClaimAmount)
var_CN9 <- var(testing_9$ClaimNb)
var_CO9 <- var(testing_9$ClaimOcc)
var_CA9 <- var(testing_9$ClaimAmount)

summary.testing_9 <- data.frame(
  Statistic = c(names(summary_CN9), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN9, var_CN9, skewness_CN9, kurtosis_CN9),
  ClaimOcc = c(summary_CO9, var_CO9, skewness_CO9, kurtosis_CO9),
  ClaimAmount = c(summary_CA9, var_CA9, skewness_CA9, kurtosis_CA9)
)

summary_CN9 <- summary(training_9$ClaimNb)
summary_CO9 <- summary(training_9$ClaimOcc)
summary_CA9 <- summary(training_9$ClaimAmount)
skewness_CN9 <- skewness(training_9$ClaimNb)
kurtosis_CN9 <- kurtosis(training_9$ClaimNb)
skewness_CO9 <- skewness(training_9$ClaimOcc)
kurtosis_CO9 <- kurtosis(training_9$ClaimOcc)
skewness_CA9 <- skewness(training_9$ClaimAmount)
kurtosis_CA9 <- kurtosis(training_9$ClaimAmount)
var_CN9 <- var(training_9$ClaimNb)
var_CO9 <- var(training_9$ClaimOcc)
var_CA9 <- var(training_9$ClaimAmount)

summary.training_9 <- data.frame(
  Statistic = c(names(summary_CN9), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN9, var_CN9, skewness_CN9, kurtosis_CN9),
  ClaimOcc = c(summary_CO9, var_CO9, skewness_CO9, kurtosis_CO9),
  ClaimAmount = c(summary_CA9, var_CA9, skewness_CA9, kurtosis_CA9)
)

summary_CN9 <- summary(training_09$ClaimNb)
summary_CO9 <- summary(training_09$ClaimOcc)
summary_CA9 <- summary(training_09$ClaimAmount)
skewness_CN9 <- skewness(training_09$ClaimNb)
kurtosis_CN9 <- kurtosis(training_09$ClaimNb)
skewness_CO9 <- skewness(training_09$ClaimOcc)
kurtosis_CO9 <- kurtosis(training_09$ClaimOcc)
skewness_CA9 <- skewness(training_09$ClaimAmount)
kurtosis_CA9 <- kurtosis(training_09$ClaimAmount)
var_CN9 <- var(training_09$ClaimNb)
var_CO9 <- var(training_09$ClaimOcc)
var_CA9 <- var(training_09$ClaimAmount)

summary.training_09 <- data.frame(
  Statistic = c(names(summary_CN9), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN9, var_CN9, skewness_CN9, kurtosis_CN9),
  ClaimOcc = c(summary_CO9, var_CO9, skewness_CO9, kurtosis_CO9),
  ClaimAmount = c(summary_CA9, var_CA9, skewness_CA9, kurtosis_CA9)
)

data9 <- cbind(summary.testing_9,summary.training_9,summary.training_09)

# Data 10
Index_10 <- which(folds == 10, arr.ind=TRUE)
testing_10 <- data[Index_10, ]
training_10 <- data[-Index_10, ]
training_010 <- subset(training_10, ClaimAmount != 0)


a10 <- as.data.frame(table(training_10$ClaimNb))
b10 <- as.data.frame(table(testing_10$ClaimNb))

# summary: min, 1st qu., median, mean, 3rd qu., max
# IQR (interquartile range): q3-q1 (selisih antara persentil 75 dan persentil 25)
# Menghitung skewness dan kurtosis untuk x1 dan x2
summary_CN10 <- summary(testing_10$ClaimNb)
summary_CO10 <- summary(testing_10$ClaimOcc)
summary_CA10 <- summary(testing_10$ClaimAmount)
skewness_CN10 <- skewness(testing_10$ClaimNb)
kurtosis_CN10 <- kurtosis(testing_10$ClaimNb)
skewness_CO10 <- skewness(testing_10$ClaimOcc)
kurtosis_CO10 <- kurtosis(testing_10$ClaimOcc)
skewness_CA10 <- skewness(testing_10$ClaimAmount)
kurtosis_CA10 <- kurtosis(testing_10$ClaimAmount)
var_CN10 <- var(testing_10$ClaimNb)
var_CO10 <- var(testing_10$ClaimOcc)
var_CA10 <- var(testing_10$ClaimAmount)

summary.testing_10 <- data.frame(
  Statistic = c(names(summary_CN10), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN10, var_CN10, skewness_CN10, kurtosis_CN10),
  ClaimOcc = c(summary_CO10, var_CO10, skewness_CO10, kurtosis_CO10),
  ClaimAmount = c(summary_CA10, var_CA10, skewness_CA10, kurtosis_CA10)
)

summary_CN10 <- summary(training_10$ClaimNb)
summary_CO10 <- summary(training_10$ClaimOcc)
summary_CA10 <- summary(training_10$ClaimAmount)
skewness_CN10 <- skewness(training_10$ClaimNb)
kurtosis_CN10 <- kurtosis(training_10$ClaimNb)
skewness_CO10 <- skewness(training_10$ClaimOcc)
kurtosis_CO10 <- kurtosis(training_10$ClaimOcc)
skewness_CA10 <- skewness(training_10$ClaimAmount)
kurtosis_CA10 <- kurtosis(training_10$ClaimAmount)
var_CN10 <- var(training_10$ClaimNb)
var_CO10 <- var(training_10$ClaimOcc)
var_CA10 <- var(training_10$ClaimAmount)

summary.training_10 <- data.frame(
  Statistic = c(names(summary_CN10), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN10, var_CN10, skewness_CN10, kurtosis_CN10),
  ClaimOcc = c(summary_CO10, var_CO10, skewness_CO10, kurtosis_CO10),
  ClaimAmount = c(summary_CA10, var_CA10, skewness_CA10, kurtosis_CA10)
)

summary_CN10 <- summary(training_010$ClaimNb)
summary_CO10 <- summary(training_010$ClaimOcc)
summary_CA10 <- summary(training_010$ClaimAmount)
skewness_CN10 <- skewness(training_010$ClaimNb)
kurtosis_CN10 <- kurtosis(training_010$ClaimNb)
skewness_CO10 <- skewness(training_010$ClaimOcc)
kurtosis_CO10 <- kurtosis(training_010$ClaimOcc)
skewness_CA10 <- skewness(training_010$ClaimAmount)
kurtosis_CA10 <- kurtosis(training_010$ClaimAmount)
var_CN10 <- var(training_010$ClaimNb)
var_CO10 <- var(training_010$ClaimOcc)
var_CA10 <- var(training_010$ClaimAmount)

summary.training_010 <- data.frame(
  Statistic = c(names(summary_CN10), "variance", "Skewness", "Kurtosis"),
  ClaimNb = c(summary_CN10, var_CN10, skewness_CN10, kurtosis_CN10),
  ClaimOcc = c(summary_CO10, var_CO10, skewness_CO10, kurtosis_CO10),
  ClaimAmount = c(summary_CA10, var_CA10, skewness_CA10, kurtosis_CA10)
)

data10 <- cbind(summary.testing_10,summary.training_10,summary.training_010)

# save hasil variasi data
library(openxlsx)
wb <- createWorkbook()

addWorksheet(wb, "data1")
writeData(wb, sheet = "data1", data1)
addWorksheet(wb, "data2")
writeData(wb, sheet = "data2", data2)
addWorksheet(wb, "data3")
writeData(wb, sheet = "data3", data3)
addWorksheet(wb, "data4")
writeData(wb, sheet = "data4", data4)
addWorksheet(wb, "data5")
writeData(wb, sheet = "data5", data5)
addWorksheet(wb, "data6")
writeData(wb, sheet = "data6", data6)
addWorksheet(wb, "data7")
writeData(wb, sheet = "data7", data7)
addWorksheet(wb, "data8")
writeData(wb, sheet = "data8", data8)
addWorksheet(wb, "data9")
writeData(wb, sheet = "data9", data9)
addWorksheet(wb, "data10")
writeData(wb, sheet = "data10", data10)

saveWorkbook(wb, "variasi.xlsx", overwrite = TRUE)


library(openxlsx)
wb <- createWorkbook()

addWorksheet(wb, "a1")
writeData(wb, sheet = "a1", a1)
addWorksheet(wb, "b1")
writeData(wb, sheet = "b1", b1)
addWorksheet(wb, "a2")
writeData(wb, sheet = "a2", a2)
addWorksheet(wb, "b2")
writeData(wb, sheet = "b2", b2)
addWorksheet(wb, "a3")
writeData(wb, sheet = "a3", a3)
addWorksheet(wb, "b3")
writeData(wb, sheet = "b3", b3)
addWorksheet(wb, "a4")
writeData(wb, sheet = "a4", a4)
addWorksheet(wb, "b4")
writeData(wb, sheet = "b4", b4)
addWorksheet(wb, "a5")
writeData(wb, sheet = "a5", a5)
addWorksheet(wb, "b5")
writeData(wb, sheet = "b5", b5)
addWorksheet(wb, "a6")
writeData(wb, sheet = "a6", a6)
addWorksheet(wb, "b6")
writeData(wb, sheet = "b6", b6)
addWorksheet(wb, "a7")
writeData(wb, sheet = "a7", a7)
addWorksheet(wb, "b7")
writeData(wb, sheet = "b7", b7)
addWorksheet(wb, "a8")
writeData(wb, sheet = "a8", a8)
addWorksheet(wb, "b8")
writeData(wb, sheet = "b8", b8)
addWorksheet(wb, "a9")
writeData(wb, sheet = "a9", a9)
addWorksheet(wb, "b9")
writeData(wb, sheet = "b9", b9)
addWorksheet(wb, "a10")
writeData(wb, sheet = "a10", a10)
addWorksheet(wb, "b10")
writeData(wb, sheet = "b10", b10)

saveWorkbook(wb, "variasi-frekuensi.xlsx", overwrite = TRUE)