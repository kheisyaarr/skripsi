
rm(list=ls())
library(Hmisc)
library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)

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

# Loss Modelling ----------------------------------------------------------

set.seed(598)  
# 80% data train dan 20% data test
training_index <- createDataPartition(data$ClaimNb, p = 0.8, list = FALSE)
training <- data[training_index, ] # untuk data frequency
testing <- data[-training_index, ]
training01 <- subset(training, ClaimAmount != 0) # untuk data severity
a = mean(training$ClaimAmount)
b = mean(testing$ClaimAmount)
abs(b-a) # 3.609945, untuk menunjukkan bahwa set.seed 598 cukup untuk digunakan

# Generalized Linear Models -----------------------------------------------

#####

# ClaimNb ~ Poisson
freqpoiglm = glm(ClaimNb~VehValue+VehAge+VehBody+Gender+DrivAge, family = poisson(link = "log"), data = training, offset = log(Exposure))
summary(freqpoiglm) # 27547
predpoiglm = predict(freqpoiglm, newdata = testing, type = "link") 
m_freqpoiglm = exp(predpoiglm) 
summary(m_freqpoiglm)

# ClaimNb ~ Negative Binomial
library(MASS)
freqnbglm = glm.nb(ClaimNb~VehValue+VehAge+VehBody+Gender+DrivAge+offset(log(Exposure)), link = log, data = training) 
summary(freqnbglm) # 27525
prednbglm = predict(freqnbglm, newdata = testing, type = "link") 
m_freqnbglm = exp(prednbglm)
summary(m_freqnbglm)

# ClaimOcc ~ Binomial
occbinglm = glm(ClaimOcc~VehValue+VehAge+VehBody+Gender+DrivAge, family = binomial(link = "logit"), data = training, offset = log(Exposure))
summary(occbinglm) # 25710
predbinglm = predict(occbinglm, newdata = testing, type = "link") 
m_occbinglm = exp(predbinglm)/(1+exp(predbinglm))
summary(m_occbinglm)

# IndividualClaim ~ Gamma, link log
igammglm = glm(IndividualClaim~VehValue+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training01)
summary(igammglm) # 62899
predigammglm = predict(igammglm, newdata = testing, type = "link") 
m_igammglm = exp(predigammglm)
summary(m_igammglm)

# IndividualClaim ~ Inverse Gaussian, link log
iinvglm = glm(IndividualClaim~VehValue+VehAge+VehBody+Gender+DrivAge, family = inverse.gaussian(link = "log"), data = training01)
summary(iinvglm) # 61077
prediinvglm = predict(iinvglm, newdata = testing, type = "link") 
m_iinvglm = exp(prediinvglm)
summary(m_iinvglm)

# ClaimAmount ~ Gamma, link log
cgammglm = glm(ClaimAmount~VehValue+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training01)
summary(cgammglm) # 63240
predcgammglm = predict(cgammglm, newdata = testing, type = "link") 
m_cgammglm = exp(predcgammglm)
summary(m_cgammglm)

# ClaimAmount ~ Inverse Gaussian, link log
cinvglm = glm(ClaimAmount~VehValue+VehAge+VehBody+Gender+DrivAge, family = inverse.gaussian(link = "log"), data = training01)
summary(cinvglm) # 61514
predcinvglm = predict(cinvglm, newdata = testing, type = "link") 
m_cinvglm = exp(predcinvglm)
summary(m_cinvglm)


# Generalized Additive Models ---------------------------------------------

#####

# install.packages("mgcv")
library(mgcv)
require(mgcv)

# ClaimNb ~ Poisson
freqpoigam = gam(ClaimNb~s(VehValue, bs="cr", k=10)+VehAge+VehBody+Gender+DrivAge, family = poisson(link = "log"), 
                 data = training, offset = log(Exposure), method="REML")
AIC(freqpoigam) # 27531.11
gam.check(freqpoigam)
summary(freqpoigam)
coef(freqpoigam)
plot.gam(freqpoigam, shade=TRUE, pages=1)
predpoigam = predict(freqpoigam, newdata = testing, type = "link") 
m_freqpoigam = exp(predpoigam)
summary(m_freqpoigam)

# ClaimNb ~ Negative Binomial
library(MASS)
freqnbgam = gam(ClaimNb~s(VehValue, bs="cr", k=10)+VehAge+VehBody+Gender+DrivAge, family = nb(link = "log"), 
                data = training, offset = log(Exposure), method="REML")
AIC(freqnbgam) # 27510.02
gam.check(freqnbgam)
summary(freqnbgam)
coef(freqnbgam)
plot.gam(freqnbgam, shade=TRUE, pages=1)
prednbgam = predict(freqnbgam, newdata = testing, type = "link") 
m_freqnbgam = exp(prednbgam)
summary(m_freqnbgam)

# ClaimOcc ~ Binomial
occbingam = gam(ClaimOcc~s(VehValue, bs="cr", k=10)+VehAge+VehBody+Gender+DrivAge, family = binomial(link = "logit"), data = training, offset = log(Exposure))
AIC(occbingam) # 25692.82
gam.check(occbingam)
summary(occbingam)
coef(occbingam)
plot.gam(occbingam, shade=TRUE, pages=1)
predbingam = predict(occbingam, newdata = testing, type = "link") 
m_occbingam = exp(predbingam)/(1+exp(predbingam))
summary(m_occbingam)

# IndividualClaim ~ Gamma, link log
igammgam = gam(IndividualClaim~s(VehValue, bs="cr", k=10)+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training01, method="REML")
AIC(igammgam) # 62820.32
gam.check(igammgam)
summary(igammgam)
coef(igammgam)
plot.gam(igammgam, shade=TRUE, pages=1)
predigammgam = predict(igammgam, newdata = testing, type = "link") 
m_igammgam = exp(predigammgam)
summary(m_igammgam)

# IndividualClaim ~ Inverse Gaussian, link log
iinvgam = gam(IndividualClaim~s(VehValue, bs="cr", k=10)+VehAge+VehBody+Gender+DrivAge, family = inverse.gaussian(link = "log"), data = training01, method="REML")
AIC(iinvgam) # 61101.36
gam.check(iinvgam)
summary(iinvgam)
coef(iinvgam)
plot.gam(iinvgam, shade=TRUE, pages=1)
prediinvgam = predict(iinvgam, newdata = testing, type = "link") 
m_iinvgam = exp(prediinvgam)
summary(m_iinvgam)

# ClaimAmount ~ Gamma, link log
cgammgam = gam(ClaimAmount~s(VehValue, bs="cr", k=10)+VehAge+VehBody+Gender+DrivAge, family = Gamma(link = "log"), data = training01, method="REML")
AIC(cgammgam) # 63163.34
gam.check(cgammgam)
summary(cgammgam)
coef(cgammgam)
plot.gam(cgammgam, shade=TRUE, pages=1)
predcgammgam = predict(cgammgam, newdata = testing, type = "link") 
m_cgammgam = exp(predcgammgam)
summary(m_cgammgam)

# ClaimAmount ~ Inverse Gaussian, link log
cinvgam = gam(ClaimAmount~s(VehValue, bs="cr", k=10)+VehAge+VehBody+Gender+DrivAge, family = inverse.gaussian(link = "log"), data = training01, method="REML")
AIC(cinvgam) # 61538.98
gam.check(cinvgam)
summary(cinvgam)
coef(cinvgam)
plot.gam(cinvgam, shade=TRUE, pages=1)
predcinvgam = predict(cinvgam, newdata = testing, type = "link") 
m_cinvgam = exp(predcinvgam)
summary(m_cinvgam)


# Evaluasi Model Kerugian -------------------------------------------------

(n <- nrow(testing)) # 13330

# GLM poi x GLM Gamma
PP1 = m_freqpoiglm*m_igammglm
MSE1 = mean((testing$ClaimAmount-PP1)^2)
RMSE1 = sqrt(MSE1)
MAE1 = sum(abs(testing$ClaimAmount-PP1))/n

# GLM poi x GLM Inverse Gaussian
PP2 = m_freqpoiglm*m_iinvglm
MSE2 = mean((testing$ClaimAmount-PP2)^2)
RMSE2 = sqrt(MSE2)
MAE2 = sum(abs(testing$ClaimAmount-PP2))/n

# GLM nb x GLM Gamma
PP4 = m_freqnbglm*m_igammglm
MSE4 = mean((testing$ClaimAmount-PP4)^2)
RMSE4 = sqrt(MSE4)
MAE4 = sum(abs(testing$ClaimAmount-PP4))/n

# GLM nb x GLM Inverse Gaussian
PP5 = m_freqnbglm*m_iinvglm
MSE5 = mean((testing$ClaimAmount-PP5)^2)
RMSE5 = sqrt(MSE5)
MAE5 = sum(abs(testing$ClaimAmount-PP5))/n

# GLM binomial x GLM Gamma
PP7 = m_occbinglm*m_cgammglm
MSE7 = mean((testing$ClaimAmount-PP7)^2)
RMSE7 = sqrt(MSE7)
MAE7 = sum(abs(testing$ClaimAmount-PP7))/n

# GLM binomial x GLM Inverse gaussian
PP8 = m_occbinglm*m_cinvglm
MSE8 = mean((testing$ClaimAmount-PP8)^2)
RMSE8 = sqrt(MSE8)
MAE8 = sum(abs(testing$ClaimAmount-PP8))/n

# Ringkasan GLM x GLM
(eval1 <- data.frame(
  RMSE = c(RMSE1,RMSE2,RMSE4,RMSE5,RMSE7,RMSE8),
  MAE = c(MAE1,MAE2,MAE4,MAE5,MAE7,MAE8)
))

# GAM poi x GAM Gamma
PP10 = m_freqpoigam*m_igammgam
MSE10 = mean((testing$ClaimAmount-PP10)^2)
RMSE10 = sqrt(MSE10)
MAE10 = sum(abs(testing$ClaimAmount-PP10))/n

# GAM poi x GAM Inverse Gaussian
PP11 = m_freqpoigam*m_iinvgam
MSE11 = mean((testing$ClaimAmount-PP11)^2)
RMSE11 = sqrt(MSE11)
MAE11 = sum(abs(testing$ClaimAmount-PP11))/n

# GAM nb x GAM Gamma
PP13 = m_freqnbgam*m_igammgam
MSE13 = mean((testing$ClaimAmount-PP13)^2)
RMSE13 = sqrt(MSE13)
MAE13 = sum(abs(testing$ClaimAmount-PP13))/n

# GAM nb x GAM Inverse Gaussian
PP14 = m_freqnbgam*m_iinvgam
MSE14 = mean((testing$ClaimAmount-PP14)^2)
RMSE14 = sqrt(MSE14)
MAE14 = sum(abs(testing$ClaimAmount-PP14))/n

# GAM binomial x GAM Gamma
PP16 = m_occbingam*m_cgammgam
MSE16 = mean((testing$ClaimAmount-PP16)^2)
RMSE16 = sqrt(MSE16)
MAE16 = sum(abs(testing$ClaimAmount-PP16))/n

# GAM binomial x GAM Inverse gaussian
PP17 = m_occbingam*m_cinvgam
MSE17 = mean((testing$ClaimAmount-PP17)^2)
RMSE17 = sqrt(MSE17)
MAE17 = sum(abs(testing$ClaimAmount-PP17))/n

# Ringkasan GAM x GAM
(eval2 <- data.frame(
  RMSE = c(RMSE10,RMSE11,RMSE13,RMSE14,RMSE16,RMSE17),
  MAE = c(MAE10,MAE11,MAE13,MAE14,MAE16,MAE17)
))

# GLM poi x GAM Gamma
PP19 = m_freqpoiglm*m_igammgam
MSE19 = mean((testing$ClaimAmount-PP19)^2)
RMSE19 = sqrt(MSE19)
MAE19 = sum(abs(testing$ClaimAmount-PP19))/n

# GLM poi x GAM Inverse Gaussian
PP20 = m_freqpoiglm*m_iinvgam
MSE20 = mean((testing$ClaimAmount-PP20)^2)
RMSE20 = sqrt(MSE20)
MAE20 = sum(abs(testing$ClaimAmount-PP20))/n

# GLM nb x GAM Gamma
PP22 = m_freqnbglm*m_igammgam
MSE22 = mean((testing$ClaimAmount-PP22)^2)
RMSE22 = sqrt(MSE22)
MAE22 = sum(abs(testing$ClaimAmount-PP22))/n

# GLM nb x GAM Inverse Gaussian
PP23 = m_freqnbglm*m_iinvgam
MSE23 = mean((testing$ClaimAmount-PP23)^2)
RMSE23 = sqrt(MSE23)
MAE23 = sum(abs(testing$ClaimAmount-PP23))/n

# GLM binomial x GAM Gamma
PP25 = m_occbinglm*m_cgammgam
MSE25 = mean((testing$ClaimAmount-PP25)^2)
RMSE25 = sqrt(MSE25)
MAE25 = sum(abs(testing$ClaimAmount-PP25))/n

# GLM binomial x GAM Inverse gaussian
PP26 = m_occbinglm*m_cinvgam
MSE26 = mean((testing$ClaimAmount-PP26)^2)
RMSE26 = sqrt(MSE26)
MAE26 = sum(abs(testing$ClaimAmount-PP26))/n

# Ringkasan GLM x GAM
(eval3 <- data.frame(
  RMSE = c(RMSE19,RMSE20,RMSE22,RMSE23,RMSE25,RMSE26),
  MAE = c(MAE19,MAE20,MAE22,MAE23,MAE25,MAE26)
))

# GAM poi x GLM Gamma
PP28 = m_freqpoigam*m_igammglm
MSE28 = mean((testing$ClaimAmount-PP28)^2)
RMSE28 = sqrt(MSE28)
MAE28 = sum(abs(testing$ClaimAmount-PP28))/n

# GAM poi x GLM Inverse Gaussian
PP29 = m_freqpoigam*m_iinvglm
MSE29 = mean((testing$ClaimAmount-PP29)^2)
RMSE29 = sqrt(MSE29)
MAE29 = sum(abs(testing$ClaimAmount-PP29))/n

# GAM nb x GLM Gamma
PP31 = m_freqnbgam*m_igammglm
MSE31 = mean((testing$ClaimAmount-PP31)^2)
RMSE31 = sqrt(MSE31)
MAE31 = sum(abs(testing$ClaimAmount-PP31))/n

# GAM nb x GLM Inverse Gaussian
PP32 = m_freqnbgam*m_iinvglm
MSE32 = mean((testing$ClaimAmount-PP32)^2)
RMSE32 = sqrt(MSE32)
MAE32 = sum(abs(testing$ClaimAmount-PP32))/n

# GAM binomial x GLM Gamma
PP34 = m_occbingam*m_cgammglm
MSE34 = mean((testing$ClaimAmount-PP34)^2)
RMSE34 = sqrt(MSE34)
MAE34 = sum(abs(testing$ClaimAmount-PP34))/n

# GAM binomial x GLM Inverse gaussian
PP35 = m_occbingam*m_cinvglm
MSE35 = mean((testing$ClaimAmount-PP35)^2)
RMSE35 = sqrt(MSE35)
MAE35 = sum(abs(testing$ClaimAmount-PP35))/n

# Ringkasan GAM x GLM
(eval4 <- data.frame(
  RMSE = c(RMSE28,RMSE29,RMSE31,RMSE32,RMSE34,RMSE35),
  MAE = c(MAE28,MAE29,MAE31,MAE32,MAE34,MAE35)
))

# Boxplot untuk model dengan MSE, RMSE, dan MAE terkecil:
a = testing$ClaimAmount
grup1 = PP7
grup2 = PP16
grup3 = PP25
grup4 = PP34
group <- list(a, grup1, grup2, grup3, grup4)
boxplot(group,
        col = c("grey", "grey", "grey", "grey", "grey"),  # Set box colors
        border = c("black", "black", "black", "black", "black"),                # Set border colors
        names = c("Data Asli", "Grup 1", "Grup 2", "Grup 3", "Grup 4"),       # Set group names
        main = "Boxplot: Perbandingan Data Asli dengan Hasil Model Terbaik",
        ylab = "ClaimAmount" ,ylim=c(0,4000))

means <- sapply(group, mean)
points(1:length(group), means, col = "blue", pch = 18, cex = 2)
text(1:length(group), means + 20, round(means, 2), col = "red", pos = 4)
# terbukti bahwa grup 1, yaitu glm binom x glm gamma lebih mendekati rata-rata data asli. 

# mean difference:
g1 <- mean(testing$ClaimAmount)-mean(PP7)
g2 <- mean(testing$ClaimAmount)-mean(PP16)
g3 <- mean(testing$ClaimAmount)-mean(PP25)
g4 <- mean(testing$ClaimAmount)-mean(PP34)
as.data.frame(cbind(g1,g2,g3,g4))

# Evaluasi Model Training (Freq & Sev) ------------------------------------

pred_freqpoiglm <- freqpoiglm$fitted.values
n=length(training$ClaimNb)
SSfreqpoiglm = sum((training$ClaimNb-pred_freqpoiglm)^2)
RMSEfreqpoiglm = sqrt(sum(((training$ClaimNb-pred_freqpoiglm)^2)/n))
MAEfreqpoiglm = sum(abs(training$ClaimNb-pred_freqpoiglm))/n

pred_freqnbglm <- freqnbglm$fitted.values
n=length(training$ClaimNb)
SSfreqnbglm = sum((training$ClaimNb-pred_freqnbglm)^2)
RMSEfreqnbglm = sqrt(sum(((training$ClaimNb-pred_freqnbglm)^2)/n))
MAEfreqnbglm = sum(abs(training$ClaimNb-pred_freqnbglm))/n

pred_occbinglm <- occbinglm$fitted.values
n=length(training$ClaimOcc)
SSoccbinglm = sum((training$ClaimOcc-pred_occbinglm)^2)
RMSEoccbinglm = sqrt(sum(((training$ClaimOcc-pred_occbinglm)^2)/n))
MAEoccbinglm = sum(abs(training$ClaimOcc-pred_occbinglm))/n

pred_igammglm <- igammglm$fitted.values
n=length(training01$IndividualClaim)
SSigammglm = sum((training01$IndividualClaim-pred_igammglm)^2)
RMSEigammglm = sqrt(sum(((training01$IndividualClaim-pred_igammglm)^2)/n))
MAEigammglm = sum(abs(training01$IndividualClaim-pred_igammglm))/n

pred_iinvglm <- iinvglm$fitted.values
n=length(training01$IndividualClaim)
SSiinvglm = sum((training01$IndividualClaim-pred_iinvglm)^2)
RMSEiinvglm = sqrt(sum(((training01$IndividualClaim-pred_iinvglm)^2)/n))
MAEiinvglm = sum(abs(training01$IndividualClaim-pred_iinvglm))/n

pred_cgammglm <- cgammglm$fitted.values
n=length(training01$ClaimAmount)
SScgammglm = sum((training01$ClaimAmount-pred_cgammglm)^2)
RMSEcgammglm = sqrt(sum(((training01$ClaimAmount-pred_cgammglm)^2)/n))
MAEcgammglm = sum(abs(training01$ClaimAmount-pred_cgammglm))/n

pred_cinvglm <- cinvglm$fitted.values
n=length(training01$ClaimAmount)
SScinvglm = sum((training01$ClaimAmount-pred_cinvglm)^2)
RMSEcinvglm = sqrt(sum(((training01$ClaimAmount-pred_cinvglm)^2)/n))
MAEcinvglm = sum(abs(training01$ClaimAmount-pred_cinvglm))/n

pred_freqpoigam <- freqpoigam$fitted.values
n=length(training$ClaimNb)
SSfreqpoigam = sum((training$ClaimNb-pred_freqpoigam)^2)
RMSEfreqpoigam = sqrt(sum(((training$ClaimNb-pred_freqpoigam)^2)/n))
MAEfreqpoigam = sum(abs(training$ClaimNb-pred_freqpoigam))/n

pred_freqnbgam <- freqnbgam$fitted.values
n=length(training$ClaimNb)
SSfreqnbgam = sum((training$ClaimNb-pred_freqnbgam)^2)
RMSEfreqnbgam = sqrt(sum(((training$ClaimNb-pred_freqnbgam)^2)/n))
MAEfreqnbgam = sum(abs(training$ClaimNb-pred_freqnbgam))/n

pred_occbingam <- occbingam$fitted.values
n=length(training$ClaimOcc)
SSoccbingam = sum((training$ClaimOcc-pred_occbingam)^2)
RMSEoccbingam = sqrt(sum(((training$ClaimOcc-pred_occbingam)^2)/n))
MAEoccbingam = sum(abs(training$ClaimOcc-pred_occbingam))/n

pred_igammgam <- igammgam$fitted.values
n=length(training01$IndividualClaim)
SSigammgam = sum((training01$IndividualClaim-pred_igammgam)^2)
RMSEigammgam = sqrt(sum(((training01$IndividualClaim-pred_igammgam)^2)/n))
MAEigammgam = sum(abs(training01$IndividualClaim-pred_igammgam))/n

pred_iinvgam <- iinvgam$fitted.values
n=length(training01$IndividualClaim)
SSiinvgam = sum((training01$IndividualClaim-pred_iinvgam)^2)
RMSEiinvgam = sqrt(sum(((training01$IndividualClaim-pred_iinvgam)^2)/n))
MAEiinvgam = sum(abs(training01$IndividualClaim-pred_iinvgam))/n

pred_cgammgam <- cgammgam$fitted.values
n=length(training01$ClaimAmount)
SScgammgam = sum((training01$ClaimAmount-pred_cgammgam)^2)
RMSEcgammgam = sqrt(sum(((training01$ClaimAmount-pred_cgammgam)^2)/n))
MAEcgammgam = sum(abs(training01$ClaimAmount-pred_cgammgam))/n

pred_cinvgam <- cinvgam$fitted.values
n=length(training01$ClaimAmount)
SScinvgam = sum((training01$ClaimAmount-pred_cinvgam)^2)
RMSEcinvgam = sqrt(sum(((training01$ClaimAmount-pred_cinvgam)^2)/n))
MAEcinvgam = sum(abs(training01$ClaimAmount-pred_cinvgam))/n

(ClaimNbPoi <- data.frame(
  glmNbpoi = c(RMSEfreqpoiglm,MAEfreqpoiglm,SSfreqpoiglm),
  gamNbpoi = c(RMSEfreqpoigam,MAEfreqpoigam,SSfreqpoigam)
))
(ClaimNbNegbin <- data.frame(
  glmNbnb = c(RMSEfreqnbglm,MAEfreqnbglm,SSfreqnbglm),
  gamNbnb = c(RMSEfreqnbgam,MAEfreqnbgam,SSfreqnbgam)
))
(ClaimOccBin <- data.frame(
  glmOccbin = c(RMSEoccbinglm,MAEoccbinglm,SSoccbinglm),
  gamOccbin = c(RMSEoccbingam,MAEoccbingam,SSoccbingam)
))
(ClaimIndGamm <- data.frame(
  glmIndGamm = c(RMSEigammglm,MAEigammglm,SSigammglm),
  gamIndGamm = c(RMSEigammgam,MAEigammgam,SSigammgam)
))
(ClaimIndInv <- data.frame(
  glmIndInv = c(RMSEiinvglm,MAEiinvglm,SSiinvglm),
  gamIndInv = c(RMSEiinvgam,MAEiinvgam,SSiinvgam)
))
(ClaimAmGamm <- data.frame(
  glmAmGamm = c(RMSEcgammglm,MAEcgammglm,SScgammglm),
  gamAmGamm = c(RMSEcgammgam,MAEcgammgam,SScgammgam)
))
(ClaimAmInv <- data.frame(
  glmAmInv = c(RMSEcinvglm,MAEcinvglm,SScinvglm),
  gamAmInv = c(RMSEcinvgam,MAEcinvgam,SScinvgam)
))


# Evaluasi Model Testing (Freq & Sev) -------------------------------------

pred_freqpoiglm <- m_freqpoiglm
n=length(testing$ClaimNb)
SSfreqpoiglm = sum((testing$ClaimNb-pred_freqpoiglm)^2)
RMSEfreqpoiglm = sqrt(sum(((testing$ClaimNb-pred_freqpoiglm)^2)/n))
MAEfreqpoiglm = sum(abs(testing$ClaimNb-pred_freqpoiglm))/n

pred_freqnbglm <- m_freqnbglm
n=length(testing$ClaimNb)
SSfreqnbglm = sum((testing$ClaimNb-pred_freqnbglm)^2)
RMSEfreqnbglm = sqrt(sum(((testing$ClaimNb-pred_freqnbglm)^2)/n))
MAEfreqnbglm = sum(abs(testing$ClaimNb-pred_freqnbglm))/n

pred_occbinglm <- m_occbinglm
n=length(testing$ClaimOcc)
SSoccbinglm = sum((testing$ClaimOcc-pred_occbinglm)^2)
RMSEoccbinglm = sqrt(sum(((testing$ClaimOcc-pred_occbinglm)^2)/n))
MAEoccbinglm = sum(abs(testing$ClaimOcc-pred_occbinglm))/n

pred_igammglm <- m_igammglm
n=length(testing$IndividualClaim)
SSigammglm = sum((testing$IndividualClaim-pred_igammglm)^2)
RMSEigammglm = sqrt(sum(((testing$IndividualClaim-pred_igammglm)^2)/n))
MAEigammglm = sum(abs(testing$IndividualClaim-pred_igammglm))/n

pred_iinvglm <- m_iinvglm
n=length(testing$IndividualClaim)
SSiinvglm = sum((testing$IndividualClaim-pred_iinvglm)^2)
RMSEiinvglm = sqrt(sum(((testing$IndividualClaim-pred_iinvglm)^2)/n))
MAEiinvglm = sum(abs(testing$IndividualClaim-pred_iinvglm))/n

pred_cgammglm <- m_cgammglm
n=length(testing$ClaimAmount)
SScgammglm = sum((testing$ClaimAmount-pred_cgammglm)^2)
RMSEcgammglm = sqrt(sum(((testing$ClaimAmount-pred_cgammglm)^2)/n))
MAEcgammglm = sum(abs(testing$ClaimAmount-pred_cgammglm))/n

pred_cinvglm <- m_cinvglm
n=length(testing$ClaimAmount)
SScinvglm = sum((testing$ClaimAmount-pred_cinvglm)^2)
RMSEcinvglm = sqrt(sum(((testing$ClaimAmount-pred_cinvglm)^2)/n))
MAEcinvglm = sum(abs(testing$ClaimAmount-pred_cinvglm))/n

pred_freqpoigam <- m_freqpoigam
n=length(testing$ClaimNb)
SSfreqpoigam = sum((testing$ClaimNb-pred_freqpoigam)^2)
RMSEfreqpoigam = sqrt(sum(((testing$ClaimNb-pred_freqpoigam)^2)/n))
MAEfreqpoigam = sum(abs(testing$ClaimNb-pred_freqpoigam))/n

pred_freqnbgam <- m_freqnbgam
n=length(testing$ClaimNb)
SSfreqnbgam = sum((testing$ClaimNb-pred_freqnbgam)^2)
RMSEfreqnbgam = sqrt(sum(((testing$ClaimNb-pred_freqnbgam)^2)/n))
MAEfreqnbgam = sum(abs(testing$ClaimNb-pred_freqnbgam))/n

pred_occbingam <- m_occbingam
n=length(testing$ClaimOcc)
SSoccbingam = sum((testing$ClaimOcc-pred_occbingam)^2)
RMSEoccbingam = sqrt(sum(((testing$ClaimOcc-pred_occbingam)^2)/n))
MAEoccbingam = sum(abs(testing$ClaimOcc-pred_occbingam))/n

pred_igammgam <- m_igammgam
n=length(testing$IndividualClaim)
SSigammgam = sum((testing$IndividualClaim-pred_igammgam)^2)
RMSEigammgam = sqrt(sum(((testing$IndividualClaim-pred_igammgam)^2)/n))
MAEigammgam = sum(abs(testing$IndividualClaim-pred_igammgam))/n

pred_iinvgam <- m_iinvgam
n=length(testing$IndividualClaim)
SSiinvgam = sum((testing$IndividualClaim-pred_iinvgam)^2)
RMSEiinvgam = sqrt(sum(((testing$IndividualClaim-pred_iinvgam)^2)/n))
MAEiinvgam = sum(abs(testing$IndividualClaim-pred_iinvgam))/n

pred_cgammgam <- m_cgammgam
n=length(testing$ClaimAmount)
SScgammgam = sum((testing$ClaimAmount-pred_cgammgam)^2)
RMSEcgammgam = sqrt(sum(((testing$ClaimAmount-pred_cgammgam)^2)/n))
MAEcgammgam = sum(abs(testing$ClaimAmount-pred_cgammgam))/n

pred_cinvgam <- m_cinvgam
n=length(testing$ClaimAmount)
SScinvgam = sum((testing$ClaimAmount-pred_cinvgam)^2)
RMSEcinvgam = sqrt(sum(((testing$ClaimAmount-pred_cinvgam)^2)/n))
MAEcinvgam = sum(abs(testing$ClaimAmount-pred_cinvgam))/n

(ClaimNbPoi01 <- data.frame(
  glmNbpoi = c(RMSEfreqpoiglm,MAEfreqpoiglm,SSfreqpoiglm),
  gamNbpoi = c(RMSEfreqpoigam,MAEfreqpoigam,SSfreqpoigam)
))
(ClaimNbNegbin01 <- data.frame(
  glmNbnb = c(RMSEfreqnbglm,MAEfreqnbglm,SSfreqnbglm),
  gamNbnb = c(RMSEfreqnbgam,MAEfreqnbgam,SSfreqnbgam)
))
(ClaimOccBin01 <- data.frame(
  glmOccbin = c(RMSEoccbinglm,MAEoccbinglm,SSoccbinglm),
  gamOccbin = c(RMSEoccbingam,MAEoccbingam,SSoccbingam)
))
(ClaimIndGamm01 <- data.frame(
  glmIndGamm = c(RMSEigammglm,MAEigammglm,SSigammglm),
  gamIndGamm = c(RMSEigammgam,MAEigammgam,SSigammgam)
))
(ClaimIndInv01 <- data.frame(
  glmIndInv = c(RMSEiinvglm,MAEiinvglm,SSiinvglm),
  gamIndInv = c(RMSEiinvgam,MAEiinvgam,SSiinvgam)
))
(ClaimAmGamm01 <- data.frame(
  glmAmGamm = c(RMSEcgammglm,MAEcgammglm,SScgammglm),
  gamAmGamm = c(RMSEcgammgam,MAEcgammgam,SScgammgam)
))
(ClaimAmInv01 <- data.frame(
  glmAmInv = c(RMSEcinvglm,MAEcinvglm,SScinvglm),
  gamAmInv = c(RMSEcinvgam,MAEcinvgam,SScinvgam)
))






