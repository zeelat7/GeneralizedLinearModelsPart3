#Zeeshan Latifi
#Predict 411 Winter 2018

# Note, some of these libraries are not needed for this template code.
library(readr)
library(dplyr)
library(zoo)
library(psych)
library(ROCR)
library(corrplot)
library(car)
library(InformationValue)
library(rJava)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(corrplot)
library(glm2)
library(aod)
library(mice)
library(lattice)
# Data Import and Variable Type Changes
setwd("/Users/Zeeshan/Desktop/PREDICT 411/Unit 2/Auto Insurance/Unit 2 Help/")
data <- read.csv("logit_insurance.csv")
test <- read.csv("logit_insurance_test.csv")

### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical

data$INDEX <- as.factor(data$INDEX)
data$TARGET_FLAG <- as.factor(data$TARGET_FLAG)
data$SEX <- as.factor(data$SEX)
data$EDUCATION <- as.factor(data$EDUCATION)
data$PARENT1 <- as.factor(data$PARENT1)
data$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$INCOME)))

data$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$MSTATUS <- as.factor(data$MSTATUS)
data$REVOKED <- as.factor(data$REVOKED)
data$RED_CAR <- as.factor(ifelse(data$RED_CAR=="yes", 1, 0))
data$URBANICITY <- ifelse(data$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
data$URBANICITY <- as.factor(data$URBANICITY)
data$JOB <- as.factor(data$JOB)
data$CAR_USE <- as.factor(data$CAR_USE)
data$CAR_TYPE <- as.factor(data$CAR_TYPE)
data$DO_KIDS_DRIVE <- as.factor(ifelse(data$KIDSDRIV > 0, 1, 0 ))
data$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$HOME_VAL)))
data$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", data$BLUEBOOK)))
summary(data)


######## Same treatment on test data set ###########################

### Need to make sure our data is understood correctly by R, since we have a mix of numerical and categorical

test$INDEX <- as.factor(test$INDEX)
test$TARGET_FLAG <- as.factor(test$TARGET_FLAG)
test$SEX <- as.factor(test$SEX)
test$EDUCATION <- as.factor(test$EDUCATION)
test$PARENT1 <- as.factor(test$PARENT1)
test$INCOME <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$INCOME)))
test$HOME_VAL <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$MSTATUS <- as.factor(test$MSTATUS)
test$REVOKED <- as.factor(test$REVOKED)
test$RED_CAR <- as.factor(ifelse(test$RED_CAR=="yes", 1, 0))
test$URBANICITY <- ifelse(test$URBANICITY == "Highly Urban/ Urban", "Urban", "Rural")
test$URBANICITY <- as.factor(test$URBANICITY)
test$JOB <- as.factor(test$JOB)
test$CAR_USE <- as.factor(test$CAR_USE)
test$CAR_TYPE <- as.factor(test$CAR_TYPE)
test$DO_KIDS_DRIVE <- as.factor(ifelse(test$KIDSDRIV > 0, 1, 0 ))
test$OLDCLAIM <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$HOME_VAL)))
test$BLUEBOOK <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", test$BLUEBOOK)))
summary(test)


#################### Part 1: Data Exploration ##############################################
# Histograms for Numeric Variables
par(mfrow=c(2,2))
hist(data$AGE, col = "red", xlab = "Age", main = "AGE Hist")
data0<- subset(data, TARGET_FLAG == 1 )
hist(data0$TARGET_AMT, col = "green", xlab = "TARGET_AMT", main = "TARGET_AMT Hist")
boxplot(data$AGE, col = "red", main = "AGE BoxPlot")
boxplot((data0$TARGET_AMT), col = "green", main = "TARGET_AMT Boxplot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(data$TRAVTIME, col = "green", xlab = "TRAVTIME", main = "TRAVTIME Hist")
hist(data$YOJ, col = "blue", xlab = "YOJ", main = "YOJ Hist")
boxplot(data$TRAVTIME, col = "green", main = "TRAVTIME BoxPlot")
boxplot(data$YOJ, col = "blue", main = "YOJ BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(data$BLUEBOOK, col = "green", xlab = "BLUEBOOK", main = "BLUEBOOK Hist")
hist((data$TIF), col = "blue", xlab = "TIF", main = "TIF Hist")
boxplot(data$BLUEBOOK, col = "green", main = "BLUEBOOK BoxPlot")
boxplot(data$TIF, col = "blue", main = "TIF BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(data$MVR_PTS, col = "red", xlab = "MVR_PTS", main = "MVR_PTS Hist")
hist(data$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE Hist")
boxplot(data$MVR_PTS, col = "red", main = "MVR_PTS BoxPlot")
boxplot(data$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(log(data$TRAVTIME), col = "red", xlab = "TRAVTIME", main = "TRAVTIME Hist")
boxplot(log(data$TRAVTIME), col = "red", main = "TRAVTIME BoxPlot")
par(mfrow=c(1,1))

########### Part 2: Data Transformation ##################
# Fix NA's, note car age

data$AGE[is.na(data$AGE)] <- mean(data$AGE, na.rm = "TRUE")
data$CAR_AGE[data$CAR_AGE < 0 ] <- 0
data$OLDCLAIM <- ifelse(data$CAR_AGE < 5 & !is.na(data$CAR_AGE),0,data$OLDCLAIM)
data$OLDCLAIM <- na.aggregate(data$OLDCLAIM, data$CAR_AGE, mean, na.rm = TRUE )
data$HOME_OWNER <- ifelse(data$HOME_VAL == 0, 0, 1)
data$SQRT_TRAVTIME <- sqrt(data$TRAVTIME)
data$SQRT_BLUEBOOK <- sqrt(data$BLUEBOOK)
data$log_TRAVTIME <- log(data$TRAVTIME)
data$log_BLUEBOOK <- log(data$BLUEBOOK)
# 
# Bin Income
data$INCOME_bin[is.na(data$INCOME)] <- "NA"
data$INCOME_bin[data$INCOME == 0] <- "Zero"
data$INCOME_bin[data$INCOME >= 1 & data$INCOME < 30000] <- "Low"
data$INCOME_bin[data$INCOME >= 30000 & data$INCOME < 80000] <- "Medium"
data$INCOME_bin[data$INCOME >= 80000] <- "High"
data$INCOME_bin <- factor(data$INCOME_bin)
data$INCOME_bin <- factor(data$INCOME_bin, levels=c("NA","Zero","Low","Medium","High"))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)

md.pattern(data)

dataNum <- subset(data, select = -c(INDEX, TARGET_FLAG, TARGET_AMT, AGE, HOMEKIDS, PARENT1, MSTATUS, SEX,
                                    EDUCATION, JOB, TRAVTIME, CAR_USE, BLUEBOOK, TIF, CAR_TYPE, RED_CAR, CLM_FREQ,
                                    REVOKED, MVR_PTS, URBANICITY, DO_KIDS_DRIVE))

dataRest <- subset(data, select = c(INDEX, TARGET_FLAG, TARGET_AMT, AGE, HOMEKIDS, PARENT1, MSTATUS, SEX,
                                    EDUCATION, JOB, TRAVTIME, CAR_USE, BLUEBOOK, TIF, CAR_TYPE, RED_CAR, CLM_FREQ,
                                    REVOKED, MVR_PTS, URBANICITY, DO_KIDS_DRIVE))

summary(dataRest)
tempData <- mice(dataNum,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

completedData <- complete(tempData,1)
finalset<-cbind(dataRest, completedData)
summary(finalset)


#do the same but for test set------------------------------------------
test$AGE[is.na(test$AGE)] <- mean(test$AGE, na.rm = "TRUE")

test$CAR_AGE[test$CAR_AGE < 0 ] <- 0 
test$OLDCLAIM <- ifelse(test$CAR_AGE < 5 & !is.na(test$CAR_AGE),0,test$OLDCLAIM)
test$OLDCLAIM <- na.aggregate(test$OLDCLAIM, test$CAR_AGE, mean, na.rm = TRUE )
test$HOME_OWNER <- ifelse(test$HOME_VAL == 0, 0, 1)
test$SQRT_TRAVTIME <- sqrt(test$TRAVTIME)
test$SQRT_BLUEBOOK <- sqrt(test$BLUEBOOK)
test$log_TRAVTIME <- log(test$TRAVTIME)
test$log_BLUEBOOK <- log(test$BLUEBOOK)

# Bin Income
test$INCOME_bin[is.na(test$INCOME)] <- "NA"
test$INCOME_bin[test$INCOME == 0] <- "Zero"
test$INCOME_bin[test$INCOME >= 1 & test$INCOME < 30000] <- "Low"
test$INCOME_bin[test$INCOME >= 30000 & test$INCOME < 80000] <- "Medium"
test$INCOME_bin[test$INCOME >= 80000] <- "High"
test$INCOME_bin <- factor(test$INCOME_bin)
test$INCOME_bin <- factor(test$INCOME_bin, levels=c("NA","Zero","Low","Medium","High"))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(test,2,pMiss)
apply(test,1,pMiss)

md.pattern(test)

testNum <- subset(test, select = -c(INDEX, TARGET_FLAG, TARGET_AMT, AGE, HOMEKIDS, PARENT1, MSTATUS, SEX,
                                    EDUCATION, JOB, TRAVTIME, CAR_USE, BLUEBOOK, TIF, CAR_TYPE, RED_CAR, CLM_FREQ,
                                    REVOKED, MVR_PTS, URBANICITY, DO_KIDS_DRIVE))

testRest <- subset(test, select = c(INDEX, TARGET_FLAG, TARGET_AMT, AGE, HOMEKIDS, PARENT1, MSTATUS, SEX,
                                    EDUCATION, JOB, TRAVTIME, CAR_USE, BLUEBOOK, TIF, CAR_TYPE, RED_CAR, CLM_FREQ,
                                    REVOKED, MVR_PTS, URBANICITY, DO_KIDS_DRIVE))

summary(testRest)
tempTestData <- mice(testNum,m=5,maxit=50,meth='pmm',seed=500)
summary(tempTestData)

completedTestData <- complete(tempTestData,1)
finalTestSet<-cbind(testRest, completedTestData)
summary(finalTestSet)


#-----------------------------------------------------------------------------------
numeric <- subset(finalset, select = c(TARGET_AMT, AGE, HOMEKIDS, YOJ, INCOME, HOME_VAL, TRAVTIME, BLUEBOOK, TIF,
                                       CLM_FREQ, MVR_PTS, CAR_AGE), na.rm = TRUE)
c <- cor(numeric)
corrplot(c, method = "square")


############# Part 3: Model Development #################################################################
#Model Development for TARGET_FLAG

#Model 1 -------------------------------------------------------------------------------------------------------
Model1 <- glm(TARGET_FLAG ~ AGE + BLUEBOOK + TRAVTIME + KIDSDRIV + SEX + URBANICITY + HOMEKIDS + 
                CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + TIF + EDUCATION + MSTATUS + PARENT1 + RED_CAR + 
                CAR_USE + CAR_TYPE + YOJ + JOB + INCOME_bin + HOME_VAL, data = finalset, family = binomial())
summary(Model1)
#finalset$Model1Prediction <- predict(Model1, type = "response")

stepwise <- stepAIC(Model1, direction = "both")

summary(stepwise)
vif(stepwise)
sqrt(vif(stepwise)) > 2
finalset$Model1Prediction <- predict(stepwise, type = "response")


#Model 2 -------------------------------------------------------------------------------------------------------
reg1 <- regsubsets(TARGET_FLAG ~  AGE + BLUEBOOK + TRAVTIME + KIDSDRIV + SEX + URBANICITY + HOMEKIDS + 
                     CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE + TIF + EDUCATION + MSTATUS + PARENT1 + RED_CAR + 
                     CAR_USE + CAR_TYPE + YOJ + JOB + INCOME_bin + HOME_VAL + OLDCLAIM, data = finalset, nvmax = 50)

summary(reg1)

Model2 <- glm(TARGET_FLAG ~ BLUEBOOK + TRAVTIME +  URBANICITY + REVOKED + MVR_PTS + TIF + PARENT1 + CAR_USE
              + HOME_VAL, data = finalset, family = binomial())
summary(Model2)
finalset$Model2Prediction <- predict(Model2, type = "response")

#Model 3 -------------------------------------------------------------------------------------------------------

Model3 <- glm(TARGET_FLAG ~ AGE + log_TRAVTIME + log_BLUEBOOK + DO_KIDS_DRIVE + URBANICITY +
                CLM_FREQ + REVOKED + MVR_PTS + TIF + EDUCATION + MSTATUS + PARENT1 + CAR_USE + CAR_TYPE + JOB + 
                HOME_OWNER,
              data = finalset, family = binomial())
summary(Model3)
#finalset$Model3Prediction <- predict(Model3, type = "response")

forward <- stepAIC(Model3, direction = "forward")

summary(forward)
vif(forward)
sqrt(vif(forward)) > 2
finalset$Model3Prediction <- predict(forward, type = "response")

###
### Note, we will need to build a quick model for TARGET_AMT.  Perhaps using just a generic multiple linear regression.
###

########## Part 4: Model Selection 
AIC(stepwise)
BIC(stepwise)
AIC(Model2)
BIC(Model2)
AIC(forward)
BIC(forward)
print(-2*logLik(stepwise, REML = TRUE))
print(-2*logLik(Model2, REML = TRUE))
print(-2*logLik(forward, REML = TRUE))
ks_stat(actuals=finalset$TARGET_FLAG, predictedScores=finalset$Model1Prediction)
ks_stat(actuals=finalset$TARGET_FLAG, predictedScores=finalset$Model2Prediction)
ks_stat(actuals=finalset$TARGET_FLAG, predictedScores=finalset$Model3Prediction)

###############ROC Curves#######################################
install.packages("Deducer")
library(Deducer)

rocplot(stepwise, diag = TRUE, AUC = TRUE)
rocplot(Model2, diag = TRUE, AUC = TRUE)
rocplot(forward, diag = TRUE, AUC = TRUE)

#model coefficients
coef(stepwise)
coef(Model2)
coef(forward)

#### Part 5:  Score Model on Test Data set and output csv file

# Again, double-checking to make sure we don't have any NA's in our Test Data Set
summary(finalTestSet)

########### STAND ALONE SCORING PROGRAM ###############
##### Model coefficients used to create P_TARGET_FLAG, Mean TARGET_AMT aggregated by CAR was used to predict
######## P-TARGET AMOUNT
######## Note that you may want to do a multiple linear regression for this model.
######## The model below is based on CAR multiplied by the amount and probability of a claim (27%)
data1<- subset(finalset, TARGET_FLAG == 1 )
finalTestSet$P_TARGET_FLAG <- predict(forward, newdata = finalTestSet, type = "response")
targetbycar <- aggregate(data1$TARGET_AMT, list(data1$CAR_TYPE), mean)


finalTestSet$P_TARGET_AMT <- ifelse(test$CAR_TYPE=="Minivan", 5601.67%*%mean(finalTestSet$P_TARGET_FLAG),
                                    ifelse(test$CAR_TYPE=="Panel Truck",7464.70%*%mean(finalTestSet$P_TARGET_FLAG),
                                           ifelse(test$CAR_TYPE=="Pickup", 5430.11%*%mean(finalTestSet$P_TARGET_FLAG),
                                                  ifelse(test$CAR_TYPE=="Sports Car", 5412.73%*%mean(finalTestSet$P_TARGET_FLAG),
                                                         ifelse(test$CAR_TYPE=="Van", 6908.553%*%mean(finalTestSet$P_TARGET_FLAG), 5241.104%*%mean(finalTestSet$P_TARGET_FLAG))))))
mean(finalTestSet$P_TARGET_FLAG)
mean(finalTestSet$P_TARGET_AMT)

# Scored Data File
scores <- finalTestSet[c("INDEX","P_TARGET_FLAG", "P_TARGET_AMT")]
write.csv(scores, file = "Zeeshan_CI_Scored.csv")
write.csv(as.data.frame(scores), file = "Zeeshan_logit_insurance_test.csv", row.names = FALSE)