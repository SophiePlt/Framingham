#install.packages

library(dplyr)
library(ggplot2)
library(GGally)
library(caTools)
library(ROCR)
library(MASS)


#### Reading and splitting data. We use the function "sample.split" 
#to ensure that training and test sets have approximately 
#equal proportions of people with TenYearCHD

framingham <- read.csv("framingham.csv")

set.seed(144)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.7)

framingham.train <- filter(framingham, split == TRUE) 
framingham.test <- filter(framingham, split == FALSE)

table(framingham.train$TenYearCHD)

## We create the logistic regression model using all the independant variables

mod1 <- glm(TenYearCHD~., data=framingham.train, family="binomial") #we consider all 
summary(mod1)

# We predict results using our model 

predTest = predict(mod1, newdata=framingham.test, type="response")
summary(predTest)

#separate the patients who have high risk and low risk using 
#the threshold obtained using the decision tree

table(framingham.test$TenYearCHD, predTest > 0.16) 

# We create a model that predicts none of the patients are at high risk for CHD
table(framingham.test$TenYearCHD, predTest > 1)

# We use our model to determine the probability that the patient will develop CHD in the next ten years
new_patient <- data.frame(male = 0,
                      age = 51,
                      education = "College",
                      currentSmoker = 1,
                      cigsPerDay = 20,
                      BPMeds = 0,
                      prevalentStroke = 0,
                      prevalentHyp = 1,
                      diabetes = 0,
                      totChol = 220,
                      sysBP = 140,
                      diaBP = 100,
                      BMI = 31,
                      heartRate = 59,
                      glucose = 78)

predict(mod1, newdata = new_patient, type = "response")

#We plot ROC curve and calculate AUC
rocr.pred <- prediction(predTest, framingham.test$TenYearCHD)
logPerformance <- performance(rocr.pred, "tpr", "fpr")
plot(logPerformance, colorize = TRUE)
abline(0, 1)
as.numeric(performance(rocr.pred, "auc")@y.values)


AUROC<- performance(ROCRpred, 'auc')
AUROC <- AUROC@y.values
AUROC 






