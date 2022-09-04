#Import dataset
setwd(dir = "c:/Users/hee/Desktop/Cardio")
#Packages
library(tidyverse)
library(lubridate)
library(tableone)

#subgroup=0
Data1.0 <- read_csv("data_1.0.csv")
#Numerator
Data1.0<-Data1.0 %>%
  rowwise() %>% 
  mutate(numerator = sum(c_across(`BMI`:`LDLC_Frail`))) %>% ungroup() 

#Denominator 1
Data1.0<-Data1.0 %>% mutate(denominator_1=40) 

#Create Frailty Index
Data1.0<-Data1.0 %>% mutate(FI_1=numerator/denominator_1)   

#cut-points
Data1.0<-Data1.0 %>%mutate(FI_1_cutpoints = ntile(FI_1, 4))
quantile(Data1.0$FI_1)
#TableOne
##Overall (Table 1 for Publication)
### Table One
vars<-c("Age",     
        "Gender",
        "Ethnicity",
        "BMI_RAW",
        "SystolicBP",
        "DiastolicBP",
        "smoking_status",
        "Hypertension",
        "Diabetes",
        "Dyslipidaemia",
        "IHD",
        "AMI",
        "CVA_or_TIA",
        "AF",
        "CCF",
        "COPD",
        "Asthma",
        "PVD",
        "Cancer_status",
        "OSA",
        "Alcohol_usage",
        "PreviousPCI",
        "PreviousCABG",
        "EjectionFraction",
        "CoronaryVesselsinvolved",
        "eGFR",
        "Hb",
        "Na",
        "TW",
        "NeutValue",
        "NeutPercent",
        "HbA1c",
        "LDLC",
        "CardiacDeath",
        "InHospitalDeath",
        "Bleeding_complication",
        "SubsequentCoros",
        "SubsequentCCF",
        "SubsequentMI",
        "SubsequentStrokeTIA")
factorVars<-c("Gender",       
              "Ethnicity",
              "smoking_status",
              "Hypertension",
              "Diabetes",
              "Dyslipidaemia",
              "IHD",
              "AMI",
              "CVA_or_TIA",
              "AF",
              "CCF",
              "COPD",
              "Asthma",
              "PVD",
              "Cancer_status",
              "OSA",
              "Alcohol_usage",
              "PreviousPCI",
              "PreviousCABG",
              "CoronaryVesselsinvolved",
              "CardiacDeath",
              "InHospitalDeath",
              "Bleeding_complication",
              "SubsequentCoros",
              "SubsequentCCF",
              "SubsequentMI",
              "SubsequentStrokeTIA")
#Gender
Data1.0<-Data1.0 %>% mutate(Gendergroup=case_when((Gender=="Male")~1,
                                              (Gender=="Female")~0,
))
Data1.0<-Data1.0 %>% mutate(CardiacDeath1=case_when((CardiacDeath=="0")~"No",
                                                (CardiacDeath=="1")~"Yes",
))
Data1.0<-Data1.0 %>% mutate(InHospitalDeath1=case_when((InHospitalDeath=="0")~"No",
                                                   (InHospitalDeath=="1")~"Yes",
))
Data1.0<-Data1.0 %>% mutate(Composite_Outcome1=case_when((Composite_Outcome=="0")~"No",
                                                     (Composite_Outcome=="1")~"Yes",
))

library(xgboost)
library(tidyverse)
library(skimr)
library(caret)
library(pROC)
#xgboost
#data1.0
skim(Data1.0)
Data1.0$Ethnicity <- factor(Data1.0$Ethnicity)
Data1.0$Gender <- factor(Data1.0$Gender)
Data1.0$smoking_status <- factor(Data1.0$smoking_status)
Data1.0$Cancer_status <- factor(Data1.0$Cancer_status)
Data1.0$CardiacDeath1 <- factor(Data1.0$CardiacDeath1)
Data1.0$InHospitalDeath1 <- factor(Data1.0$InHospitalDeath1)
Data1.0$Composite_Outcome1 <- factor(Data1.0$Composite_Outcome1)
table(Data1.0$CardiacDeath1)
table(Data1.0$InHospitalDeath1)
table(Data1.0$Composite_Outcome1)

data2.0 <- Data1.0[c("Age","Gender","FI_1_cutpoints","CardiacDeath1","InHospitalDeath1","Composite_Outcome1")]  
data2.0
#Oversampling
#CardiacDeath
prop.table(table(data2.0$CardiacDeath1))
data_balanced_overCD2.0 <- ovun.sample(CardiacDeath1 ~ ., data = data2.0, method = "over",N = 2188)$data
table(data_balanced_overCD2.0$CardiacDeath1)

#InHospitalDeath
prop.table(table(data2$InHospitalDeath1))
data_balanced_overID2.0 <- ovun.sample(InHospitalDeath1 ~ ., data = data2.0, method = "over",N = 2084)$data
table(data_balanced_overID2.0$InHospitalDeath1)

#Composite_Outcome
prop.table(table(data2$Composite_Outcome1))
data_balanced_overCO2.0 <- ovun.sample(Composite_Outcome1 ~ ., data = data2.0, method = "over",N = 1726)$data
table(data_balanced_overCO2.0$Composite_Outcome1)
#CardiacDeath
set.seed(42)
trains <- createDataPartition(
  y=data_balanced_overCD2.0$CardiacDeath1,
  p=0.7,
  list=F
)
trains2 <- sample(trains,nrow(data_balanced_overCD2.0)*0.7)
valids <- setdiff(trains,trains2)
data_train <- data_balanced_overCD2.0[trains2,]
data_valid <- data_balanced_overCD2.0[valids,]
data_test <- data_balanced_overCD2.0[-trains,]
table(data_train$CardiacDeath1)
table(data_valid$CardiacDeath1)
table(data_test$CardiacDeath1)
#Data Processing
colnames(data_balanced_overCD2.0)
# training set
form_cls <- as.formula(
  paste0(
    "CardiacDeath1 ~ Age+Gender+FI_1_cutpoints"
  ))
dvfunc <- dummyVars(~., data = data_train[, 1:3], fullRank = T)
data_trainx <- predict(dvfunc, newdata = data_train[, 1:3])
data_trainy <- ifelse(data_train$CardiacDeath1 == "No", 0, 1)
# valid set
data_validx <- predict(dvfunc, newdata = data_valid[, 1:3])
data_validy <- ifelse(data_valid$CardiacDeath1 == "No", 0, 1)
# test set
data_testx <- predict(dvfunc, newdata = data_test[, 1:3])
data_testy <- ifelse(data_test$CardiacDeath1 == "No", 0, 1)
# xgb.DMatrix
dtrain <- xgb.DMatrix(data = data_trainx, label = data_trainy)
dvalid <- xgb.DMatrix(data = data_validx, label = data_validy)
dtest <- xgb.DMatrix(data = data_testx, label = data_testy)
watchlist <- list(train = dtrain, test = dvalid)
# Training Model
fit_xgb_cls <- xgb.train(
  data = dtrain,
  eta = 0.3,
  gamma = 0.001,
  max_depth = 1,
  subsample = 0.7,
  colsample_bytree = 0.4,
  objective = "binary:logistic",
  nrounds = 1000,
  watchlist = watchlist,
  verbose = 1,
  print_every_n = 100,
  early_stopping_rounds = 200
)

# Model Summary
fit_xgb_cls
# importance
importance_matrix <- xgb.importance(model = fit_xgb_cls)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    measure = "Cover")
# SHAP
xgb.plot.shap(data = data_trainx,
              model = fit_xgb_cls,
              top_n = 3)

#predict
trainpredprob <- predict(fit_xgb_cls,
                         newdata = dtrain)
trainroc <- roc(response=data_train$CardiacDeath1,
                predictor=trainpredprob)
plot(trainroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(trainroc)

bestp <- trainroc$thresholds[
  which.max(trainroc$sensitivities+trainroc$specificities - 1)
]
bestp
#Training set prediction classification
trainpredlab <- as.factor(ifelse(trainpredprob > bestp, "Yes", "No"))
#Training set confusion matrix
confusionMatrix(data = trainpredlab,
                reference = data_train$CardiacDeath1,
                positive = "Yes",
                mode = "everything")
#Test set prediction probability
testpredprob <- predict(fit_xgb_cls,newdata = dtest)
testpredlab <- as.factor(ifelse(testpredprob > bestp,"Yes","No"))
confusionMatrix(data = testpredlab,
                reference = data_test$CardiacDeath1,
                positive = "Yes",
                mode = "everything")
#test-roc
testroc <- roc(response=data_test$CardiacDeath1,
               predictor=testpredprob)
plot(testroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(testroc)
plot(trainroc,
     print.auc=TRUE,
     grid=c(0.1,0.2),
     auc.polygon=F,
     max.auc.polygon=T,
     main="ROC",
     grid.col=c("green","red"))
plot(testroc,
     print.auc=TRUE,
     print.auc.y=0.4,
     add=T,
     col="red")
legend("bottomright",
       legend = c("traindata","testdata"),
       col = c(par("fg"),"red"),
       lwd=2,
       cex=0.9)
#InHospitalDeath
set.seed(42)
trains <- createDataPartition(
  y=data_balanced_overID2.0$InHospitalDeath1,
  p=0.7,
  list=F
)
trains2 <- sample(trains,nrow(data_balanced_overID2.0)*0.7)
valids <- setdiff(trains,trains2)
data_train <- data_balanced_overID2.0[trains2,]
data_valid <- data_balanced_overID2.0[valids,]
data_test <- data_balanced_overID2.0[-trains,]
table(data_train$InHospitalDeath1)
table(data_valid$InHospitalDeath1)
table(data_test$InHospitalDeath1)
#data processing
colnames(data_balanced_overID2.0)
# training set
form_cls <- as.formula(
  paste0(
    "InHospitalDeath1 ~ Age+Gender+FI_1_cutpoints"
  ))
dvfunc <- dummyVars(~., data = data_train[, 1:3], fullRank = T)
data_trainx <- predict(dvfunc, newdata = data_train[, 1:3])
data_trainy <- ifelse(data_train$InHospitalDeath1 == "No", 0, 1)
# valid set
data_validx <- predict(dvfunc, newdata = data_valid[, 1:3])
data_validy <- ifelse(data_valid$InHospitalDeath1 == "No", 0, 1)
# test set
data_testx <- predict(dvfunc, newdata = data_test[, 1:3])
data_testy <- ifelse(data_test$InHospitalDeath1 == "No", 0, 1)
# xgb.DMatrix
dtrain <- xgb.DMatrix(data = data_trainx, label = data_trainy)
dvalid <- xgb.DMatrix(data = data_validx, label = data_validy)
dtest <- xgb.DMatrix(data = data_testx, label = data_testy)
watchlist <- list(train = dtrain, test = dvalid)
# model training
fit_xgb_cls <- xgb.train(
  data = dtrain,
  eta = 0.3,
  gamma = 0.001,
  max_depth = 1,
  subsample = 0.7,
  colsample_bytree = 0.4,
  objective = "binary:logistic",
  nrounds = 1000,
  watchlist = watchlist,
  verbose = 1,
  print_every_n = 100,
  early_stopping_rounds = 200
)

# model summary
fit_xgb_cls
# importance
importance_matrix <- xgb.importance(model = fit_xgb_cls)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    measure = "Cover")
# SHAP
xgb.plot.shap(data = data_trainx,
              model = fit_xgb_cls,
              top_n = 3)

#predict
trainpredprob <- predict(fit_xgb_cls,
                         newdata = dtrain)
trainroc <- roc(response=data_train$InHospitalDeath1,
                predictor=trainpredprob)
plot(trainroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(trainroc)

bestp <- trainroc$thresholds[
  which.max(trainroc$sensitivities+trainroc$specificities - 1)
]
bestp
#Training set prediction classification
trainpredlab <- as.factor(ifelse(trainpredprob > bestp, "Yes", "No"))
#Training set confusion matrix
confusionMatrix(data = trainpredlab,
                reference = data_train$InHospitalDeath1,
                positive = "Yes",
                mode = "everything")
#Test set prediction probability
testpredprob <- predict(fit_xgb_cls,newdata = dtest)
testpredlab <- as.factor(ifelse(testpredprob > bestp,"Yes","No"))
confusionMatrix(data = testpredlab,
                reference = data_test$InHospitalDeath1,
                positive = "Yes",
                mode = "everything")
#test-roc
testroc <- roc(response=data_test$InHospitalDeath1,
               predictor=testpredprob)
plot(testroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(testroc)
plot(trainroc,
     print.auc=TRUE,
     grid=c(0.1,0.2),
     auc.polygon=F,
     max.auc.polygon=T,
     main="ROC",
     grid.col=c("green","red"))
plot(testroc,
     print.auc=TRUE,
     print.auc.y=0.4,
     add=T,
     col="red")
legend("bottomright",
       legend = c("traindata","testdata"),
       col = c(par("fg"),"red"),
       lwd=2,
       cex=0.9)
#Composite_Outcome
set.seed(42)
trains <- createDataPartition(
  y=data_balanced_overCO2.0$Composite_Outcome1,
  p=0.7,
  list=F
)
trains2 <- sample(trains,nrow(data_balanced_overCO2.0)*0.7)
valids <- setdiff(trains,trains2)
data_train <- data_balanced_overCO2.0[trains2,]
data_valid <- data_balanced_overCO2.0[valids,]
data_test <- data_balanced_overCO2.0[-trains,]
table(data_train$Composite_Outcome1)
table(data_valid$Composite_Outcome1)
table(data_test$Composite_Outcome1)
#data processing
colnames(data_balanced_overCO2.0)
# training set
form_cls <- as.formula(
  paste0(
    "Composite_Outcome1 ~ Age+Gender+FI_1_cutpoints"
  ))
dvfunc <- dummyVars(~., data = data_train[, 1:3], fullRank = T)
data_trainx <- predict(dvfunc, newdata = data_train[, 1:3])
data_trainy <- ifelse(data_train$Composite_Outcome1 == "No", 0, 1)
# valid set
data_validx <- predict(dvfunc, newdata = data_valid[, 1:3])
data_validy <- ifelse(data_valid$Composite_Outcome1 == "No", 0, 1)
# test set
data_testx <- predict(dvfunc, newdata = data_test[, 1:3])
data_testy <- ifelse(data_test$Composite_Outcome1 == "No", 0, 1)
# xgb.DMatrix
dtrain <- xgb.DMatrix(data = data_trainx, label = data_trainy)
dvalid <- xgb.DMatrix(data = data_validx, label = data_validy)
dtest <- xgb.DMatrix(data = data_testx, label = data_testy)
watchlist <- list(train = dtrain, test = dvalid)
# model training
fit_xgb_cls <- xgb.train(
  data = dtrain,
  eta = 0.3,
  gamma = 0.001,
  max_depth = 2,
  subsample = 0.7,
  colsample_bytree = 0.4,
  objective = "binary:logistic",
  nrounds = 1000,
  watchlist = watchlist,
  verbose = 1,
  print_every_n = 100,
  early_stopping_rounds = 200
)

# model summary
fit_xgb_cls
# importance
importance_matrix <- xgb.importance(model = fit_xgb_cls)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    measure = "Cover")
# SHAP
xgb.plot.shap(data = data_trainx,
              model = fit_xgb_cls,
              top_n = 3)

#predict
trainpredprob <- predict(fit_xgb_cls,
                         newdata = dtrain)
trainroc <- roc(response=data_train$Composite_Outcome1,
                predictor=trainpredprob)
plot(trainroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(trainroc)

bestp <- trainroc$thresholds[
  which.max(trainroc$sensitivities+trainroc$specificities - 1)
]
bestp
#Training set prediction classification
trainpredlab <- as.factor(ifelse(trainpredprob > bestp, "Yes", "No"))
#Training set confusion matrix
confusionMatrix(data = trainpredlab,
                reference = data_train$Composite_Outcome1,
                positive = "Yes",
                mode = "everything")
#Test set prediction probability
testpredprob <- predict(fit_xgb_cls,newdata = dtest)
testpredlab <- as.factor(ifelse(testpredprob > bestp,"Yes","No"))
confusionMatrix(data = testpredlab,
                reference = data_test$Composite_Outcome1,
                positive = "Yes",
                mode = "everything")
#test-roc
testroc <- roc(response=data_test$Composite_Outcome1,
               predictor=testpredprob)
plot(testroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(testroc)
plot(trainroc,
     print.auc=TRUE,
     grid=c(0.1,0.2),
     auc.polygon=F,
     max.auc.polygon=T,
     main="ROC",
     grid.col=c("green","red"))
plot(testroc,
     print.auc=TRUE,
     print.auc.y=0.4,
     add=T,
     col="red")
legend("bottomright",
       legend = c("traindata","testdata"),
       col = c(par("fg"),"red"),
       lwd=2,
       cex=0.9)

Data1.1 <- read_csv("data_1.1.csv")
#Numerator
Data1.1<-Data1.1 %>%
  rowwise() %>% 
  mutate(numerator = sum(c_across(`BMI`:`LDLC_Frail`))) %>% ungroup() 

#Denominator 1
Data1.1<-Data1.1 %>% mutate(denominator_1=40) 

#Create Frailty Index
Data1.1<-Data1.1 %>% mutate(FI_1=numerator/denominator_1)   

#cut-points
Data1.1<-Data1.1 %>%mutate(FI_1_cutpoints = ntile(FI_1, 4))
quantile(Data1.1$FI_1)
#TableOne
##Overall (Table 1 for Publication)
### Table One
vars<-c("Age",     
        "Gender",
        "Ethnicity",
        "BMI_RAW",
        "SystolicBP",
        "DiastolicBP",
        "smoking_status",
        "Hypertension",
        "Diabetes",
        "Dyslipidaemia",
        "IHD",
        "AMI",
        "CVA_or_TIA",
        "AF",
        "CCF",
        "COPD",
        "Asthma",
        "PVD",
        "Cancer_status",
        "OSA",
        "Alcohol_usage",
        "PreviousPCI",
        "PreviousCABG",
        "EjectionFraction",
        "CoronaryVesselsinvolved",
        "eGFR",
        "Hb",
        "Na",
        "TW",
        "NeutValue",
        "NeutPercent",
        "HbA1c",
        "LDLC",
        "CardiacDeath",
        "InHospitalDeath",
        "Bleeding_complication",
        "SubsequentCoros",
        "SubsequentCCF",
        "SubsequentMI",
        "SubsequentStrokeTIA")
factorVars<-c("Gender",       
              "Ethnicity",
              "smoking_status",
              "Hypertension",
              "Diabetes",
              "Dyslipidaemia",
              "IHD",
              "AMI",
              "CVA_or_TIA",
              "AF",
              "CCF",
              "COPD",
              "Asthma",
              "PVD",
              "Cancer_status",
              "OSA",
              "Alcohol_usage",
              "PreviousPCI",
              "PreviousCABG",
              "CoronaryVesselsinvolved",
              "CardiacDeath",
              "InHospitalDeath",
              "Bleeding_complication",
              "SubsequentCoros",
              "SubsequentCCF",
              "SubsequentMI",
              "SubsequentStrokeTIA")
#Gender
Data1.1<-Data1.1 %>% mutate(Gendergroup=case_when((Gender=="Male")~1,
                                                  (Gender=="Female")~0,
))
Data1.1<-Data1.1 %>% mutate(CardiacDeath1=case_when((CardiacDeath=="0")~"No",
                                                    (CardiacDeath=="1")~"Yes",
))
Data1.1<-Data1.1 %>% mutate(InHospitalDeath1=case_when((InHospitalDeath=="0")~"No",
                                                       (InHospitalDeath=="1")~"Yes",
))
Data1.1<-Data1.1 %>% mutate(Composite_Outcome1=case_when((Composite_Outcome=="0")~"No",
                                                         (Composite_Outcome=="1")~"Yes",
))

library(xgboost)
library(tidyverse)
library(skimr)
library(caret)
library(pROC)
#xgboost
#data1.0
skim(Data1.1)
Data1.1$Ethnicity <- factor(Data1.1$Ethnicity)
Data1.1$Gender <- factor(Data1.1$Gender)
Data1.1$smoking_status <- factor(Data1.1$smoking_status)
Data1.1$Cancer_status <- factor(Data1.1$Cancer_status)
Data1.1$CardiacDeath1 <- factor(Data1.1$CardiacDeath1)
Data1.1$InHospitalDeath1 <- factor(Data1.1$InHospitalDeath1)
Data1.1$Composite_Outcome1 <- factor(Data1.1$Composite_Outcome1)
table(Data1.1$CardiacDeath1)
table(Data1.1$InHospitalDeath1)
table(Data1.1$Composite_Outcome1)
install.packages("mFLICA")
library(leadership)
data2.1 <- Data1.1[c("Age","Gender","FI_1_cutpoints","CardiacDeath1","InHospitalDeath1","Composite_Outcome1")]  
data2.1
#Oversampling
#CardiacDeath
prop.table(table(data2.1$CardiacDeath1))
data_balanced_overCD2.1 <- ovun.sample(CardiacDeath1 ~ ., data = data2.1, method = "over",N = 1098)$data
table(data_balanced_overCD2.1$CardiacDeath1)

#InHospitalDeath
prop.table(table(data2.1$InHospitalDeath1))
data_balanced_overID2.1 <- ovun.sample(InHospitalDeath1 ~ ., data = data2.1, method = "over",N = 1074)$data
table(data_balanced_overID2.1$InHospitalDeath1)

#Composite_Outcome
prop.table(table(data2.1$Composite_Outcome1))
data_balanced_overCO2.1 <- ovun.sample(Composite_Outcome1 ~ ., data = data2.1, method = "over",N = 908)$data
table(data_balanced_overCO2.1$Composite_Outcome1)

#CardiacDeath
set.seed(42)
trains <- createDataPartition(
  y=data_balanced_overCD2.1$CardiacDeath1,
  p=0.7,
  list=F
)
trains2 <- sample(trains,nrow(data_balanced_overCD2.1)*0.7)
valids <- setdiff(trains,trains2)
data_train <- data_balanced_overCD2.1[trains2,]
data_valid <- data_balanced_overCD2.1[valids,]
data_test <- data_balanced_overCD2.1[-trains,]
table(data_train$CardiacDeath1)
table(data_valid$CardiacDeath1)
table(data_test$CardiacDeath1)
#Data Processing
colnames(data_balanced_overCD2.1)
# training set
form_cls <- as.formula(
  paste0(
    "CardiacDeath1 ~ Age+Gender+FI_1_cutpoints"
  ))
dvfunc <- dummyVars(~., data = data_train[, 1:3], fullRank = T)
data_trainx <- predict(dvfunc, newdata = data_train[, 1:3])
data_trainy <- ifelse(data_train$CardiacDeath1 == "No", 0, 1)
# valid set
data_validx <- predict(dvfunc, newdata = data_valid[, 1:3])
data_validy <- ifelse(data_valid$CardiacDeath1 == "No", 0, 1)
# test set
data_testx <- predict(dvfunc, newdata = data_test[, 1:3])
data_testy <- ifelse(data_test$CardiacDeath1 == "No", 0, 1)
# xgb.DMatrix
dtrain <- xgb.DMatrix(data = data_trainx, label = data_trainy)
dvalid <- xgb.DMatrix(data = data_validx, label = data_validy)
dtest <- xgb.DMatrix(data = data_testx, label = data_testy)
watchlist <- list(train = dtrain, test = dvalid)
# Training Model
fit_xgb_cls <- xgb.train(
  data = dtrain,
  eta = 0.3,
  gamma = 0.001,
  max_depth = 2,
  subsample = 0.7,
  colsample_bytree = 0.4,
  objective = "binary:logistic",
  nrounds = 1000,
  watchlist = watchlist,
  verbose = 1,
  print_every_n = 100,
  early_stopping_rounds = 200
)

# Model Outline
fit_xgb_cls
# importance
importance_matrix <- xgb.importance(model = fit_xgb_cls)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    measure = "Cover")
# SHAP
xgb.plot.shap(data = data_trainx,
              model = fit_xgb_cls,
              top_n = 3)

#predict
trainpredprob <- predict(fit_xgb_cls,
                         newdata = dtrain)
trainroc <- roc(response=data_train$CardiacDeath1,
                predictor=trainpredprob)
plot(trainroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(trainroc)

bestp <- trainroc$thresholds[
  which.max(trainroc$sensitivities+trainroc$specificities - 1)
]
bestp
#Training set prediction classification
trainpredlab <- as.factor(ifelse(trainpredprob > bestp, "Yes", "No"))
#Training set confusion matrix
confusionMatrix(data = trainpredlab,
                reference = data_train$CardiacDeath1,
                positive = "Yes",
                mode = "everything")
#Test set prediction probability
testpredprob <- predict(fit_xgb_cls,newdata = dtest)
testpredlab <- as.factor(ifelse(testpredprob > bestp,"Yes","No"))
confusionMatrix(data = testpredlab,
                reference = data_test$CardiacDeath1,
                positive = "Yes",
                mode = "everything")
#test-roc
testroc <- roc(response=data_test$CardiacDeath1,
               predictor=testpredprob)
plot(testroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(testroc)
plot(trainroc,
     print.auc=TRUE,
     grid=c(0.1,0.2),
     auc.polygon=F,
     max.auc.polygon=T,
     main="ROC",
     grid.col=c("green","red"))
plot(testroc,
     print.auc=TRUE,
     print.auc.y=0.4,
     add=T,
     col="red")
legend("bottomright",
       legend = c("traindata","testdata"),
       col = c(par("fg"),"red"),
       lwd=2,
       cex=0.9)
#InHospitalDeath
set.seed(42)
trains <- createDataPartition(
  y=data_balanced_overID2.1$InHospitalDeath1,
  p=0.7,
  list=F
)
trains2 <- sample(trains,nrow(data_balanced_overID2.1)*0.7)
valids <- setdiff(trains,trains2)
data_train <- data_balanced_overID2.1[trains2,]
data_valid <- data_balanced_overID2.1[valids,]
data_test <- data_balanced_overID2.1[-trains,]
table(data_train$InHospitalDeath1)
table(data_valid$InHospitalDeath1)
table(data_test$InHospitalDeath1)
#Data Processing
colnames(data_balanced_overID2.1)
# training set
form_cls <- as.formula(
  paste0(
    "InHospitalDeath1 ~ Age+Gender+FI_1_cutpoints"
  ))
dvfunc <- dummyVars(~., data = data_train[, 1:3], fullRank = T)
data_trainx <- predict(dvfunc, newdata = data_train[, 1:3])
data_trainy <- ifelse(data_train$InHospitalDeath1 == "No", 0, 1)
# valid set
data_validx <- predict(dvfunc, newdata = data_valid[, 1:3])
data_validy <- ifelse(data_valid$InHospitalDeath1 == "No", 0, 1)
# test set
data_testx <- predict(dvfunc, newdata = data_test[, 1:3])
data_testy <- ifelse(data_test$InHospitalDeath1 == "No", 0, 1)
# xgb.DMatrix
dtrain <- xgb.DMatrix(data = data_trainx, label = data_trainy)
dvalid <- xgb.DMatrix(data = data_validx, label = data_validy)
dtest <- xgb.DMatrix(data = data_testx, label = data_testy)
watchlist <- list(train = dtrain, test = dvalid)
# Training Model
fit_xgb_cls <- xgb.train(
  data = dtrain,
  eta = 0.3,
  gamma = 0.001,
  max_depth = 2,
  subsample = 0.7,
  colsample_bytree = 0.4,
  objective = "binary:logistic",
  nrounds = 1000,
  watchlist = watchlist,
  verbose = 1,
  print_every_n = 100,
  early_stopping_rounds = 200
)

# Model Outline
fit_xgb_cls
# importance
importance_matrix <- xgb.importance(model = fit_xgb_cls)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    measure = "Cover")
# SHAP
xgb.plot.shap(data = data_trainx,
              model = fit_xgb_cls,
              top_n = 3)

#predict
trainpredprob <- predict(fit_xgb_cls,
                         newdata = dtrain)
trainroc <- roc(response=data_train$InHospitalDeath1,
                predictor=trainpredprob)
plot(trainroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(trainroc)

bestp <- trainroc$thresholds[
  which.max(trainroc$sensitivities+trainroc$specificities - 1)
]
bestp
#Training set prediction classification
trainpredlab <- as.factor(ifelse(trainpredprob > bestp, "Yes", "No"))
#Training set confusion matrix
confusionMatrix(data = trainpredlab,
                reference = data_train$InHospitalDeath1,
                positive = "Yes",
                mode = "everything")
#Test set prediction probability
testpredprob <- predict(fit_xgb_cls,newdata = dtest)
testpredlab <- as.factor(ifelse(testpredprob > bestp,"Yes","No"))
confusionMatrix(data = testpredlab,
                reference = data_test$InHospitalDeath1,
                positive = "Yes",
                mode = "everything")
#test-roc
testroc <- roc(response=data_test$InHospitalDeath1,
               predictor=testpredprob)
plot(testroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(testroc)
plot(trainroc,
     print.auc=TRUE,
     grid=c(0.1,0.2),
     auc.polygon=F,
     max.auc.polygon=T,
     main="ROC",
     grid.col=c("green","red"))
plot(testroc,
     print.auc=TRUE,
     print.auc.y=0.4,
     add=T,
     col="red")
legend("bottomright",
       legend = c("traindata","testdata"),
       col = c(par("fg"),"red"),
       lwd=2,
       cex=0.9)
#Composite_Outcome
set.seed(42)
trains <- createDataPartition(
  y=data_balanced_overCO2.1$Composite_Outcome1,
  p=0.7,
  list=F
)
trains2 <- sample(trains,nrow(data_balanced_overCO2.1)*0.7)
valids <- setdiff(trains,trains2)
data_train <- data_balanced_overCO2.1[trains2,]
data_valid <- data_balanced_overCO2.1[valids,]
data_test <- data_balanced_overCO2.1[-trains,]
table(data_train$Composite_Outcome1)
table(data_valid$Composite_Outcome1)
table(data_test$Composite_Outcome1)
#data processing
colnames(data_balanced_overCO2.1)
# training set
form_cls <- as.formula(
  paste0(
    "Composite_Outcome1 ~ Age+Gender+FI_1_cutpoints"
  ))
dvfunc <- dummyVars(~., data = data_train[, 1:3], fullRank = T)
data_trainx <- predict(dvfunc, newdata = data_train[, 1:3])
data_trainy <- ifelse(data_train$Composite_Outcome1 == "No", 0, 1)
# valid set
data_validx <- predict(dvfunc, newdata = data_valid[, 1:3])
data_validy <- ifelse(data_valid$Composite_Outcome1 == "No", 0, 1)
# test set
data_testx <- predict(dvfunc, newdata = data_test[, 1:3])
data_testy <- ifelse(data_test$Composite_Outcome1 == "No", 0, 1)
# xgb.DMatrix
dtrain <- xgb.DMatrix(data = data_trainx, label = data_trainy)
dvalid <- xgb.DMatrix(data = data_validx, label = data_validy)
dtest <- xgb.DMatrix(data = data_testx, label = data_testy)
watchlist <- list(train = dtrain, test = dvalid)
# model training
fit_xgb_cls <- xgb.train(
  data = dtrain,
  eta = 0.3,
  gamma = 0.001,
  max_depth = 1,
  subsample = 0.7,
  colsample_bytree = 0.4,
  objective = "binary:logistic",
  nrounds = 1000,
  watchlist = watchlist,
  verbose = 1,
  print_every_n = 200,
  early_stopping_rounds = 100
)

# model summary
fit_xgb_cls
# importance
importance_matrix <- xgb.importance(model = fit_xgb_cls)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,
                    measure = "Cover")
# SHAP
xgb.plot.shap(data = data_trainx,
              model = fit_xgb_cls,
              top_n = 3)

#predict
trainpredprob <- predict(fit_xgb_cls,
                         newdata = dtrain)
trainroc <- roc(response=data_train$Composite_Outcome1,
                predictor=trainpredprob)
plot(trainroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(trainroc)

bestp <- trainroc$thresholds[
  which.max(trainroc$sensitivities+trainroc$specificities - 1)
]
bestp
#Training set prediction classification
trainpredlab <- as.factor(ifelse(trainpredprob > bestp, "Yes", "No"))
#Training set confusion matrix
confusionMatrix(data = trainpredlab,
                reference = data_train$Composite_Outcome1,
                positive = "Yes",
                mode = "everything")
#Test set prediction probability
testpredprob <- predict(fit_xgb_cls,newdata = dtest)
testpredlab <- as.factor(ifelse(testpredprob > bestp,"Yes","No"))
confusionMatrix(data = testpredlab,
                reference = data_test$Composite_Outcome1,
                positive = "Yes",
                mode = "everything")
#test-roc
testroc <- roc(response=data_test$Composite_Outcome1,
               predictor=testpredprob)
plot(testroc,
     print.auc=TRUE,
     auc.polygon=TRUE,
     grid=T,
     max.auc.polygon=T,
     auc.polygon.col="skyblue",
     print.thres=T,
     legacy.axes=T,
     bty="l")
ci.auc(testroc)
plot(trainroc,
     print.auc=TRUE,
     grid=c(0.1,0.2),
     auc.polygon=F,
     max.auc.polygon=T,
     main="ROC",
     grid.col=c("green","red"))
plot(testroc,
     print.auc=TRUE,
     print.auc.y=0.4,
     add=T,
     col="red")
legend("bottomright",
       legend = c("traindata","testdata"),
       col = c(par("fg"),"red"),
       lwd=2,
       cex=0.9)







