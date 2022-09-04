install.packages("xgboost")
library(xgboost)
library(tidyverse)
library(skimr)
library(caret)
library(pROC)
#randomforest
#Gender
data1<-data1 %>% mutate(Gendergroup=case_when((Gender=="Male")~1,
                                              (Gender=="Female")~0,
))
data1<-data1 %>% mutate(CardiacDeath1=case_when((CardiacDeath=="0")~"No",
                                                (CardiacDeath=="1")~"Yes",
))
data1<-data1 %>% mutate(InHospitalDeath1=case_when((InHospitalDeath=="0")~"No",
                                                   (InHospitalDeath=="1")~"Yes",
))
data1<-data1 %>% mutate(Composite_Outcome1=case_when((Composite_Outcome=="0")~"No",
                                                     (Composite_Outcome=="1")~"Yes",
))
skim(data1)
data1
data1$Ethnicity <- factor(data1$Ethnicity)
data1$Gender <- factor(data1$Gender)
data1$smoking_status <- factor(data1$smoking_status)
data1$Cancer_status <- factor(data1$Cancer_status)
data1$CardiacDeath1 <- factor(data1$CardiacDeath1)
data1$InHospitalDeath1 <- factor(data1$InHospitalDeath1)
data1$Composite_Outcome1 <- factor(data1$Composite_Outcome1)
table(data1$CardiacDeath1)
table(data1$InHospitalDeath1)
table(data1$Composite_Outcome1)
install.packages("mFLICA")
library(leadership)
data2 <- data1[c("Age","Gender","FI_1_cutpoints","CardiacDeath1","InHospitalDeath1","Composite_Outcome1")]  
data2
#Oversampling
#CardiacDeath
prop.table(table(data2$CardiacDeath1))
data_balanced_overCD2 <- ovun.sample(CardiacDeath1 ~ ., data = data2, method = "over",N = 3296)$data
table(data_balanced_overCD2$CardiacDeath1)

#InHospitalDeath
prop.table(table(data2$InHospitalDeath1))
data_balanced_overID2 <- ovun.sample(InHospitalDeath1 ~ ., data = data2, method = "over",N = 3168)$data
table(data_balanced_overID2$InHospitalDeath1)

#Composite_Outcome
prop.table(table(data2$Composite_Outcome1))
data_balanced_overCO2 <- ovun.sample(Composite_Outcome1 ~ ., data = data2, method = "over",N = 2636)$data
table(data_balanced_overCO2$Composite_Outcome1)
#CardiacDeath
set.seed(42)
trains <- createDataPartition(
  y=data_balanced_overCD2$CardiacDeath1,
  p=0.8,
  list=F
)
trains2 <- sample(trains,nrow(data_balanced_overCD2)*0.7)
valids <- setdiff(trains,trains2)
data_train <- data_balanced_overCD2[trains2,]
data_valid <- data_balanced_overCD2[valids,]
data_test <- data_balanced_overCD2[-trains,]
table(data_train$CardiacDeath1)
table(data_valid$CardiacDeath1)
table(data_test$CardiacDeath1)
#Data Processing
colnames(data_balanced_overCD2)
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
#xgb.DMatrix
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
  y=data_balanced_overID2$InHospitalDeath,
  p=0.8,
  list=F
)
trains2 <- sample(trains,nrow(data_balanced_overID2)*0.7)
valids <- setdiff(trains,trains2)
data_train <- data_balanced_overID2[trains2,]
data_valid <- data_balanced_overID2[valids,]
data_test <- data_balanced_overID2[-trains,]
table(data_train$InHospitalDeath1)
table(data_valid$InHospitalDeath1)
table(data_test$InHospitalDeath1)
#Data Processing
colnames(data_balanced_overID2)
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
  y=data_balanced_overCO2$Composite_Outcome1,
  p=0.8,
  list=F
)
trains2 <- sample(trains,nrow(data_balanced_overCO2)*0.7)
valids <- setdiff(trains,trains2)
data_train <- data_balanced_overCO2[trains2,]
data_valid <- data_balanced_overCO2[valids,]
data_test <- data_balanced_overCO2[-trains,]
table(data_train$Composite_Outcome1)
table(data_valid$Composite_Outcome1)
table(data_test$Composite_Outcome1)
#data processing
colnames(data_balanced_overCO2)
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
