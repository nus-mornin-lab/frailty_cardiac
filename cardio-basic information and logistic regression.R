#Import dataset
setwd(dir = "c:/Users/hee/Desktop/Cardio")
library(readxl)
Data <- read_excel("cardio0208.xlsx")
#Packages
library(tidyverse)
library(lubridate)
library(tableone)
##Confirm dates - not really necessary
Data$ProcedureDate<-ymd(Data$ProcedureDate)
Data$Dummydate<-ymd(Data$Dummydate)

####################################################################################
#Create Score Components
##BMI
Data<-Data %>% mutate(BMI_RAW=Weight/(Height/100)^2)
Data<-Data %>% mutate(BMI=case_when((BMI_RAW>=27.5) ~ 2,
                                    (BMI_RAW>=23 & BMI_RAW<27.5) | (BMI_RAW<18.5) ~ 1,
                                    (BMI_RAW>=18.5 & BMI_RAW<23) ~ 0))
##BP
Data<-Data %>% mutate(SysBP=if_else(SystolicBP>130,1,0))
Data<-Data %>% mutate(DiasBP=if_else(DiastolicBP>80,1,0))
##Ejection fraction
Data<-Data %>% mutate(EF=if_else(EjectionFraction>=50,1,0))

##eGFR
Data<-Data %>% mutate(eGFR_Frail=case_when((eGFR>=60) ~ 0,
                                           (eGFR<60 & eGFR>=45) ~ 1,
                                           (eGFR<45 & eGFR>=30) ~ 2,
                                           (eGFR<30) ~ 3))
##Hb
Data<-Data %>% mutate(Hb_Frail=if_else((Hb<13 & Gender=="Male") | (Hb<12 & Gender=="Female"),1,0))


##Na
Data<-Data %>% mutate(Na_Frail=if_else((Na<135 | Na>145),1,0))


##smoker
Data<-Data %>% mutate(Smoker_Frail=case_when((Smoker==1) ~ 2,
                                             (Smoker==2) ~ 1,
                                             (Smoker==3) ~ 0
))
##COPD and Asthma
Data<-Data %>% mutate(COPD=if_else(COPDorAsthma==1,1,0))
Data<-Data %>% mutate(Asthma=if_else(COPDorAsthma==2,1,0))

##TW
Data<-Data %>% mutate(TW_Frail=if_else((TW<4 | TW>10.01),1,0))

#Neut
Data<-Data %>% mutate(NeutValue_Frail=if_else(NeutValue>6.27,1,0))

#Neut%
Data<-Data %>% mutate(NeutPercent_Frail=if_else(NeutPercent>60,1,0))

#HbA1c
Data<-Data %>% mutate(HbA1c_Frail=case_when((HbA1c<7) ~ 0,
                                            (HbA1c>=7 & HbA1c<=8) ~ 1,
                                            (HbA1c>8) ~ 2
))

#LDLC
Data<-Data %>% mutate(LDLC_Frail=case_when((LDLC<1.8)~0,
                                           (LDLC>=1.8 & LDLC<=4) ~ 1,
                                           (LDLC>4) ~ 2
))

#Cancer
Data<-Data %>% mutate(Metastasis_Frail=if_else(Metastasis_cancer==1,1,0))
Data<-Data %>% replace_na(list(Metastasis_Frail=0))
Data<-Data %>% mutate(Cancer_Frail=case_when((Cancer==0) ~ 0,
                                             (Cancer==1 & (Metastasis_cancer==0 | Metastasis_Frail==0)) ~ 1,
                                             (Cancer==1 & Metastasis_cancer==1) ~ 2
))

Data<-Data %>% mutate(smoking_status=case_when((Smoker_Frail==2)~"Current Smoker",
                                               (Smoker_Frail==1)~"Ex-Smoker",
                                               (Smoker_Frail==0)~"Non-Smoker"))
Data<-Data %>% mutate(Cancer_status=case_when((Cancer_Frail==2)~"metastatic cancer",
                                              (Cancer_Frail==1)~"non-metastatic cancer",
                                              (Cancer_Frail==0)~"no cancer"))
#Composite Adverse Event Outcome
Data<-Data %>% mutate(Composite_Outcome=if_else(Bleeding_complication==1 | SubsequentCoros==1 | SubsequentCCF==1 | SubsequentMI==1 | SubsequentStrokeTIA==1,1,0))

##############################################################
#Selecting variables
Cardio<-Data %>% select(`Age`,
                        `Gender`,
                        `Ethnicity`,
                        `BMI`,
                        `SysBP`,
                        `DiasBP`,
                        `Smoker_Frail`,
                        `Hypertension`,
                        `Diabetes`,
                        `Dyslipidaemia`,
                        `IHD`,
                        `AMI`,
                        `CVA_or_TIA`,
                        `AF`,
                        `CCF`,
                        `COPD`,
                        `Asthma`,
                        `PVD`,
                        `Cancer_Frail`,
                        `OSA`,
                        `Alcohol_usage`,
                        `PreviousPCI`,
                        `PreviousCABG`,
                        `EF`,
                        `CoronaryVesselsinvolved`,
                        `eGFR_Frail`,
                        `Hb_Frail`,
                        `Na_Frail`,
                        `TW_Frail`,
                        `NeutValue_Frail`,
                        `NeutPercent_Frail`,
                        `HbA1c_Frail`,
                        `LDLC_Frail`,
                        `CardiacDeath`,
                        `InHospitalDeath`,
                        `Bleeding_complication`,
                        `SubsequentCoros`,
                        `SubsequentCCF`,
                        `SubsequentMI`,
                        `SubsequentStrokeTIA`,
                        "Composite_Outcome",
                        "BMI_RAW",
                        "SystolicBP",
                        "DiastolicBP",
                        "smoking_status",
                        "Cancer_status",
                        "EjectionFraction",
                        "eGFR",
                        "Hb",
                        "Na",
                        "TW",
                        "NeutValue",
                        "NeutPercent",
                        "HbA1c",
                        "LDLC")

#Missing Data Analysis
library(finalfit)
Cardio %>% ff_glimpse()
library(mice)
str(Cardio)

#missing data
imp <- mice(Cardio,seed=8888,meth = "cart", minbucket = 4)
plot(imp)
complete(imp)
data1 <- complete(imp)
summary(data1)
write.csv(data1,"data_1.csv")
summary(is.na(data1))

install.packages("ROSE")
library(ROSE)

#Numerator
data1<-data1 %>%
  rowwise() %>% 
  mutate(numerator = sum(c_across(`BMI`:`LDLC_Frail`))) %>% ungroup() 

#Denominator 1
data1<-data1 %>% mutate(denominator_1=40) 

#Create Frailty Index
data1<-data1 %>% mutate(FI_1=numerator/denominator_1)   

#cut-points
data1<-data1 %>%mutate(FI_1_cutpoints = ntile(FI_1, 4))
quantile(data1$FI_1)

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
A<-print(tableOne<-CreateTableOne(vars=vars, strata=c("FI_1_cutpoints"), data=data1, factorVars=factorVars))
write.csv(A,"FI_1.csv")
B<-print(tableOne<-CreateTableOne(vars=vars, data=data1, factorVars=factorVars))
write.csv(B,"overall.csv")
#Binary Variable Distribution
#1. age group
data1$Agegp <- ifelse( data1$Age<65, "Age <65", "Age >=65")
data1$Agegpn <- ifelse( data1$Age<65, 0, 1)
p1<-ggplot(data1, aes(x=Agegp))+geom_bar(fill="lightblue")+ labs(x="Age Group")+ theme_minimal(base_size=10)

#2. Gender
p2<-ggplot(data1, aes(x=Gender))+geom_bar(fill="indianred3")+ labs(x="Gender")+ theme_minimal(base_size=10)

#3. Ethnicity
p3<-ggplot(data1, aes(x=Ethnicity))+geom_bar(fill="seagreen2")+ labs(x="Ethnicity")+ theme_minimal(base_size=10)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
#4. FI_1_cutpoints
p4<-ggplot(data1, aes(x=FI_1_cutpoints))+geom_bar(fill="orange2")+ labs(x="Frailty Index")+ theme_minimal(base_size=10)
install.packages("patchwork")
library(patchwork)
(p1+p2+p3+p4)+plot_annotation(title="Demographic and Frailty Index Distribution")
#Continuous Variables Disbribution
#1. Age
c1<- ggplot(data1, aes(x=Age))+ geom_histogram(binwidth=5, colour="white", fill="darkseagreen2", alpha=0.8)+
  geom_density(eval(bquote(aes(y=..count..*5))),colour="darkgreen", fill="darkgreen", alpha=0.3)+ scale_x_continuous(breaks=seq(40,100,10))+geom_vline(xintercept = 65, linetype="dashed")+ annotate("text", x=50, y=45, label="Age <65", size=2.5, color="dark green") + annotate("text", x=80, y=45, label="Age >= 65", size=2.5, color="dark red") +labs(title="Age Distribution") + theme_minimal(base_size = 8)
c1

#Gender
data1<-data1 %>% mutate(Gendergroup=case_when((Gender=="Male")~1,
                                              (Gender=="Female")~0,
))
#Ethnicity
data1$Ethnicitygroup <- 1 * (data1$Ethnicity %in% c('Caucasian', 'Chinese','Eurasian', 'India','Malay', 'Others','Sikh'))
#Correlations
h1<- subset(data1, select=c(Age,Gendergroup,Ethnicitygroup,FI_1_cutpoints,CardiacDeath, InHospitalDeath, Composite_Outcome))
r=cor(h1)
r
install.packages("corrplot")
library(corrplot)
corrplot(r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 90)

#Preparing for Logistic Regression
install.packages("foreign")
install.packages("rms")
install.packages("pROC")
install.packages("rmda")
install.packages("devtools")
install.packages("nricens")
install.packages("calibrate")

library(devtools)
library(foreign)
library(rms) #Logistic
library(pROC) #ROC
library(rmda) #DCA
library(calibrate)
library(nricens) #Net Reclassification Index
install_github("mdbrown/DecisionCurve")


#Method1 Random
##make this example reproducible
#set.seed(1)

##Use 80% of dataset as training set and remaining 20% as testing set
#sample <- sample(c(TRUE, FALSE), nrow(Cardio), replace=TRUE, prob=c(0.8,0.2))
#Train <- Cardio[sample, ]
#Valid <- Cardio[!sample, ] 

#C<-print(tableOne<-CreateTableOne(vars=vars, data=Train, factorVars=factorVars))
#D<-print(tableOne<-CreateTableOne(vars=vars, data=Valid, factorVars=factorVars))

#Oversampling
#CardiacDeath
prop.table(table(data1$CardiacDeath))
data_balanced_overCD1 <- ovun.sample(CardiacDeath ~ ., data = data1, method = "over",N = 3296)$data
table(data_balanced_overCD1$CardiacDeath)

#InHospitalDeath
prop.table(table(data1$InHospitalDeath))
data_balanced_overID1 <- ovun.sample(InHospitalDeath ~ ., data = data1, method = "over",N = 3168)$data
table(data_balanced_overID1$InHospitalDeath)

#Composite_Outcome
prop.table(table(data1$Composite_Outcome))
data_balanced_overCO1 <- ovun.sample(Composite_Outcome ~ ., data = data1, method = "over",N = 2636)$data
table(data_balanced_overCO1$Composite_Outcome)
#Method2 Outcome dependant

##CardiacDeath
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(data_balanced_overCD1$CardiacDeath, p = .8,
                                  list = FALSE,
                                  times = 1)
Train1 <- data_balanced_overCD1[ trainIndex,]
Valid1 <- data_balanced_overCD1[-trainIndex,]

#For supplementary
Train$HbA1c<-as.numeric(Train$HbA1c)
Train$LDLC<-as.numeric(Train$LDLC)
Valid$LDLC<-as.numeric(Valid$LDLC)
Valid$HbA1c<-as.numeric(Valid$HbA1c)
E<-print(tableOne<-CreateTableOne(vars=vars, data=Train, factorVars=factorVars))
F<-print(tableOne<-CreateTableOne(vars=vars, data=Valid, factorVars=factorVars))



table(data_balanced_over$cls)
# Logstic
##Cardiac Death
##train 
CDtrain<-glm(CardiacDeath ~ FI_1_cutpoints, family = binomial(link="logit"), x=T, 
        data = Train1)
summary(CDtrain)
exp(cbind(OR=coef(CDtrain), confint(CDtrain)))
#C-Index
library(Hmisc)
CindexCDtrain <- rcorrcens(CardiacDeath~predict(CDtrain), data = Train1)
CindexCDtrain
#ROC
probCDtrain=predict(CDtrain, Train, type="response")
Train$probCDtrain=probCDtrain
gCDtrain <- roc(CardiacDeath ~ probCDtrain, data = Train1)
plot(gCDtrain,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE CDtrain", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gCDtrain$auc
coords(gCDtrain,"best", transpose = FALSE)
ci.auc(gCDtrain)
#DCA curve
moduCDtrain1<- decision_curve(data=Train1,
                       
                       CardiacDeath ~ Age + Gender,
                       
                       family = binomial(link ='logit'),
                       
                       thresholds= seq(0,1, by = 0.01),
                       
                       confidence.intervals = 0.95)
moduCDtrain2<- decision_curve(data=Train1,
                             
                             CardiacDeath ~ FI_1_cutpoints,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)
moduCDtrain3<- decision_curve(data=Train1,
                             
                             CardiacDeath ~ FI_1_cutpoints + Age + Gender,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)

List<- list(moduCDtrain1,moduCDtrain2,moduCDtrain3)
par(pin = c(4,1.7))
plot_decision_curve(List,curve.names= c('moduCDtrain1','moduCDtrain2','moduCDtrain3'),
                   cost.benefit.axis =FALSE,col = c('red','blue','green'),
                   confidence.intervals =FALSE,standardize = FALSE)

##valid
#ROC
probCDvalid=predict(CDtrain, Valid1, type="response")
Valid$probCDvalid=probCDvalid
gCDvalid <- roc(CardiacDeath ~ probCDvalid, data = Valid1)
plot(gCDvalid,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE CDvalid", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gCDvalid$auc
coords(gCDvalid,"best", transpose = FALSE)
ci.auc(gCDvalid)
#DCA curve
moduCDvalid1<- decision_curve(data=Valid1,
                       
                       CardiacDeath ~ Age + Gender,
                       
                       family = binomial(link ='logit'),
                       
                       thresholds= seq(0,1, by = 0.01),
                       
                       confidence.intervals = 0.95)
moduCDvalid2<- decision_curve(data=Valid1,
                             
                             CardiacDeath ~ FI_1_cutpoints,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)
moduCDvalid3<- decision_curve(data=Valid1,
                             
                             CardiacDeath ~ FI_1_cutpoints + Age + Gender,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)
List<- list(moduCDvalid1,moduCDvalid2,moduCDvalid3)
par(pin = c(4,1.7))
plot_decision_curve(List,curve.names= c('moduCDvalid1','moduCDvalid2','moduCDvalid3'),
                    cost.benefit.axis =FALSE,col = c('red','blue','green'),
                    confidence.intervals =FALSE,standardize = FALSE)

##In hospital Death
set.seed(3456)
trainIndex <- createDataPartition(data_balanced_overID1$InHospitalDeath, p = .8,
                                  list = FALSE,
                                  times = 1)
Train2 <- data_balanced_overID1[ trainIndex,]
Valid2 <- data_balanced_overID1[-trainIndex,]

IDtrain<-glm(InHospitalDeath ~ FI_1_cutpoints, family = binomial(link="logit"), x=T,  
        data = Train2)
summary(IDtrain)
exp(cbind(OR=coef(IDtrain), confint(IDtrain)))

#C-Index
CindexIDtrain <- rcorrcens(InHospitalDeath~predict(IDtrain), data = Train2)
CindexIDtrain
#ROC
probIDtrain=predict(IDtrain, Train2, type="response")
Train$probIDtrain=probIDtrain
gIDtrain <- roc(InHospitalDeath ~ probIDtrain, data = Train2)
plot(gIDtrain,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gIDtrain$auc
coords(gIDtrain,"best", transpose = FALSE)
ci.auc(gIDtrain)
#DAC curve
moduIDtrain1<- decision_curve(data=Train2,
                       
                       InHospitalDeath ~ Age + Gender,
                       
                       family = binomial(link ='logit'),
                       
                       thresholds= seq(0,1, by = 0.01),
                       
                       confidence.intervals = 0.95)
moduIDtrain2<- decision_curve(data=Train2,
                             
                             InHospitalDeath ~ FI_1_cutpoints,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)
moduIDtrain3<- decision_curve(data=Train2,
                             
                             InHospitalDeath ~ FI_1_cutpoints + Age + Gender,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)

List<- list(moduIDtrain1,moduIDtrain2,moduIDtrain3)
par(pin = c(4,1.7))
plot_decision_curve(List,curve.names= c('moduIDtrain1','moduIDtrain2','moduIDtrain3'),
                    cost.benefit.axis =FALSE,col = c('red','blue','green'),
                    confidence.intervals =FALSE,standardize = FALSE)

##test
#ROC
probIDvalid=predict(IDtrain, Valid2, type="response")
Valid$probIDvalid=probIDvalid
gIDvalid <- roc(InHospitalDeath ~ probIDvalid, data = Valid2)
plot(gIDvalid,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gIDvalid$auc
coords(gIDvalid,"best", transpose = FALSE)
ci.auc(gIDvalid)
#DAC curve
moduIDvalid1<- decision_curve(data=Valid2,
                       
                       InHospitalDeath ~ Age + Gender,
                       
                       family = binomial(link ='logit'),
                       
                       thresholds= seq(0,1, by = 0.01),
                       
                       confidence.intervals = 0.95)
moduIDvalid2<- decision_curve(data=Valid2,
                             
                             InHospitalDeath ~ FI_1_cutpoints,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)
moduIDvalid3<- decision_curve(data=Valid2,
                             
                             InHospitalDeath ~ FI_1_cutpoints + Age + Gender,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)
List<- list(moduIDvalid1,moduIDvalid2,moduIDvalid3)
par(pin = c(4,1.7))
plot_decision_curve(List,curve.names= c('moduIDvalid1','moduIDvalid2','moduIDvalid3'),
                    cost.benefit.axis =FALSE,col = c('red','blue','green'),
                    confidence.intervals =FALSE,standardize = FALSE)

#Composite Outcome
set.seed(3456)
trainIndex <- createDataPartition(data_balanced_overCO1$Composite_Outcome, p = .8,
                                  list = FALSE,
                                  times = 1)
Train3 <- data_balanced_overCO1[ trainIndex,]
Valid3<- data_balanced_overCO1[-trainIndex,]

COtrain<-glm(Composite_Outcome ~ FI_1_cutpoints, family = binomial(link="logit"), x=T,  
        data = Train3)
summary(COtrain)
exp(cbind(OR=coef(COtrain), confint(COtrain)))

#C-Index
CindexCOtrain <- rcorrcens(Composite_Outcome~predict(COtrain), data = Train3)
CindexCOtrain
#ROC
probCOtrain=predict(COtrain, Train3, type="response")
Train$probCOtrain=probCOtrain
gCOtrain <- roc(Composite_Outcome ~ probCOtrain, data = Train3)
plot(gCOtrain,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gCOtrain$auc
coords(gCOtrain,"best", transpose = FALSE)
ci.auc(gCOtrain)
#DAC curve
moduCOtrain1<- decision_curve(data=Train3,
                         
                         Composite_Outcome ~ Age + Gender,
                         
                         family = binomial(link ='logit'),
                         
                         thresholds= seq(0,1, by = 0.01),
                         
                         confidence.intervals = 0.95)
moduCOtrain2<- decision_curve(data=Train3,
                             
                             Composite_Outcome ~ FI_1_cutpoints,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)
moduCOtrain3<- decision_curve(data=Train3,
                             
                             Composite_Outcome ~ FI_1_cutpoints + Age + Gender,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)
List<- list(moduCOtrain1,moduCOtrain2,moduCOtrain3)
par(pin = c(4,1.7))
plot_decision_curve(List,curve.names= c('moduCOtrain1','moduCOtrain2','moduCOtrain3'),
                    cost.benefit.axis =FALSE,col = c('red','blue','green'),
                    confidence.intervals =FALSE,standardize = FALSE)

##test
#ROC
probCOvalid=predict(COtrain, Valid3, type="response")
Valid$probCOvalid=probCOvalid
gCOvalid <- roc(Composite_Outcome ~ probCOvalid, data = Valid3)
plot(gCOvalid,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gCOvalid$auc
coords(gCOvalid,"best", transpose = FALSE)
ci.auc(gCOvalid)
#DAC curve
moduCOvalid1<- decision_curve(data=Valid3,
                          
                          Composite_Outcome ~ Age + Gender,
                          
                          family = binomial(link ='logit'),
                          
                          thresholds= seq(0,1, by = 0.01),
                          
                          confidence.intervals = 0.95)
moduCOvalid2<- decision_curve(data=Valid3,
                             
                             Composite_Outcome ~ FI_1_cutpoints,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)
moduCOvalid3<- decision_curve(data=Valid3,
                             
                             Composite_Outcome ~ FI_1_cutpoints + Age + Gender,
                             
                             family = binomial(link ='logit'),
                             
                             thresholds= seq(0,1, by = 0.01),
                             
                             confidence.intervals = 0.95)
List<- list(moduCOvalid1,moduCOvalid2,moduCOvalid3)
par(pin = c(4,1.7))
plot_decision_curve(List,curve.names= c('moduCOvalid1','moduCOvalid2','moduCOvalid3'),
                    cost.benefit.axis =FALSE,col = c('red','blue','green'),
                    confidence.intervals =FALSE,standardize = FALSE)

#Sensitivity Analysis
##CardiacDeath
CDtrains<-glm(CardiacDeath ~ FI_1_cutpoints + Age + Gender, family = binomial(link="logit"), x=T, 
        data = Train1)
summary(CDtrains)
exp(cbind(OR=coef(CDtrains), confint(CDtrains)))
#C-Index
library(Hmisc)
CindexCDtrains <- rcorrcens(CardiacDeath~predict(CDtrains), data = Train1)
CindexCDtrains
#ROC
probCDtrains=predict(CDtrains, Train1, type="response")
Train1$probCDtrains=probCDtrains
gCDtrains <- roc(CardiacDeath ~ probCDtrains, data = Train1)
plot(gCDtrains,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gCDtrains$auc
coords(gCDtrains,"best", transpose = FALSE)
ci.auc(gCDtrains)
##test
#ROC
probCDvalids=predict(CDtrains, Valid1, type="response")
Valid1$probCDvalids=probCDvalids
gCDvalids <- roc(CardiacDeath ~ probCDvalids, data = Valid1)
plot(gCDvalids,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gCDvalids$auc
coords(gCDvalids,"best", transpose = FALSE)
ci.auc(gCDvalids)

##In hospital Death
IDtrains<-glm(InHospitalDeath ~ FI_1_cutpoints + Age + Gender, family = binomial(link="logit"), x=T, 
        data = Train2)
summary(IDtrains)
exp(cbind(OR=coef(IDtrains), confint(IDtrains)))
#C-Index
CindexIDtrains <- rcorrcens(InHospitalDeath~predict(IDtrains), data = Train2)
CindexIDtrains
#ROC
probIDtrains=predict(IDtrains, Train2, type="response")
Train2$probIDtrains=probIDtrains
gIDtrains <- roc(InHospitalDeath ~ probIDtrains, data = Train2)
plot(gIDtrains,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gIDtrains$auc
coords(gIDtrains,"best", transpose = FALSE)
ci.auc(gIDtrains)

##test
#ROC
probIDvalids=predict(IDtrains, Valid2, type="response")
Valid2$probIDvalids=probIDvalids
gIDvalids <- roc(InHospitalDeath ~ probIDvalids, data = Valid2)
plot(gIDvalids,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gIDvalids$auc
coords(gIDvalids,"best", transpose = FALSE)
ci.auc(gIDvalids)

#Composite Outcome
COtrains<-glm(Composite_Outcome ~ FI_1_cutpoints + Age + Gender, family = binomial(link="logit"), x=T, 
        data = Train3)
summary(COtrains)
exp(cbind(OR=coef(COtrains), confint(COtrains)))
#C-Index
CindexCOtrains <- rcorrcens(Composite_Outcome~predict(COtrains), data = Train3)
CindexCOtrains
#ROC
probCOtrains=predict(COtrains, Train3, type="response")
Train3$probCOtrains=probCOtrains
gCOtrains <- roc(Composite_Outcome ~ probCOtrains, data = Train3)
plot(gCOtrains,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gCOtrains$auc
coords(gCOtrains,"best", transpose = FALSE)
ci.auc(gCOtrains)

##test
#ROC
probCOvalids=predict(COtrains, Valid3, type="response")
Valid3$probCOvalids=probCOvalids
gCOvalids <- roc(Composite_Outcome ~ probCOvalids, data = Valid3)
plot(gCOvalids,print.auc=TRUE, print.thres=TRUE, main = "ROC CURVE", col= "red", print.thres.col="black", identity.col="blue", identity.lty=1,identity.lwd=1)
gCOvalids$auc
coords(gCOvalids,"best", transpose = FALSE)
ci.auc(gCOvalids)

#NRI-Compare whether the inclusion of demographic variables has an impact on the quality of the model
#Train-AGE+Gender
#CardiacDeath
library(PredictABEL)
set.seed(1234)
fit_A1 <- glm(CardiacDeath~Age + Gender, data = Train1, family = binomial(link="logit"),x=TRUE)
fit_B1 <- glm(CardiacDeath~FI_1_cutpoints+Age+Gender, data = Train1, family = binomial(link="logit"),x=TRUE)
which(colnames(Train1) == 'CardiacDeath')
pstd <- fit_A1$fitted.values
pnew <- fit_B1$fitted.values
reclassification(data = Train1, cOutcome = 34,
                 predrisk1 = pstd, predrisk2 = pnew,
                 cutoff = c(0,0.50,1))

#InHospitalDeath
fit_A2 <- glm(InHospitalDeath~Age + Gender, data = Train2, family = binomial(link="logit"),x=TRUE)
fit_B2 <- glm(InHospitalDeath~FI_1_cutpoints+Age + Gender, data = Train2, family = binomial(link="logit"),x=TRUE)
which(colnames(Train2) == 'InHospitalDeath')
pstd <- fit_A2$fitted.values
pnew <- fit_B2$fitted.values
reclassification(data = Train2, cOutcome = 35,
                 predrisk1 = pstd, predrisk2 = pnew,
                 cutoff = c(0,0.50,1))
#Composite_Outcome
fit_A3 <- glm(Composite_Outcome~Age + Gender, data = Train3, family = binomial(link="logit"),x=TRUE)
fit_B3 <- glm(Composite_Outcome~FI_1_cutpoints+Age + Gender, data = Train3, family = binomial(link="logit"),x=TRUE)
which(colnames(Train3) == 'Composite_Outcome')
pstd <- fit_A3$fitted.values
pnew <- fit_B3$fitted.values
reclassification(data = Train3, cOutcome = 41,
                 predrisk1 = pstd, predrisk2 = pnew,
                 cutoff = c(0,0.50,1))
#Valid
#CardiacDeath
fit_A1.1 <- glm(CardiacDeath~Age + Gender, data = Valid1, family = binomial(link="logit"),x=TRUE)
fit_B1.1 <- glm(CardiacDeath~FI_1_cutpoints+Age + Gender, data = Valid1, family = binomial(link="logit"),x=TRUE)
which(colnames(Train1) == 'CardiacDeath')
pstd <- fit_A1.1$fitted.values
pnew <- fit_B1.1$fitted.values
reclassification(data = Valid1, cOutcome = 34,
                 predrisk1 = pstd, predrisk2 = pnew,
                 cutoff = c(0,0.50,1))

#InHospitalDeath
fit_A2.2 <- glm(InHospitalDeath~Age + Gender, data = Valid2, family = binomial(link="logit"),x=TRUE)
fit_B2.2 <- glm(InHospitalDeath~FI_1_cutpoints+Age + Gender, data = Valid2, family = binomial(link="logit"),x=TRUE)
which(colnames(Train1) == 'InHospitalDeath')
pstd <- fit_A2.2$fitted.values
pnew <- fit_B2.2$fitted.values
reclassification(data = Valid2, cOutcome = 35,
                 predrisk1 = pstd, predrisk2 = pnew,
                 cutoff = c(0,0.50,1))
#Composite_Outcome
fit_A3.3 <- glm(Composite_Outcome~Age + Gender, data = Valid3, family = binomial(link="logit"),x=TRUE)
fit_B3.3 <- glm(Composite_Outcome~FI_1_cutpoints+Age + Gender, data = Valid3, family = binomial(link="logit"),x=TRUE)
which(colnames(Train1) == 'Composite_Outcome')
pstd <- fit_A3.3$fitted.values
pnew <- fit_B3.3$fitted.values
reclassification(data = Valid3, cOutcome = 41,
                 predrisk1 = pstd, predrisk2 = pnew,
                 cutoff = c(0,0.50,1))