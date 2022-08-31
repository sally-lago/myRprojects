View(Survival_Data)
# Loading package
library(survival)
library(survminer)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(ranger)
library(gbm)
library(caret)


# Fitting the survival model
Survival_Function<-(Surv(Survival_Data$time,Survival_Data$DEATH_EVENT==1))
Survival_Function
# Plotting the function
plot(Survival_Function)

class(Survival_Function)
Survival_Function

# Fitting a Survival Curve

genfit<-survfit(Surv(time,DEATH_EVENT)~1,data=Survival_Data)
Summary(genfit)
sufit<-survfit(Surv(time,DEATH_EVENT)~sex,data=Survival_Data)
summary(sufit)

# constructing life tables
range(Survival_Data$time)
seq(0,300,by=50)
summary(sufit,times =seq(0,300,by=15) )

# Fittimg the Kaplan Meier curve for age groups and sex
library(survminer)
sufit<-survfit(Surv(time,DEATH_EVENT)~sex,data=Survival_Data)
select.event<-filter(Survival_Data,DEATH_EVENT==1)
select.event
sfit
sufit<-survfit(Surv(time,DEATH_EVENT==1)~sex,data=Survival_Data)
summary(sufit)
ggsurvplot(sufit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Female","Male"), legend.title="Sex",  
           palette=c("dodgerblue2", "orchid2"), 
           title="Kaplan-Meier Curve for Heart Failure Survival", 
           risk.table.height=.15)

sfit<-Survival_Data %>%
  mutate(age = ifelse((age < 60),"LT60","OV60"),
              age=as.factor(age))
agefit<-survfit(Surv(time,DEATH_EVENT==1)~age,data=sfit)
summary(agefit)
autoplot(agefit)
autoplo

# Fitting a Cox model
cox <-coxph(Surv(time,DEATH_EVENT==1)~.,data=Survival_Data)
cox
cox<-coxph(Surv(time, DEATH_EVENT==1)~age+anaemia+log(creatinine_phosphokinase)+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking,data=Survival_Data)
cox
summary(cox)
cox_fit <- survfit(cox)
cox_fit
autoplot(cox_fit)
ggforest(cox,data=sfit)

#Fitting aelens additive model
aelen.fit <-aareg(Surv(time, DEATH_EVENT) ~age+anaemia+log(creatinine_phosphokinase)+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking,data=sfit)
aelen.fit
autoplot(aelen.fit)
# Checking the model assumptions

#proportionality
library(survival)
library(survminer)
test.prop<-cox.zph(cox)
test.prop
ggcoxzph(test.prop)

#linearity
ggcoxfunctional()

#influential observation
ggcoxdiagnostics(cox,type="deviance",linear.predictions=FALSE)

library(caret)
install.packages("caret")

#4.Prediction
#partion data
set.seed(323)
ind<-sample(2,nrow(Survival_Data),replace = T,prob=c(0.7,0.3))
train<-Survival_Data[ind==1,]
test<-Survival_Data[ind==2,]

library(ranger)
rf<-randomForest()