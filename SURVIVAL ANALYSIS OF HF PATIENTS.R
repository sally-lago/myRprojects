View(Survival_Data)
# Loading package
library(survival)
library(survminer)

#Overal descriptives statistics
Meanage<-mean(Survival_Data$age) #mean age of patients
Male_total<-sum(Survival_Data$sex==1)
Female_total<-sum(Survival_Data$sex==0)
MaleDeath_events<-sum(Survival_Data$DEATH_EVENT==1&Survival_Data$sex==1)
FemaleDeath_events<-sum(Survival_Data$DEATH_EVENT==1&Survival_Data$sex==0)

# Creating the survival object
Survival_Function<-Surv(Survival_Data$time,Survival_Data$DEATH_EVENT==1)
Survival_Function
# Plotting the function
plot(Survival_Function)

# Fitting a Survival Curve
genfit<-survfit(Surv(time,DEATH_EVENT)~1,data=Survival_Data)
summary(genfit)
#survival probabilities by sex
sufit<-survfit(Surv(time,DEATH_EVENT)~sex,data=Survival_Data)
summary(sufit)
View(Survival_Data)
# Fittimg the Kaplan Meier curve for age groups and sex
library(survminer)
sufit<-survfit(Surv(time,DEATH_EVENT)~sex,data=Survival_Data)
sufit<-survfit(Surv(time,DEATH_EVENT==0)~sex,data=Survival_Data)
summary(sufit)
HBP<-survfit(Surv(time,DEATH_EVENT==0)~high_blood_pressure,data=Survival_Data)
Anaemia<-survfit(Surv(time,DEATH_EVENT==0)~anaemia,data=Survival_Data)

ggsurvplot(HBP, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Normal","High"), legend.title="Blood Pressure",  
           palette=c("Kaplan", "Yellow"), ggtheme=theme_bw(),risk.table.col="strata",
           title="Kaplan-Meier Curve for Heart Failure Survival", 
           risk.table.height=.15)

ggsurvplot(sufit, conf.int=TRUE, pval=TRUE, risk.table=TRUE, 
           legend.labs=c("Female","Male"), legend.title="Sex",  
           palette=c("Blue", "Yellow"), ggtheme=theme_bw(),risk.table.col="strata",
           title="Kaplan-Meier Curve for Heart Failure Survival", 
           risk.table.height=.15)
ggsurvplot(Anaemia, conf.int=TRUE, pval=TRUE, risk.table=TRUE,risk.table.col="strata",linetype = "strata",
           legend.labs=c("Anaemic","Non Anaemic"), legend.title="Anaemia",  
           palette=c("Blue", "Yellow"),surv.meadian.line="hv",ggtheme=theme_bw(),
           title="Kaplan-Meier Curve for Heart Failure Survival",)


# Fitting a Cox model
cox <-coxph(Surv(time,DEATH_EVENT==1)~.,data=Survival_Data)
summary(cox)

cox<-coxph(Surv(time, DEATH_EVENT==1)~age+anaemia+log(creatinine_phosphokinase)+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking,data=Survival_Data)
cox
#testing cox model assumptions in R
test.ph <- cox.zph(cox) #proportionality assumptions
test.ph
ggcoxzph(test.ph)
ggcoxdiagnostics(cox, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw()) #outlying observations
ggcoxfunctional(cox,data = Survival_Data) #linearity assumptions

remedy_cox<-coxph(Surv(time, DEATH_EVENT==1)~age+anaemia+(creatinine_phosphokinase)^2+diabetes+ejection_fraction+high_blood_pressure+platelets+(serum_creatinine)^2+serum_sodium+sex+smoking,data=Survival_Data)
remedy_cox
ggcoxfunctional(remedy_cox,data = Survival_Data)
summary(remedy_cox)
#Fitting aelens additive model
aelen.fit <-aareg(Surv(time, DEATH_EVENT) ~age+anaemia+log(creatinine_phosphokinase)+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking,data=sfit)
aelen.fit

#Plotting a forest plot
ggforest(remedy_cox,data=Survival_Data)


#Forest Plot with only the significant variables
sigvar_cox<-coxph(Surv(time, DEATH_EVENT==1)~age+anaemia+(creatinine_phosphokinase)^2+ejection_fraction+high_blood_pressure+(serum_creatinine)^2+serum_sodium,data=Survival_Data)
summary(sigvar_cox)

ggforest(sigvar_cox,data=Survival_Data)

