---
title: "ConnectionsStudy"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Load stats packages
```{r}
library(Amelia)
library(prettyR)
library(nlme)
library(descr)
library(foreign)
library(lme4)
library(sjstats)
library(MissMech)
library(BaylorEdPsych)
library(ggplot2)
library(HLMdiag)
library(psych)
library(MuMIn)
```
Data cleaning

First we need to matched pairs so we subset the base and 6month followup and match on those with 6month follow-up data. 
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/ConnectionsPaperData")
GPRAAll = read.csv("ConnGPRA.csv", header = TRUE, na.strings = c(-9, -8, -7, -1, "", " ", "NULL", NULL, NA, "NA")) 
GPRAAll_base = subset(GPRAAll, GPRAAll$InterviewType == 1)
dim(GPRAAll_base)
GPRAAll_six = subset(GPRAAll, GPRAAll$InterviewType == 2)
dim(GPRAAll_six)
GPRAAll_six$ClientID
GPRAAll_base$ClientID

### Only want those with 6-month data
GPRA_wide = merge(GPRAAll_base, GPRAAll_six, by = "ClientID", all.y = TRUE)
dim(GPRA_wide)
dim(GPRAAll_six)

## Double check ids are the same
GPRAAll_six$ClientID  == GPRA_wide$ClientID


print(median(GPRA_wide$Age.x))
print(median(GPRA_wide$Age.y))

##Grant was $2,000,000 with 186 total people in the program. Following analysis on 105 of them


##Cost per Person
print(cost_per_person <- 2000000/186)

##Example with ER Visits: https://wisqars.cdc.gov:8443/costT/cost_Part1_Finished.jsp

##Healthcare Costs
##105 observations

```
ER and Hospital Costs
```{r}
GPRA_wide_MHcC<- GPRA_wide[c("ERAlcoholSA.x", "ERAlcoholSATimes.x", "ERAlcoholSA.y","ERAlcoholSATimes.y", "ERMental.x",  "ERMentalTimes.x", "ERMental.y", "ERMentalTimes.y", "InpatientMental.x", "InpatientMentalNights.x", "InpatientMental.y", "InpatientMentalNights.y", "InpatientAlcoholSA.x", "InpatientAlcoholSANights.x", "InpatientAlcoholSA.y", "InpatientAlcoholSANights.y", "OutpatientMental.x", "OutpatientMentalTimes.x", "OutpatientMental.y", "OutpatientMentalTimes.y", "OutpatientAlcoholSA.x", "OutpatientAlcoholSATimes.x", "OutpatientAlcoholSA.y", "OutpatientAlcoholSATimes.y")]
GPRA_wide_MHcC[is.na(GPRA_wide_MHcC)]<-0
GPRA_wide_data_MHcC = na.omit(GPRA_wide_MHcC)

##Total ER visits for base
print(ER_AlcoholBase_tot<- sum(GPRA_wide_data_MHcC$ERAlcoholSATimes.x))
print(ER_MentalBase_tot<- sum(GPRA_wide_data_MHcC$ERMentalTimes.x))
print(Total_ERvisitsBase<-sum(ER_AlcoholBase_tot,ER_MentalBase_tot))

##Total ER visits for 6mo
print(ER_AlcoholSix_tot<- sum(GPRA_wide_data_MHcC$ERAlcoholSATimes.y))
print(ER_MentalSix_tot<- sum(GPRA_wide_data_MHcC$ERMentalTimes.y))
print(Total_ERvisitsSix<-sum(ER_AlcoholSix_tot,ER_MentalSix_tot))

##Percent change in ER visits
print(perc_change<- ((Total_ERvisitsSix-Total_ERvisitsBase)/Total_ERvisitsBase)*100) ##44.4% reduction
#print(ER_money_saved<-3486*0.444) ##Saved $1,548 per patient in ER visit costs
print(Tot_ERMoneySaved<-3486*8) ##Saved $309,557 total

##Total Inpatient Nights for base
print(Inpt_MentalBase_tot<- sum(GPRA_wide_data_MHcC$InpatientMentalNights.x))
print(Inpt_AlcoholBase_tot<- sum(GPRA_wide_data_MHcC$InpatientAlcoholSANights.x))
print(Total_InptVisitsBase<-sum(Inpt_MentalBase_tot,Inpt_AlcoholBase_tot))

##Total Inpatient Nights for 6mo
print(Inpt_MentalSix_tot<- sum(GPRA_wide_data_MHcC$InpatientMentalNights.y))
print(Inpt_AlcoholSix_tot<- sum(GPRA_wide_data_MHcC$InpatientAlcoholSANights.y))
print(Total_InptVisitsSix<-sum(Inpt_MentalSix_tot,Inpt_AlcoholSix_tot))

##Percent change in Inpatient Nights
print(perc_change<- ((Total_InptVisitsSix-Total_InptVisitsBase)/Total_InptVisitsBase)*100) ##42.5% reduction

##Total Outpatient Nights for base
print(Outpt_MentalBase_tot<- sum(GPRA_wide_data_MHcC$OutpatientMentalTimes.x))
print(Outpt_AlcoholBase_tot<- sum(GPRA_wide_data_MHcC$OutpatientAlcoholSATimes.x))
print(Total_OutptVisitsBase<-sum(Outpt_MentalBase_tot,Outpt_AlcoholBase_tot))

##Total Outpatient Nights for 6mo
print(Outpt_MentalSix_tot<- sum(GPRA_wide_data_MHcC$OutpatientMentalTimes.y))
print(Outpt_AlcoholSix_tot<- sum(GPRA_wide_data_MHcC$OutpatientAlcoholSATimes.y))
print(Total_OutptVisitsSix<-sum(Outpt_MentalSix_tot,Outpt_AlcoholSix_tot))

##Percent change in Outpatient Nights
print(perc_change<- ((Total_OutptVisitsSix-Total_OutptVisitsBase)/Total_OutptVisitsBase)*100) ##59.5% reduction

##Total Hospitalization Nights for base
print(Total_HospitalizationsBase<-sum(Total_InptVisitsBase,Total_OutptVisitsBase))

##Total Hospitalization Nights for 6mo
print(Total_HospitalizationsSix<-sum(Total_InptVisitsSix,Total_OutptVisitsSix))

##Percent change in Total Hospitalization Nights
print(Hospitalizations_change<- ((Total_HospitalizationsSix-Total_HospitalizationsBase)/Total_HospitalizationsBase)*100) ##54.2% reduction
#print(InPt_money_saved<-34456*0.542) ##Saved $19,882.73 per patient in ER visit costs 
## Should grab the inflation rate do the math in R.
print(Tot_InPtMoneySaved<-34456*(253-116)) ##Saved $3,976,546 total

##Incarceration/Criminal Costs
##104 observations

GPRA_wide_Crime<- GPRA_wide[c("ArrestedConfineDays.x", "ArrestedConfineDays.y", "DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y","NrCrimes.x", "NrCrimes.y", "ArrestedDays.x", "ArrestedDays.y")]
GPRA_wide_data_Crime = na.omit(GPRA_wide_Crime)


##Total Days Incarcerated for base
print(IncarceratedBase_tot<- sum(GPRA_wide_data_Crime$ArrestedConfineDays.x))

##Total Days Incarcerated for 6mo
print(IncarceratedSix_tot<- sum(GPRA_wide_data_Crime$ArrestedConfineDays.y))

##Percent change in Days Incarcerated
print(Incarcerated_change<- ((IncarceratedSix_tot-IncarceratedBase_tot)/IncarceratedBase_tot)*100) ##170% increase

##Costs of Incarceration
print(Incarceration_costs<-(101.88*1.7))
## Should be 105, because we only have 105 people in the data set
print(Incarceration_TotCosts<-(Incarceration_costs*105))


##Mean Income Wage for base
print(IncomeBase<- mean(GPRA_wide_data_EduEmp$IncomeWages.x))

##Mean Income Wage for 6mo
print(IncomeSix<- mean(GPRA_wide_data_EduEmp$IncomeWages.y))

##Percent change in Mean Income Wage
print(Income_change<- ((IncomeSix-IncomeBase)/IncomeBase)*100) ##83.4% increase

##Mean Pub Assist Income for base
print(PubAssistIncomeBase<- mean(GPRA_wide_data_EduEmp$IncomePubAssist.x))

##Mean Pub Assist Income Income for 6mo
print(PubAssistIncomeSix<- mean(GPRA_wide_data_EduEmp$IncomePubAssist.y))

##Percent change in Mean Pub Assist Income  
print(PubAssistIncome_change<- ((PubAssistIncomeSix-PubAssistIncomeBase)/PubAssistIncomeBase)*100) ##19.8% increase

##Mean Disability Income for base
print(DisabilityIncomeBase<- mean(GPRA_wide_data_EduEmp$IncomeDisability.x))

##Mean Disability Income for 6mo
print(DisabilityIncomeSix<- mean(GPRA_wide_data_EduEmp$IncomeDisability.y))

##Percent change in Mean Disability Income 
print(DisabilityIncome_change<- ((DisabilityIncomeSix-DisabilityIncomeBase)/DisabilityIncomeBase)*100) ##106.2% increase

##Total Income for base
print(TotIncomeBase<- sum(IncomeBase, PubAssistIncomeBase, DisabilityIncomeBase))

##Total Income for 6mo
print(TotIncomeSix<- sum(IncomeSix, PubAssistIncomeSix, DisabilityIncomeSix))

##Percent change in Total Income 
print(TotIncome_change<- ((TotIncomeSix-TotIncomeBase)/TotIncomeBase)*100) ##69% increase
print(Income_diff<- (TotIncomeSix-TotIncomeBase)) ##$121 increase
print(Tot_Income_diff<-(Income_diff*200)) ##$24,200 increase overall

GPRA_wide_EnuffMoney<- GPRA_wide[c("EnoughMoneyForNeeds.x", "EnoughMoneyForNeeds.y")]
GPRA_wide_data_EnuffMoney = na.omit(GPRA_wide_EnuffMoney)
##48 Observations

##Median Enough Money for Needs for base
print(EnuffMoneyBase<- median(GPRA_wide_data_EnuffMoney$EnoughMoneyForNeeds.x))

##Median Enough Money for Needs for 6mo
print(EnuffMoneySix<- median(GPRA_wide_data_EnuffMoney$EnoughMoneyForNeeds.y))

##Percent change in Median Enough Money for Needs
print(Education_change<- ((EnuffMoneySix-EnuffMoneyBase)/EnuffMoneyBase)*100) ##No change -> "Not at All"

#Non-monetary Outputs

GPRA_wide_Depression<- GPRA_wide[c("Depression.x", "Depression.y")]
GPRA_wide_data_Depression = na.omit(GPRA_wide_Depression)
##103 Observations

##Total Days Depressed for base
print(DepressedBase_tot<- mean(GPRA_wide_data_Depression$Depression.x))

##Total Days Depressed for 6mo
print(DepressedSix_tot<- mean(GPRA_wide_data_Depression$Depression.y))

##Percent change in Days Depressed
print(Depressed_change<- ((DepressedSix_tot-DepressedBase_tot)/DepressedBase_tot)*100) ##8.9% reduction

GPRA_wide_Anxiety<- GPRA_wide[c("Anxiety.x", "Anxiety.y")]
GPRA_wide_data_Anxiey = na.omit(GPRA_wide_Anxiety)
##103 Observations

##Total Days Anxious for base
print(AnxietyBase_tot<- mean(GPRA_wide_data_Anxiey$Anxiety.x))

##Total Days Anxious for 6mo
print(AnxietySix_tot<- mean(GPRA_wide_data_Anxiey$Anxiety.y))

##Percent change in Days Anxious
print(Anxiety_change<- ((AnxietySix_tot-AnxietyBase_tot)/AnxietyBase_tot)*100) ##10.3% reduction

GPRA_wide_Suicide<- GPRA_wide[c("Suicide.x", "Suicide.y")]
GPRA_wide_data_Suicide = na.omit(GPRA_wide_Suicide)
##104 Observations

##Total Days Anxious for base
print(SuicideBase_tot<- sum(GPRA_wide_data_Suicide$Suicide.x))

##Total Days Anxious for 6mo
print(SuicideSix_tot<- sum(GPRA_wide_data_Suicide$Suicide.y))

##Percent change in Days Anxious
print(Anxiety_change<- ((SuicideSix_tot-SuicideBase_tot)/SuicideBase_tot)*100) ##63.6% reduction

GPRA_wide_PsycEmo<- GPRA_wide[c("PsycholEmotImpact.x", "PsycholEmotImpact.y")]
GPRA_wide_data_PsycEmo = na.omit(GPRA_wide_PsycEmo)
##92 Observations

##Psych/Emo for base
print(PsycEmoBase<- median(GPRA_wide_data_PsycEmo$PsycholEmotImpact.x))

##Psych/Emo for 6mo
print(PsycEmoSix<- median(GPRA_wide_data_PsycEmo$PsycholEmotImpact.y))

##Percent change in Psych/Emo 
print(PsycEmo_change<- ((PsycEmoSix-PsycEmoBase)/PsycEmoBase)*100) ##25% decrease; i.e. change from "considerably" to "moderately"

```


