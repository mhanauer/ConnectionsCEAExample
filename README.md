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
ER and Hospital Costs Data cleaning
Grab just the variables that we want.  
(Make sure this is true) The problem is that we do not know if the NAs in the times variables are actually NAs, because if the client says they have never been to the hosptial or ER then they get an NA for the times variable associated with that the non-times variable.  

We first checked if there were any actual NAs in the each of the times variables that were actual NAs (i.e. they put NA on the non-times variables).  Then we made all the NAs Zero, because the remaining NAs should only be zeros.
```{r}
GPRA_wide_MHcC<- GPRA_wide[c("ERAlcoholSATimes.x","ERAlcoholSATimes.y",  "ERMentalTimes.x", "ERMentalTimes.y", "ERPhysicalTimes.x", "ERPhysicalTimes.y", "InpatientMentalNights.x", "InpatientMentalNights.y", "InpatientAlcoholSANights.x", "InpatientAlcoholSANights.y", "InpatientPhysicalNights.x", "InpatientPhysicalNights.y", "OutpatientMentalTimes.x", "OutpatientMentalTimes.y", "OutpatientAlcoholSATimes.x", "OutpatientAlcoholSATimes.y", "OutpatientPhysicalTimes.x", "OutpatientPhysicalTimes.y")]
GPRA_wide_MHcC[is.na(GPRA_wide_MHcC)]<-0
GPRA_wide_data_MHcC = na.omit(GPRA_wide_MHcC)
dim(GPRA_wide_data_MHcC)
```
ER and Hospital Calculations

ER and Hosp
WISQARS age range 21 to 67 everything else included.
Took the average of the five intents that it included: unitentiaional, sexual assualt, other assualt, self-harm, legal intervention: https://wisqars.cdc.gov:8443/costT/cost_Part1_Finished.jsp
This include work loss costs not sure if that should be included have a low rate of working.

Adjust for inflation: https://meps.ahrq.gov/about_meps/Price_Index.shtml#t1a3

Multiple the number the base year (2010) with the actual year (2017)

ER first
```{r}

### Need age 
range(GPRA_wide$Age.x)
##Total ER visits for base

Total_ERvisitsBase<-sum(GPRA_wide_data_MHcC$ERAlcoholSATimes.x, GPRA_wide_data_MHcC$ERMentalTimes.x, GPRA_wide_data_MHcC$ERPhysicalTimes.x)

Total_ERvisitsMonth6<-sum(GPRA_wide_data_MHcC$ERAlcoholSATimes.y, GPRA_wide_data_MHcC$ERMentalTimes.y, GPRA_wide_data_MHcC$ERPhysicalTimes.y)
Total_ERvisitsMonth6

## Using 8, because we are looking at the sum of visits to the ER.  There were 8 less visits from base to 6 to months and $6,139
mean(6139,8540,6716,4823,6484)
Tot_ERMoneySaved<-mean(6139,8540,6716,4823,6484)*(Total_ERvisitsBase-Total_ERvisitsMonth6) 
### Adjust for inflation for health care for 2017
inflat = 106.073/95.705
Tot_ERMoneySaved = Tot_ERMoneySaved*inflat
Tot_ERMoneySaved
```
Inpat
Inpat costs: https://www.kff.org/health-costs/state-indicator/expenses-per-inpatient-day/?currentTimeframe=0&selectedRows=%7B%22states%22:%7B%22indiana%22:%7B%7D%7D%7D&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
```{r}
##Total Inpatient Nights for base
Total_inpat_base = sum(GPRA_wide_data_MHcC$InpatientMentalNights.x, GPRA_wide_data_MHcC$InpatientAlcoholSANights.x, GPRA_wide_data_MHcC$InpatientMentalNights.x)
Total_inpat_base

Total_inpat_month6 = sum(GPRA_wide_data_MHcC$InpatientMentalNights.y, GPRA_wide_data_MHcC$InpatientAlcoholSANights.y, GPRA_wide_data_MHcC$InpatientPhysicalNights.y)
Total_inpat_month6

inpat_costs= 2508
TotalInpatMoneySaved = (Total_inpat_base - Total_inpat_month6)*inpat_costs*inflat
TotalInpatMoneySaved
```
Outpat
Price per outpat visit: $183 in 1995 dollars: https://www.ncbi.nlm.nih.gov/pubmed/10156484
```{r}
##Total Outpatient Nights for base
print(Outpt_MentalBase_tot<- sum(GPRA_wide_data_MHcC$OutpatientMentalTimes.x))
print(Outpt_AlcoholBase_tot<- sum(GPRA_wide_data_MHcC$OutpatientAlcoholSATimes.x))
print(Total_OutptVisitsBase<-sum(Outpt_MentalBase_tot,Outpt_AlcoholBase_tot))

Total_outpat_base = sum(GPRA_wide_data_MHcC$OutpatientAlcoholSATimes.x, GPRA_wide_data_MHcC$OutpatientMentalTimes.x, GPRA_wide_data_MHcC$OutpatientPhysicalTimes.x)

Total_outpat_month6 = sum(GPRA_wide_data_MHcC$OutpatientAlcoholSATimes.y, GPRA_wide_data_MHcC$OutpatientMentalTimes.y, GPRA_wide_data_MHcC$OutpatientPhysicalTimes.y)

inflat_95 = 106.073 / 73.346

out_save = 183

TotalOutMoneySaved = (Total_outpat_base - Total_outpat_month6)* out_save*inflat_95
TotalOutMoneySaved
```


Incarerated 
```{r}
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
print(Incarceration_TotCosts<-(Incarceration_costs*200))

```
Income
```{r}
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

```
CEA
```{r}
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

