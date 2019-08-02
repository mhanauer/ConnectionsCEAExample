---
title: "Program BCA Template"
author: "Keith McConomy, Matt Hanauer"
date: "July 26, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

#Data cleaning

##First we need to match pairs so we subset the base and 6month follow-up and match on those with 6month follow-up data.

```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/ConnectionsPaperData")
GPRAAll = read.csv("ConnGPRA.csv", header = TRUE, na.strings = c(-9, -8, -7, -1, "", " ", "NULL", NULL, NA, "NA")) 
GPRAAll_base = subset(GPRAAll, GPRAAll$InterviewType == 1)
dim(GPRAAll_base)
GPRAAll_six = subset(GPRAAll, GPRAAll$InterviewType == 2)
dim(GPRAAll_six)
GPRAAll_six$ClientID
GPRAAll_base$ClientID
```

### Only want those with 6-month data

```{r}
GPRA_wide = merge(GPRAAll_base, GPRAAll_six, by = "ClientID", all.y = TRUE)
dim(GPRA_wide)
dim(GPRAAll_six)
```

## Double check ids are the same

```{r}
GPRAAll_six$ClientID  == GPRA_wide$ClientID
```

#Study Methodology

In order to asses the benefits and costs of this program, we split our analysis into two parts. Part I, we analyzed only the benefits and costs of the sample. Part II, we predict the benefits and costs of the program as a whole. We analyzed 3 different scenarios for Part II. All scenarios will be based off of the benefits and costs that were calculated analyzing the data in the sample. To assess the full benefits and costs of the program, our sample only contained responses from individuals that filled out both the intake and 6-month survey. In our study, that was 105 out of 186 (56% of participants). The remaining 81 individuals completed the intake survey and either finished the program and did not fill out the 6-month survey, or dropped out of the program sometime within the 6-month timeframe. To account for some this variation the following 3 scenarios will be analyzed: Scenario 1, will predict the benefits and costs of the program by taking the total net benefits/costs of each variable in the sample and assuming that this would be the same for the remaining 81 participants in the program. Scenario 2, will assume a 50% decrease in observed benefits and costs for the participants that did not complete the survey. Scenario 3, will assume a 75% decrease in observed benefits and costs of the unobserved particpants. We are assuming that the program spent all 2 million dollars of the grant money that was awarded to the program, and that funding was equally spreadout among all participants in the program, regardless of retention status. 

#Healthcare 

ER and Hospital Costs Data cleaning Grab just the variables that we want.
(Make sure this is true) The problem is that we do not know if the NAs in the times variables are actually NAs, because if the client says they have never been to the hosptial or ER then they get an NA for the times variable associated with that the non-times variable.

We first checked if there were any actual NAs in the each of the times variables that were actual NAs (i.e. they put NA on the non-times variables). Then we made all the NAs Zero, because the remaining NAs should only be zeros.

```{r}
GPRA_wide_MHcC<- GPRA_wide[c("ERAlcoholSATimes.x","ERAlcoholSATimes.y",  "ERMentalTimes.x", "ERMentalTimes.y", "ERPhysicalTimes.x", "ERPhysicalTimes.y", "InpatientMentalNights.x", "InpatientMentalNights.y", "InpatientAlcoholSANights.x", "InpatientAlcoholSANights.y", "InpatientAlcoholSA.x","InpatientAlcoholSA.y", "InpatientMental.x", "InpatientMental.y", "InpatientPhysicalNights.x", "InpatientPhysicalNights.y", "OutpatientMentalTimes.x", "OutpatientMentalTimes.y", "OutpatientAlcoholSATimes.x", "OutpatientAlcoholSATimes.y", "OutpatientPhysicalTimes.x", "OutpatientPhysicalTimes.y")]
GPRA_wide_MHcC[is.na(GPRA_wide_MHcC)]<-0
GPRA_wide_data_MHcC = na.omit(GPRA_wide_MHcC)
dim(GPRA_wide_data_MHcC)
```

##Need age information 
range(GPRA_wide$Age.x)
median(GPRA_wide$Age.x)

##ER and Hospital Calculations

ER and Hosp CDC WISQARS age range 21 to 67 everything else included. Took the average of the five intents that it included: unitentiaional, sexual assualt, other assualt, self-harm, legal intervention: https://wisqars.cdc.gov:8443/costT/cost_Part1_Finished.jsp 
This cost data is on a national level rather than for only Indiana. It also includes work loss costs not sure if that should be included have a low rate of working.

Divide the inflation rate for the desired year (i.e. 2018) by the listed year's inflation rate (i.e. 2010) and multiply that by listed year's price (i.e. 2010 price of service). Go here for more help: 
**Adjust for inflation:** https://meps.ahrq.gov/about_meps/Price_Index.shtml#t1a3

Hospitalization Costs: Divided into in-patient and out-patient measures. 
In-patient costs are based off of the Kaiser Family Foundation (KFF) estimate for Indiana: https://www.kff.org/health-costs/state-indicator/expenses-per-inpatient-day/?currentTimeframe=0&selectedRows=%7B%22states%22:%7B%22indiana%22:%7B%7D%7D%7D&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
Out-patient cost data came from cost per day estimate for alcohol out-patient rehabilitation services made by the American Addiction Centers and is a national estimate: https://americanaddictioncenters.org/alcohol-rehab/cost

Many of the services' costs have been adjusted for inflation and can be found in the "NOMS Indicators" file here: https://docs.google.com/spreadsheets/d/1iFATLei8aE4SwlU0oopaYOrB2qRFWYRUSKx3N9BeAJI/edit#gid=0

##ER 

**Total ER visits for base and month 6**

```{r}
print(Total_ERvisitsBase<-sum(GPRA_wide_data_MHcC$ERAlcoholSATimes.x, GPRA_wide_data_MHcC$ERMentalTimes.x))
print(Total_ERvisitsMonth6<-sum(GPRA_wide_data_MHcC$ERAlcoholSATimes.y, GPRA_wide_data_MHcC$ERMentalTimes.y))
```

###Part I

Using 8, because we are looking at the sum of visits to the ER.  There were 8 less visits from base to 6 to months and at $3,486 per ER visit.

```{r}
print(Part1_ERsavings<-3486*(Total_ERvisitsBase-Total_ERvisitsMonth6))
```

###Part II

```{r}
print(Part2i_ERsavings<- Part1_ERsavings*1.445)
print(Part2ii_ERsavings<- Part1_ERsavings + (Part1_ERsavings*0.5*0.445))
print(Part2iii_ERsavings<- Part1_ERsavings + (Part1_ERsavings*0.25*0.445))
```

In Part I, the total savings came out to be $27,888. In Part II, Scenario 1 showed cost savings of $40,298; Scenario 2 showed cost savings of $34,093; and Scenario 3 showed cost savings of $30,991. 

##In-patient Costs

Here, we used $2,508 (2017 dollars) from the KFF data and then adjust for inflation. This is specifically for Indiana, where WISQARS is nationwide data. **Future projects should look to revising survey questions to be more tailored towards using the CDC WISQARS data.**

**Inflation rate from 2017 - 2018**

```{R}
print(inflat<- 120.745/117.816)
print(pat_costs <- 2508)
print(HospCosts<-inflat*pat_costs)
```

**Total In-patient Nights for base and month 6**

```{R}
print(Total_inpat_base <- sum(GPRA_wide_data_MHcC$InpatientMentalNights.x, GPRA_wide_data_MHcC$InpatientAlcoholSANights.x))
```

```{r}
print(Total_inpat_month6 <- sum(GPRA_wide_data_MHcC$InpatientMentalNights.y, GPRA_wide_data_MHcC$InpatientAlcoholSANights.y))
```

Within the 6 month timeframe: Total in-patient visits went from 80 to 46. **Future studies should look to try and get better cost data for in-patient costs. There is a large difference between the KFF estimate and the WISQARS estimate.**

###Part I - In-patient Savings

```{r}
print(Part1_InpatSavings <- (Total_inpat_base - Total_inpat_month6)*HospCosts) 
```

The program saved $87,392 in the sample.

###Part II - In-patient Savings

```{r}
print(Part2i_InpatSavings<-Part1_InpatSavings*1.445)
print(Part2ii_InpatSavings<-Part1_InpatSavings + (Part1_InpatSavings*0.50*0.445))
print(Part2iii_InpatSavings<-Part1_InpatSavings + (Part1_InpatSavings*0.25*0.445))
```

The program saved about $126,281 in Scenario 1, $106,837 in Scenario 2, and $97,114 in Scenario 3.

##Out-patient Costs

Here, we used data from the American Addiction Center's cost estimate on alcohol rehabilitation services, which came out to be $300. This is a national cost estimate, similar to WISQARS. **Future projects should look to revising survey questions to be more tailored towards using the CDC WISQARS data.**

**Total Out-patient Days for base and month 6**

```{r}
print(Total_outpat_base <- sum(GPRA_wide_data_MHcC$OutpatientMentalTimes.x,GPRA_wide_data_MHcC$OutpatientAlcoholSATimes.x))
```

```{r}
print(Total_outpat_month6 <- sum(GPRA_wide_data_MHcC$OutpatientAlcoholSATimes.y, GPRA_wide_data_MHcC$OutpatientMentalTimes.y))
```

Within the 6 month timeframe: Total out-patient visits went from 173 to 70. 
**Future studies should look to try and get better cost data for out-patient costs. While American Addiction Centers cost estimates may work for this study, it would be more advantagous if we could use a more "scholarly" resource.**

###Part I - Out-patient Savings

```{r}
OutpatCosts<- 300
```

```{r}
print(Part1_OutpatSavings <- (Total_outpat_base - Total_outpat_month6)*OutpatCosts) 
```

The program saved $30,900 in the sample.

###Part II - Out-patient Savings

```{r}
print(Part2i_OutpatSavings<-Part1_OutpatSavings*1.445)
print(Part2ii_OutpatSavings<-Part1_OutpatSavings + (Part1_OutpatSavings*0.50*0.445))
print(Part2iii_OutpatSavings<-Part1_OutpatSavings + (Part1_OutpatSavings*0.25*0.445))
```

The program saved about $44,651 in Scenario 1, $37,775 in Scenario 2, and $34,338 in Scenario 3.

#Income
```{r}
GPRA_wide_EduEmp<- GPRA_wide[c("EducationYears.x", "EducationYears.y", "EmployStatus.x", "EmployStatus.y", "IncomeWages.x", "IncomeWages.y", "TrainingProgram.x", "TrainingProgram.y", "IncomePubAssist.x", "IncomePubAssist.y", "IncomeDisability.x", "IncomeDisability.y")]
GPRA_wide_data_EduEmp = na.omit(GPRA_wide_EduEmp)
```

##Income based on Mean and Total Wages for base and month 6

```{r}
print(IncomeBase<- mean(GPRA_wide_data_EduEmp$IncomeWages.x))
print(IncomeBaseSUM<- sum(GPRA_wide_data_EduEmp$IncomeWages.x))
```

```{r}
print(IncomeSix<- mean(GPRA_wide_data_EduEmp$IncomeWages.y))
print(IncomeSixSUM<- sum(GPRA_wide_data_EduEmp$IncomeWages.y))
```

##Percent change in Mean and Total Wage Income

```{r}
print(Income_change<- ((IncomeSix-IncomeBase)/IncomeBase)) 
print(IncomeSUM_change<- ((IncomeSixSUM-IncomeBaseSUM)/IncomeBaseSUM)) 
```

There was a 83.4% increase in wage income. Mean wage went from $78 to $143 and the sum went from $8,194 to $15,030.

##Mean and Total Pub Assist Income for base and month 6

```{r}
print(PubAssistIncomeBase<- mean(GPRA_wide_data_EduEmp$IncomePubAssist.x))
print(PubAssistIncomeBaseSUM<- sum(GPRA_wide_data_EduEmp$IncomePubAssist.x))
```

```{r}
print(PubAssistIncomeSix<- mean(GPRA_wide_data_EduEmp$IncomePubAssist.y))
print(PubAssistIncomeSixSUM<- sum(GPRA_wide_data_EduEmp$IncomePubAssist.y))
```

##Percent change in Mean and Total Pub Assist Income  

```{r}
print(PubAssistIncome_change<- ((PubAssistIncomeSix-PubAssistIncomeBase)/PubAssistIncomeBase))
print(PubAssistIncomeSUM_change<- ((PubAssistIncomeSixSUM-PubAssistIncomeBaseSUM)/PubAssistIncomeBaseSUM)) 
```

There was about a 19.8% increase in Pub assist income. The nean went from $85 to $102 and the sum went from $8,917 to $10,681.

##Mean and Total Disability Income for base and month 6

```{r}
print(DisabilityIncomeBase<- mean(GPRA_wide_data_EduEmp$IncomeDisability.x))
print(DisabilityIncomeBaseSUM<- sum(GPRA_wide_data_EduEmp$IncomeDisability.x))
```

```{r}
print(DisabilityIncomeSix<- mean(GPRA_wide_data_EduEmp$IncomeDisability.y))
print(DisabilityIncomeSixSUM<- sum(GPRA_wide_data_EduEmp$IncomeDisability.y))
```

##Percent change in Mean and Total Disability Income 

```{r}
print(DisabilityIncome_change<- ((DisabilityIncomeSix-DisabilityIncomeBase)/DisabilityIncomeBase)) 
print(DisabilityIncomeSUM_change<- ((DisabilityIncomeSixSUM-DisabilityIncomeBaseSUM)/DisabilityIncomeBaseSUM)) 
```

There was about an 106.2% increase in Disability income. The mean went from $82 to $170 and the sum went from $8,639 to $17,816.

##Total Income for base and month

```{r}
print(TotIncomeBase<- sum(IncomeBase, PubAssistIncomeBase, DisabilityIncomeBase))
print(TotIncomeBaseSUM<- sum(IncomeBaseSUM, PubAssistIncomeBaseSUM, DisabilityIncomeBaseSUM))
```

```{r}
print(TotIncomeSix<- sum(IncomeSix, PubAssistIncomeSix, DisabilityIncomeSix))
print(TotIncomeSixSUM<- sum(IncomeSixSUM, PubAssistIncomeSixSUM, DisabilityIncomeSixSUM))
```

###Part I Income benefits

```{r}
print(Part1_Income<- (TotIncomeSixSUM-TotIncomeBaseSUM))
```

There was a $17,777 increase in income.

###Part 2 Income benefits

```{r}
print(Part2i_Income<-Part1_Income*1.445)
print(Part2ii_Income<-Part1_Income + (Part1_Income*0.5*0.445))
print(Part2iii_Income<-Part1_Income + (Part1_Income*0.25*0.445))
```

Scenario 1 had a $25,688 increase in income, Scenario 2 had a $21,732 increase, and Scenario 3 had a $19,755 increase.

#Program Costs

###Part II Costs per person

```{r}
print(cost_per_person <- 2000000/186)
```

###Part I program costs: Benefits/Costs of only the sample analyzed

```{r}
print(Part1_costs<- cost_per_person*105)
```

#Incarceration
```{r}
GPRA_wide_Crime<- GPRA_wide[c("ArrestedConfineDays.x", "ArrestedConfineDays.y", "DAUseIllegDrugsDays.x", "DAUseIllegDrugsDays.y","NrCrimes.x", "NrCrimes.y", "ArrestedDays.x", "ArrestedDays.y")]
GPRA_wide_data_Crime = na.omit(GPRA_wide_Crime)
```

##Total Days Incarcerated for base and month 6

```{r}
print(IncarceratedBase_tot<- sum(GPRA_wide_data_Crime$ArrestedConfineDays.x))
print(IncarceratedSix_tot<- sum(GPRA_wide_data_Crime$ArrestedConfineDays.y))
```

##Percent change in Days Incarcerated

```{r}
print(Incarcerated_change<- (IncarceratedSix_tot-IncarceratedBase_tot)/IncarceratedBase_tot) 
```

170% increase in days incarcerated. Went from 120 to 324.

##Costs of Incarceration

###Part I

```{r}
print(Part1_Incarceration<- (IncarceratedSix_tot-IncarceratedBase_tot)*101.88)
```

Adds $20,784 to the program costs in the sample.

###Part II
```{r}
print(Part2i_Incarceration<- Part1_Incarceration*1.445)
print(Part2ii_Incarceration<- Part1_Incarceration + (Part1_Incarceration*0.5*0.445))
print(Part2ii_Incarceration<- Part1_Incarceration + (Part1_Incarceration*0.25*0.445))
```

Adds $30,032 to the program costs in Scenario 1, $25,408 in Scenario 2, and $23,096 in Scenario 3.

#Opportunity Cost

The opportunity costs consist of the particpants' time during sessions. Sessions were 1-hr visits 4 times per quarter (3 months). This gives us 8 visits for this study. We used the minimum wage ($7.25/hour) as our measurement. 

###Part I

```{r}
print(OC<-8*7.25)
print(Part1_OC<-OC*105)
```

$58 per person in opportunity cost and would be $6,090 in overall program costs.

###Part II

```{r}
print(Part2i_OC<-OC*186)
print(Part2ii_OC<-Part1_OC + (Part1_OC*0.5))
print(Part2iii_OC<-Part1_OC + (Part1_OC*0.25))
```

The opportunity cost would be $10,788 in Scenario 1, $9,135 in Scenario 2, and $7,613 in Scenario 3.

#CEA - Non-monetary Outputs
```{R}
GPRA_wide_EnuffMoney<- GPRA_wide[c("EnoughMoneyForNeeds.x", "EnoughMoneyForNeeds.y")]
GPRA_wide_data_EnuffMoney = na.omit(GPRA_wide_EnuffMoney)
```

**48 Observations**

##Median Enough Money for Needs for base
```{r}
print(EnuffMoneyBase<- median(GPRA_wide_data_EnuffMoney$EnoughMoneyForNeeds.x))
```

##Median Enough Money for Needs for 6mo

```{r}
print(EnuffMoneySix<- median(GPRA_wide_data_EnuffMoney$EnoughMoneyForNeeds.y))
```

##Percent change in Median Enough Money for Needs

```{r}
print(Education_change<- ((EnuffMoneySix-EnuffMoneyBase)/EnuffMoneyBase)*100) 
```

No change -> "Not at All"

```{r}
GPRA_wide_Depression<- GPRA_wide[c("Depression.x", "Depression.y")]
GPRA_wide_data_Depression = na.omit(GPRA_wide_Depression)
```
**103 Observations**

##Total Days Depressed for base

```{r}
print(DepressedBase_tot<- mean(GPRA_wide_data_Depression$Depression.x))
```

##Total Days Depressed for 6mo

```{r}
print(DepressedSix_tot<- mean(GPRA_wide_data_Depression$Depression.y))
```

##Percent change in Days Depressed

```{r}
print(Depressed_change<- ((DepressedSix_tot-DepressedBase_tot)/DepressedBase_tot)*100)
```

8.9% reduction

```{r}
GPRA_wide_Anxiety<- GPRA_wide[c("Anxiety.x", "Anxiety.y")]
GPRA_wide_data_Anxiey = na.omit(GPRA_wide_Anxiety)
```

**103 Observations**

##Total Days Anxious for base

```{r}
print(AnxietyBase_tot<- mean(GPRA_wide_data_Anxiey$Anxiety.x))
```

##Total Days Anxious for 6mo

```{r}
print(AnxietySix_tot<- mean(GPRA_wide_data_Anxiey$Anxiety.y))
```

##Percent change in Days Anxious

```{r}
print(Anxiety_change<- ((AnxietySix_tot-AnxietyBase_tot)/AnxietyBase_tot)*100)
```

10.3% reduction

```{r}
GPRA_wide_Suicide<- GPRA_wide[c("Suicide.x", "Suicide.y")]
GPRA_wide_data_Suicide = na.omit(GPRA_wide_Suicide)
```

**104 Observations**

##Total Suicide Attempts for base

```{r}
print(SuicideBase_tot<- sum(GPRA_wide_data_Suicide$Suicide.x))
```

##Total Suicide Attempts for 6mo

```{r}
print(SuicideSix_tot<- sum(GPRA_wide_data_Suicide$Suicide.y))
```

##Percent change in Total Suicide Attempts

```{r}
print(Anxiety_change<- ((SuicideSix_tot-SuicideBase_tot)/SuicideBase_tot)*100)
```

63.6% reduction

```{r}
GPRA_wide_PsycEmo<- GPRA_wide[c("PsycholEmotImpact.x", "PsycholEmotImpact.y")]
GPRA_wide_data_PsycEmo = na.omit(GPRA_wide_PsycEmo)
```

**92 Observations**

##Psych/Emo for base

```{r}
print(PsycEmoBase<- median(GPRA_wide_data_PsycEmo$PsycholEmotImpact.x))
```

##Psych/Emo for 6mo

```{r}
print(PsycEmoSix<- median(GPRA_wide_data_PsycEmo$PsycholEmotImpact.y))
```

##Percent change in Psych/Emo 

```{r}
print(PsycEmo_change<- ((PsycEmoSix-PsycEmoBase)/PsycEmoBase)*100) 
```

25% decrease; i.e. change from "considerably" to "moderately"
