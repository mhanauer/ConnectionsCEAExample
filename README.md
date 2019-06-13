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
Want to get only matching, but the data is in long form.  Need to make it wide.  


Conn_Base = ConnPaper[c("LivingWhere_follow", "HealthStatus", "Age", "EducationYears", "Gender", "DAUseIllegDrugsDays", "County", "PHQ9Base", "ER_visit", "Ncrimes", "ParoleProbation", "Hospital")]

```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/ConnectionsPaperData")
GPRAAll = read.csv("ConnGPRA.csv", header = TRUE, na.strings = c(-9, -8, -7, -1, "", " ", "NULL", NULL, NA, "NA")) 
head(GPRAAll)
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

### Select only the variables that you want
head(GPRA_wide)


GPRA_wide_data = GPRA_wide[c("HealthSatisfaction.x", "HealthSatisfaction.y", "ERAlcoholSA.x", "ERAlcoholSA.y", "ERMental.x", "ERMental.y", "ERPhysical.x", "ERPhysical.y")]
### Get rid of missing data
dim(GPRA_wide_data)
GPRA_wide_data_complete = na.omit(GPRA_wide_data)
dim(GPRA_wide_data_complete)
```
Grant was $2,000,000 48 total people we have health status on
Should we only include people we have data on or include the whole sample?
```{r}
## Make sure nothing funky going on 
describe(GPRA_wide_data_complete)

eff_health =  mean((GPRA_wide_data_complete$HealthSatisfaction.y - GPRA_wide_data_complete$HealthSatisfaction.x)/GPRA_wide_data_complete$HealthSatisfaction.x)
eff_health*100

cost_per_person = (2000000*(48/200))/48
cost_per_person

```
Example with ER Visits: https://wisqars.cdc.gov:8443/costT/cost_Part1_Finished.jsp
```{r}
er_visit_base = ifelse(GPRA_wide_data_complete$ERAlcoholSA.x == 1, 1,ifelse(GPRA_wide_data_complete$ERMental.x == 1 ,1, ifelse(GPRA_wide_data_complete$ERPhysical.x == 1,1, 0)))
describe.factor(er_visit_base)

er_visit_six = ifelse(GPRA_wide_data_complete$ERAlcoholSA.y == 1, 1,ifelse(GPRA_wide_data_complete$ERMental.y == 1 ,1, ifelse(GPRA_wide_data_complete$ERPhysical.y == 1,1,0)))

describe.factor(er_visit_six)
eff_er = mean(er_visit_six - er_visit_base)
eff_er

cost_per_person = (2000000/160)

cost_per_person

## Need to adjust for inflation
er_2010 = 6131*abs(eff_er)
er_2010

er_data = round(data.frame(eff_er, cost_per_person, er_2010),3)
er_data
```
Regresino
```{r}
library(tidyr)
er_visit_total = data.frame(er_visit_base, er_visit_six)
head(er_visit_total)


er_total = reshape(er_visit_total, varying = list(c("er_visit_base", "er_visit_six")), times = c(0,1), direction = "long")
er_total

er_reg = glm(er_visit_base ~ time, data = er_total, family = "binomial")
er_reg_sum = summary(er_reg)
er_reg_sum
exp(er_reg_sum$coefficients[,1])
exp(confint(er_reg))
```


