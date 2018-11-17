"
title: w241 Project - Robustness of Null Results
"
  
  # 1. Load libraries, data

# Libraries
library(lmtest)
library(sandwich)
library(ggplot2)
library(data.table)
library(stargazer)
library(ri)
library(multiwayvcov)
library(AER)

rm(list=ls())

#d <- read.csv('~/mids-w241-final/Analysis/Combined Log.csv')
d <- read.csv("/Users/nwchen24/desktop/UC_Berkeley/experiments_and_causality/final_project_github_repo/mids-w241-final/Analysis/Combined Log.csv")
d <- data.table(d)
# d <- d[complete.cases(d),] drops any row that is too incomplete - too stringent since we have some cols which are not essential
d <- d[!is.na(no)] # Just drop row with missing values

# Base data
head(d)

# Convert, transform data for analysis

# Drop some cols
d[,c('title','full_URL', 'reply_email_TO_BE_FILLED_IN_standard','posting_ID','notes') :=NULL]

# Set gender = 1 for Jane
d[treatment_assignment=='Jane_Control' | treatment_assignment=='Jane_Treat_High' | treatment_assignment=='Jane_Treat_Low',gender:=1]
d[treatment_assignment=='John_Control' | treatment_assignment=='John_Treat_High' | treatment_assignment=='John_Treat_Low',gender:=0]

# Set treatment variable = 0 for control, 1 for low, 2 for high
d[treatment_assignment=='Jane_Control' | treatment_assignment=='John_Control', treatment:=0]
d[treatment_assignment=='Jane_Treat_Low' | treatment_assignment=='John_Treat_Low', treatment:=1]
d[treatment_assignment=='Jane_Treat_High' | treatment_assignment=='John_Treat_High', treatment:=2]

# Alternatively, treat treatment types as categorical variables instead of continuous
d[treatment_assignment=='Jane_Treat_Low' | treatment_assignment=='John_Treat_Low', low_treatment:=1]
d[treatment_assignment=='Jane_Treat_High' | treatment_assignment=='John_Treat_High', high_treatment:=1]
d$low_treatment[is.na(d$low_treatment)] <- 0
d$high_treatment[is.na(d$high_treatment)] <- 0

d[low_treatment==1 | high_treatment==0, assigned:=1]
d$assigned[is.na(d$assigned)] <- 0

# Capture compliers
d[sent!='', compliers:=1]
d$compliers[is.na(d$compliers)] <- 0

# 2. Check data, do simple tables to check for balance

cat('\nTable of Outcomes (By Treatment):')
table(d$outcome, d$treatment)


## Try with RI
di <- d
di[treatment==2,treatment:=1]
di[,city:=as.numeric(factor(d$city))]

hist(di$treatment)
hist(di$city)

y <- di$outcome
Z <- di$treatment
cls <- di$gender
blk <- di$city

perms <- genperms(Z, clustvar = NULL, blockvar = blk)
probs <- genprobexact(Z, clustvar = NULL, blockvar = blk) # probability of treatment
ate <- estate(y,Z,prob=probs) # estimate the ATE


Ys <- genouts(y,Z,ate=0) # generate potential outcomes under sharp null of no effect
distout <- gendist(Ys,perms, prob=probs) # generate sampling dist. under sharp null
dispdist(distout, ate, quantiles = c(0.025, 0.975), display.plot = TRUE) # display characteristics of sampling dist. for inference

#P-value for actual data
p.val.actual = sum(abs(distout) > ate) / length(distout)
p.val.actual

#get respnse rate by treatment or control
actual.response.rate.by.treatment <- di[, mean(outcome), by = c("treatment")]
actual.response.rate.by.treatment

di[, sum(outcome > -100), by = c("treatment")]


#**********************************
#NC Work on null finding checks
#**********************************

#create a new temp column of outcomes where the share of responses is n%

#To see what treatment response rate is required for significant result, adjust this variable.
#Found that a treatment response rate of about 0.6 would be required to observe significant result
treatment.response.rate <- 0.51
di$hypothetical.outcomes.temp <- sample(c(0,1), size = nrow(d), replace = TRUE, prob = c(1-treatment.response.rate, treatment.response.rate))

#create new outcome column that takes original outcomes for control group, but new hypothetical outcomes with adjusted response rate for treatment rows
di$hypothetical.outcomes = d$outcome
di[treatment==1, hypothetical.outcomes:=hypothetical.outcomes.temp]

fake.response.rate.by.treatment <- di[, mean(hypothetical.outcomes), by = c("treatment")]
fake.response.rate.by.treatment

#run RI using the fake data
y.fake <- di$hypothetical.outcomes
Z.fake <- di$treatment
cls.fake <- di$gender
blk.fake <- di$city

perms.fake <- genperms(Z.fake, clustvar = NULL, blockvar = blk.fake)
probs.fake <- genprobexact(Z.fake, clustvar = NULL, blockvar = blk.fake) # probability of treatment
ate.fake <- estate(y.fake,Z.fake,prob=probs.fake) # estimate the ATE


Ys.fake <- genouts(y.fake,Z.fake,ate=0) # generate potential outcomes under sharp null of no effect
distout.fake <- gendist(Ys.fake,perms.fake, prob=probs.fake) # generate sampling dist. under sharp null
dispdist(distout.fake, ate.fake, quantiles = c(0.025, 0.975), display.plot = TRUE,) # display characteristics of sampling dist. for inference

#P-value for actual data
p.val.fake = sum(abs(distout.fake) > ate.fake) / length(distout.fake)
p.val.fake




#***********************
#How many non-CL mail relay emails are there

#flag rows where email host is @craigslist.org
d$flag_cl_mail_relay <- as.numeric(grepl('craigslist.org', d$reply_email_TO_BE_FILLED_IN_standard))

sum(d$flag_cl_mail_relay)


sum(d$professional) / 483


