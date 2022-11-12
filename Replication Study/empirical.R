# Empirical

setwd("/Users/fengzixiao/Desktop/HKBU/Second Semester/Advanced Economitrics/lab/emp")
getwd()

# # data_prep
library(readr)
library(tidyr)
library(dplyr)

PUMS <- read_csv('usa_00001.csv')
summary(PUMS$YEAR)

#Drop households with more than one couple or one family
PUMS <- subset(PUMS, NFAMS == 1 &  NCOUPLES == 1)
ACS <- PUMS
#Define the dummy to indicate whether the person worked in the previous year.
ACS$worklastyr <- as.integer(with(ACS, ifelse(WORKEDYR == 3,1,0)))

#Define the father dummy (with at least two children, household head or spouse)
ACS$isFather <- as.integer(with(ACS, ifelse((RELATE == 2 | RELATE == 1) & SEX == 1 & NCHILD >= 2 & MARST == 1,1,0)))

#Define the mother dummy (sample mother, age 21-35, at least two children, married)
ACS$isMother <- as.integer(with(ACS, ifelse((RELATE == 2 | RELATE == 1) & SEX == 2 & AGE >= 21
                                            & AGE <= 35 & NCHILD >= 2 & MARST == 1,1,0)))
# ismother = househead or spouse, age 21-35, at least 2 children, married

# Check if both parents have the same reported number of child.
ACS <- ACS %>% mutate(nchild_mum = NCHILD * isMother,
                      nchild_dad = NCHILD * isFather)

ACS <- left_join(ACS,summarise(group_by(ACS, SERIAL), nchild_mum1 = sum(nchild_mum)),by="SERIAL")
ACS <- left_join(ACS,summarise(group_by(ACS, SERIAL), nchild_dad1 = sum(nchild_dad)),by="SERIAL")

# Define the child dummy
ACS$isChild <- as.integer(with(ACS, ifelse(RELATE == 3,1,0)))

# Compute the number of child in a household
ACS <- left_join(ACS,summarise(group_by(ACS, SERIAL), num_child = sum(isChild)),by="SERIAL")

# Number of child reported by both parents are consistent with the actual number of child
ACS <- subset(ACS, nchild_dad1 == nchild_mum1)
ACS <- subset(ACS, num_child == nchild_mum1)

ACS <- subset(ACS, nchild_dad1 == nchild_mum1 & num_child == nchild_mum1)

# Define age of mother when she had her first-born
ACS$age1st <- ACS$AGE - ACS$ELDCH

# Find 1st sex
ACS$childAge <- ACS$AGE * ACS$isChild
ACS <- left_join(ACS,summarise(group_by(ACS, SERIAL), oldest = max(childAge)),by="SERIAL")
ACS$isoldest <- as.integer(with(ACS, ifelse(oldest == childAge,1,0))) 
ACS <- left_join(ACS,summarise(group_by(ACS, SERIAL), numoldest = sum(isoldest)),by="SERIAL")
ACS <- subset(ACS, numoldest == 1)
ACS$firstsex <- ACS$isoldest * ACS$SEX
ACS <- left_join(ACS,summarise(group_by(ACS, SERIAL), firstsex1 = sum(firstsex)),by="SERIAL")
ACS <- subset(ACS, isoldest == 0)

# Find 2nd sex
ACS <- left_join(ACS,summarise(group_by(ACS, SERIAL), oldest2 = max(childAge)),by="SERIAL")
ACS$isoldest <- as.integer(with(ACS, ifelse(oldest2 == childAge,1,0))) 
ACS <- left_join(ACS,summarise(group_by(ACS, SERIAL), numoldest2 = sum(isoldest)),by="SERIAL")
ACS <- subset(ACS, numoldest2 == 1)
ACS$secondsex <- ACS$isoldest * ACS$SEX
ACS <- left_join(ACS,summarise(group_by(ACS, SERIAL), secondsex1 = sum(secondsex)),by="SERIAL")

# Define a dummy that indicate whether a mother has more than two kids.
# the endogenous var I want to use
ACS$morethan2kids <- as.integer(with(ACS, ifelse(NCHILD >= 3,1,0))) 

ACS <- subset(ACS, isMother == 1)
ACS_combined <- ACS


### lab 3

library(AER)
library(psych)

#Generate  dummy variables for samesex,twogirls, twoboys, boy1st, boy2nd
ACS_combined$samesex <- as.integer(with(ACS_combined, ifelse(firstsex1 == secondsex1,1,0)))
ACS_combined$twogirls <- as.integer(with(ACS_combined, ifelse(firstsex1 == 2 & secondsex1 == 2,1,0)))
ACS_combined$twoboys <- as.integer(with(ACS_combined, ifelse(firstsex1 == 1 & secondsex1 == 1,1,0)))
ACS_combined$boy1st <- as.integer(with(ACS_combined, ifelse(firstsex1 == 1,1,0)))
ACS_combined$boy2nd <- as.integer(with(ACS_combined, ifelse(secondsex1 == 1,1,0)))

# other dependents, than worklasryr
ACS_combined$college <- as.integer(with(ACS_combined, ifelse(EDUC >= 7,1,0)))
ACS_combined$ln_familyinc <- log(ACS_combined$FTOTINC)


## Table 2
vars <- c('morethan2kids', 'boy1st', 'boy2nd', 'twoboys', 'twogirls', 'firstsex1',  'secondsex1',  'samesex', 
          'AGE',  'age1st',  'worklastyr',  'INCWAGE', 'WKSWORK2', 'UHRSWORK', 'FTOTINC')
describe(ACS_combined[vars], fast = T)

sink("Table 2.txt")
describe(ACS_combined[vars], fast = T)
sink()

## Table 5
# Equa 1
equa1 <- lm(NCHILD ~ samesex, data = ACS_combined)
summary(equa1)

# Equa 2
equa2 <- lm(morethan2kids ~ samesex, data = ACS_combined)
summary(equa2)

sink("table5-3.txt")
summary(equa4)
summary(equa5)
sink()

# Equa 3
equa3 <- lm(worklastyr ~ samesex, data = ACS_combined) # reduced form
summary(equa3)

# Equa 4: TSLS
equa4 <- ivreg(worklastyr ~ NCHILD | samesex, data = ACS_combined)
summary(equa4)

# Equa 5: TSLS
equa5 <- ivreg(worklastyr ~ morethan2kids | samesex, data = ACS_combined)
summary(equa5)

# # table 6
# Equa 6
equa6 <- lm(morethan2kids ~ AGE + age1st + samesex + factor(RACE), data = ACS_combined)
summary(equa6) # first stage with additional controls 

# Equa 7
equa7 <- lm(morethan2kids ~ AGE + age1st + samesex + boy2nd + boy1st + factor(RACE), data = ACS_combined)
summary(equa7) # [samesex], even with more control the 1st stage remains the same.

# Equa 8
equa8 <- lm(morethan2kids ~ AGE + age1st + twoboys + twogirls + boy1st + factor(RACE), data = ACS_combined)
summary(equa8) # if the 1st stage differ when you have 2boys versus 2girls, more likely to have 3rd child with 2girls

sink("table 6.txt")
summary(equa6)
summary(equa7)
summary(equa8)
sink()


# # table 7
# Equa 9
equa9 <- lm(INCWAGE ~ morethan2kids + AGE + age1st +  factor(RACE), data = ACS_combined)
summary(equa9) 

# Equa 10
equa10 <- ivreg(INCWAGE ~ morethan2kids + AGE + age1st +  factor(RACE) 
                | samesex + AGE + age1st +  factor(RACE), data = ACS_combined) 
summary(equa10) 

# Equa 11
equa11 <- ivreg(INCWAGE ~ morethan2kids + AGE + age1st +  factor(RACE) 
                | twoboys + twogirls + AGE + age1st + factor(RACE), data = ACS_combined)
summary(equa11)

sink("table 7.txt")
summary(equa9)
summary(equa10)
summary(equa11)
sink()
