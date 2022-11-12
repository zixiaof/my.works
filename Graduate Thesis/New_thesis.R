rm(list=ls()) 
cat("\014")   
dev.off()   

library(urca)
library(forecast)
library(tidyverse)
library(readxl)
library(quantmod)
library(vars)
library(dynlm)
library(tseries)
library(dynamac)
library(car)

setwd("~/Desktop/Gthesis")

# load data
HKdata <- read_xlsx("data.xlsx", sheet = 2)
HKdata$year <- as.yearqtr(HKdata$year) # TFR as xts series

# define xts objects
TFR <- xts(HKdata$TFR, HKdata$year)#["1989::2021"]
HP <- xts(HKdata$HP, HKdata$year)#["1989::2021"]
R <- xts(HKdata$FLFPR, HKdata$year)#["1989::2021"]
FE <- xts(HKdata$FME, HKdata$year)#["1989::2021"]
ME <- xts(HKdata$MME, HKdata$year)#["1989::2021"]
HI <- xts(HKdata$HI, HKdata$year)#["1989::2021"]
rGDPpc <- xts(HKdata$rGDPpc, HKdata$year)#["1989::2021"]
FE.adj <- xts(HKdata$FEadj, HKdata$year)#["1989::2021"]
ME.adj <- xts(HKdata$MEadj, HKdata$year)#["1989::2021"]
MAW <- xts(HKdata$MAW, HKdata$year)#["1989::2021"]
MAW.adj <- xts(HKdata$MAWadj, HKdata$year)#["1989::2021"]
TAW <- xts(HKdata$TAW, HKdata$year)#["1989::2021"]
TAW.adj <- xts(HKdata$TAW.adj, HKdata$year)#["1989::2021"]
benefit <- xts(HKdata$benefit, HKdata$year)#["1989::2021"]
benefit.adj <- xts(HKdata$benefit.adj, HKdata$year)#["1989::2021"]
CPI <- xts(HKdata$CPI, HKdata$year)#["1989::2021"]



# log transform
lTFR <- log(TFR)
lHP <- log(HP)
lR <- log(R)
lFE <- log(FE)
lME <- log(ME)
lHI <- log(HI)
lrGDPpc <- log(rGDPpc)
lFE.adj <- log(FE.adj)
lME.adj <- log(ME.adj)
lMAW <- log(MAW)
lMAW.adj <- log(MAW.adj)
lTAW <- log(TAW)
lTAW.adj <- log(TAW.adj)
lbenefit <- log(benefit)
lbenefit.adj <- log(benefit.adj)
lCPI <- log(CPI)

# plot.1
par(mfrow = c(2, 1))

# plot the series
plot(as.zoo(TFR),
     col = "steelblue",
     lwd = 2,
     ylab = "fertility rate",
     xlab = "year",
     main = "Total Fertility Rate",
     cex.main = 1)

plot(as.zoo(benefit),
     col = "steelblue",
     lwd = 2,
     ylab = "HK Dollar",
     xlab = "year",
     main = "Estimated Benefit",
     cex.main = 1)

# plot.2
par(mfrow = c(3, 2))
plot(as.zoo(benefit.adj),
     col = "steelblue",
     lwd = 2,
     ylab = "benefit",
     xlab = "year",
     main = "Estimated Benefit",
     cex.main = 1)

plot(as.zoo(HP),
     col = "steelblue",
     lwd = 2,
     ylab = "Housing Price Index",
     xlab = "year",
     main = "Housing Price Index",
     cex.main = 1)

plot(as.zoo(R),
     col = "steelblue",
     lwd = 2,
     ylab = "Female Labor Force Participation rate",
     xlab = "year",
     main = "Female Labor Force Participation rate",
     cex.main = 1)

plot(as.zoo(FE.adj),
     col = "steelblue",
     lwd = 2,
     ylab = "CPI adjusted amount",
     xlab = "year",
     main = "Female Median Monthly Earning",
     cex.main = 1)

plot(as.zoo(ME.adj),
     col = "steelblue",
     lwd = 2,
     ylab = "CPI adjusted amount",
     xlab = "year",
     main = "Male Median Monthly Earning",
     cex.main = 1)

plot(as.zoo(MAW.adj),
     col = "steelblue",
     lwd = 2,
     ylab = "minimum wage",
     xlab = "year",
     main = "Minimum Wage for FDHs",
     cex.main = 1)


# summary
library(psych)
vars <- c('TFR', 'HP', 'FLFPR', "FME", "MME", "FEadj", "MEadj", 
          "MAWadj", "MAW", "TAW", "TAW.adj", "benefit", "benefit.adj")
describe(HKdata[vars], fast = T)


## Step 1: unit root tests
# H0 = nonstationary; H1 = stationary

# TFR
summary(ur.df(lTFR, type = "trend", selectlags = "BIC")) # non
summary(ur.df(lTFR, type = "drift", selectlags = "BIC")) # non
summary(ur.df(lTFR, type = "none", selectlags = "BIC")) # non
summary(ur.df(diff(lTFR)[-1], selectlags = "BIC")) # non
summary(ur.df(diff(lTFR, differences = 2)[-c(1,2)], selectlags = "BIC")) # stat
# HP
summary(ur.df(lHP, type = "trend", selectlags = "BIC")) # non
summary(ur.df(lHP, type = "drift", selectlags = "BIC")) # non
summary(ur.df(lHP, type = "none", selectlags = "BIC")) # non
summary(ur.df(diff(lHP)[-1], selectlags = "BIC"))# non
summary(ur.df(diff(lHP, differences = 2)[-c(1,2)], selectlags = "BIC")) # stat
# FLFPR
summary(ur.df(lR, type = "trend", selectlags = "BIC")) # non
summary(ur.df(lR, type = "drift", selectlags = "BIC")) # non
summary(ur.df(lR, type = "none", selectlags = "BIC")) # stat
summary(ur.df(diff(lR)[-1], selectlags = "BIC")) # stat
# FE.adj
summary(ur.df(lFE.adj, type = "trend", selectlags = "BIC")) # non
summary(ur.df(lFE.adj, type = "drift", selectlags = "BIC")) # non
summary(ur.df(lFE.adj, type = "none", selectlags = "BIC")) # stat
summary(ur.df(diff(lFE.adj)[-1], selectlags = "BIC")) # stat
# ME.adj
summary(ur.df(lME.adj, type = "trend", selectlags = "BIC")) # non
summary(ur.df(lME.adj, type = "drift", selectlags = "BIC")) # non
summary(ur.df(lME.adj, type = "none", selectlags = "BIC")) # non
summary(ur.df(diff(lME.adj)[-1], selectlags = "BIC")) # stat
# MAW.adj
summary(ur.df(lMAW.adj, type = "trend", selectlags = "BIC")) # non
summary(ur.df(lMAW.adj, type = "drift", selectlags = "BIC")) # stat
summary(ur.df(lMAW.adj, type = "none", selectlags = "BIC")) # non
summary(ur.df(diff(lMAW.adj)[-1], selectlags = "BIC")) # stat
# benefit.adj
summary(ur.df(lbenefit.adj, type = "trend", selectlags = "BIC")) # stat
summary(ur.df(lbenefit.adj, type = "drift", selectlags = "BIC")) # non
summary(ur.df(lbenefit.adj, type = "none", selectlags = "BIC")) # stat
summary(ur.df(diff(lbenefit.adj)[-1], selectlags = "BIC")) # stat


# KPSS
# H0: The time series is trend stationary.
# HA: The time series is not trend stationary.

kpss.test(lTFR, null = "L") # stationary! 
kpss.test(lHP, null = "L") # non
kpss.test(lR, null = "L") # non
kpss.test(lFE.adj, null = "L") # non
kpss.test(lME.adj, null = "L") # non
kpss.test(lMAW.adj, null = "L") # non
kpss.test(lbenefit.adj, null = "L") # non


kpss.test(diff(lTFR), null = "L") # stationary! 
kpss.test(diff(lHP), null = "L") # stat
kpss.test(diff(lR), null = "L") # stat
kpss.test(diff(lFE), null = "L") # stat
kpss.test(diff(lME), null = "L") # stat
kpss.test(diff(lMAW), null = "L") # stat
kpss.test(diff(lbenefit), null = "L") # stat


### Johansen (1991) procedure
# co-integration analysis is a system of many time series,
# will it cause problem if we include too many series in one system?

# lag selection
d.adj <- cbind(lTFR, lHP, lR, lME.adj, lFE.adj, lMAW.adj, lbenefit.adj)
lagselect1 <- VARselect(d.adj, lag.max = 15, type = "const") # lag.max = ?
lagselect1$selection # 4


d.nom <- cbind(lTFR, lHP, lR, lME, lFE, lMAW, lbenefit, CPI)
lagselect1 <- VARselect(d.nom, lag.max = 15, type = "const") # lag.max = ?
lagselect1$selection # 3


# optimal p = 2, no drift and time trend component, from ADF 

# trace statistics
trace.2 <- ca.jo(d.adj, type = "trace", ecdet = "const", K = 2)
summary(trace.2)

trace.3 <- ca.jo(d.adj, type = "trace", ecdet = "const", K = 3)
summary(trace.3)

# Max Eigen
eigen.2 <- ca.jo(d.adj, type = "eigen", ecdet = "const", K = 2)
summary(eigen.2)

eigen.3 <- ca.jo(d.adj, type = "eigen", ecdet = "const", K = 3)
summary(eigen.3)
# there exist at least two coint vectors



### ADL model 

# define ts objects
fert <- ts(lTFR, start = c(1989, 1), end = c(2021, 1), frequency = 1)
bene <- ts(lbenefit.adj, start = c(1989, 1), end = c(2021, 1), frequency = 1)
r <- ts(lR, start = c(1989, 1), end = c(2021, 1), frequency = 1)
hp <- ts(lHP, start = c(1989, 1), end = c(2021, 1), frequency = 1)
mearning <- ts(lME.adj, start = c(1989, 1), end = c(2021, 1), frequency = 1)
fearning <- ts(lFE.adj, start = c(1989, 1), end = c(2021, 1), frequency = 1)
mwage <- ts(lMAW.adj, start = c(1989, 1), end = c(2021, 1), frequency = 1)
df <- as.data.frame(cbind(fert, bene, r, hp, mearning, fearning, mwage))
series <- c("fert", "bene", "r", "hp", "mearning", "fearning", "mwage")


# F -test function

F <- function(R2.u, R2.r, df.u, df.r, n, k){
  f = ((R2.u-R2.r)/(df.r-df.u))/((1-R2.u)/(n-k-1))
  f
}

# by hand
l = c(1,2)
eq1 <- dynardl(fert ~ bene + r + hp + mearning + fearning + mwage, data = df,
                   lags = list("fert" = l, "bene" = l, "r" = l, "hp" = l, "mearning" = l,
                               "fearning" = l, "mwage" = l),
                   diffs = c(series[-1]),
                   lagdiffs = list("fert" = 1),
                   ec = T, simulate = F)
summary(unres)


# restrict on bene
rest1 <- dynardl(fert ~ r + hp + mearning + fearning + mwage, data = df,
                 lags = list("fert" = l, "r" = l, "hp" = l, "mearning" = l,
                             "fearning" = l, "mwage" = l),
                 diffs = c(series[-c(1,2)]),
                 lagdiffs = list("fert" = 1),
                 ec = T, simulate = F)
summary(rest1)
(F(R2.u = 0.7746, R2.r = 0.7291, df.u = 10, df.r = 13, n = 31, k = 21)) # 0.6055901
length(res.d2)
qf(p = 0.05, df1 = 10, df2 = 13, lower.tail = T) # 0.3463594

# restrict on hp
rest2 <- dynardl(fert ~ bene + r + mearning + fearning + mwage, data = df,
                 lags = list("fert" = l, "bene" = l, "r" = l, "mearning" = l,
                             "fearning" = l, "mwage" = l),
                 diffs = c(series[-c(1,4)]),
                 lagdiffs = list("fert" = 1),
                 ec = T, simulate = F)
summary(rest2)
(F(R2.u = 0.7746, R2.r = 0.6415, df.u = 10, df.r = 13, n = 31, k = 21)) # 1.771517
length(res.d2)
qf(p = 0.05, df1 = 10, df2 = 13, lower.tail = T) # 0.3463594


# restrict on R
l = c(1:2)
rest3 <- dynardl(fert ~ bene + hp + mearning + fearning + mwage, data = df,
                 lags = list("fert" = l, "bene" = l, "hp" = l, "mearning" = l,
                             "fearning" = l, "mwage" = l),
                 diffs = c(series[-c(1,3)]),
                 lagdiffs = list("fert" = 1),
                 ec = T, simulate = F)
summary(rest3)
(F(R2.u = 0.7746, R2.r = 0.7518, df.u = 10, df.r = 13, n = 31, k = 21)) # 0.3034605





