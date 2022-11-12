# thesis

setwd("/Users/fengzixiao/Desktop/RESEARCH/undergraduate thesis") 
library(haven)
dat <- read_dta("databench.dta")
summary(dat)
library(stargazer)
stargazer(as.data.frame(dat), type="text",
          out="summary_statistics.txt",
          summary.stat=c("n","mean","sd","min","max"), digits=3)
install.packages("plm")
library(plm)
library(car)
ols1 <- lm( ln_pds ~ ln_provin_tax, data = dat ) # OLS regression 
fe1 <- plm(ln_pds ~ ln_provin_tax, data = dat, index=c("year"), model="within")
fe2 <- plm(ln_pds ~ ln_provin_tax, data = dat, index=c("id","year"), model="within", effect = "twoways")

ols2 <- lm( ln_pcs ~ ln_provin_tax, data = dat ) # OLS regression 
fe3 <- plm(ln_pcs ~ ln_provin_tax, data = dat, index=c("year"), model="within")
fe4 <- plm(ln_pcs ~ ln_provin_tax, data = dat, index=c("id","year"), model="within", effect = "twoways")

#

summary(fe1)
summary(fe2)
summary(ols1)
summary(ols2)
summary(fe3)
summary(fe4)
plot(ols1) 
plot(ols2) 
phtest(ln_pds ~ ln_provin_tax, data = dat, method = "aux", vcov = vcovHC) 

#table 2-6
age <- read_dta("age.dta")

#2
age1 <- age[which(age$AgeGroup == "15+"), ]
fe2 <- plm(ln_pds ~ ln_provin_tax, data = age1, index=c("id","year"), model="within", effect = "twoways")
fe4 <- plm(ln_pcs ~ ln_provin_tax, data = age1, index=c("id","year"), model="within", effect = "twoways")
#3
age2 <- age[which(age$AgeGroup == "15-19"), ]
fe2 <- plm(ln_pds ~ ln_provin_tax, data = age2, index=c("id","year"), model="within", effect = "twoways")
fe4 <- plm(ln_pcs ~ ln_provin_tax, data = age2, index=c("id","year"), model="within", effect = "twoways")
#4
age3 <- age[which(age$AgeGroup == "20-24"), ]
fe2 <- plm(ln_pds ~ ln_provin_tax, data = age3, index=c("id","year"), model="within", effect = "twoways")
fe4 <- plm(ln_pcs ~ ln_provin_tax, data = age3, index=c("id","year"), model="within", effect = "twoways")
#5
age4 <- age[which(age$AgeGroup == "25-44"), ]
fe2 <- plm(ln_pds ~ ln_provin_tax, data = age4, index=c("id","year"), model="within", effect = "twoways")
fe4 <- plm(ln_pcs ~ ln_provin_tax, data = age4, index=c("id","year"), model="within", effect = "twoways")
#6
age5 <- age[which(age$AgeGroup == "45+"), ]
fe2 <- plm(ln_pds ~ ln_provin_tax, data = age5, index=c("id","year"), model="within", effect = "twoways")
fe4 <- plm(ln_pcs ~ ln_provin_tax, data = age5, index=c("id","year"), model="within", effect = "twoways")


#table 7
ols1 <- lm( ln_ets ~ ln_provin_tax, data = dat ) # OLS regression 
fe1 <- plm(ln_ets ~ ln_provin_tax, data = dat, index=c("year"), model="within")
fe2 <- plm(ln_ets ~ ln_provin_tax, data = dat, index=c("id","year"), model="within", effect = "twoways")

### CPI

# process CPI file
cpi <- read.csv('CPI_MONTHLY.csv')
cpi$year <- apply(cpi['date'], 1, function(x) strsplit(x, '-')[[1]][1]) # get year
cpi_year <- aggregate(V41690973  ~ year, cpi, mean) # group by year and get mean
colnames(cpi_year) <- c('year', 'cpi')
write.csv(cpi_year, 'cpi_year_mean.csv',row.names=FALSE)

# process databench.dta
cpi_year <- read.csv('cpi_year_mean.csv')
dat$index <- 1:nrow(dat) # add a new column for sort
dat <- merge(x = dat, y = cpi_year, by = "year", all.x = TRUE, sort = FALSE) # add cpi column
dat <- dat[order(dat$index), ] # reset order
dat <- dat[ , !(names(dat) %in% c('index'))] # delete the index column
dat$federal_cpi <- dat$federal / dat$cpi * 100 # cpi adjust federal
dat$provincial_cpi <- dat$provincial / dat$cpi *100 # cpi adjust provincial
dat$total_cpi <- dat$federal_cpi + dat$provincial_cpi # cpi adjust total
dat$ln_federal_cpi <- log(dat$federal_cpi) # take ln
dat$ln_provin_tax_cpi <- log(dat$provincial_cpi) # take ln
dat$ln_total_cpi <- log(dat$total_cpi) # take ln
dat$federal_cpi <- round(dat$federal_cpi, 2) # round to 2 decimal
dat$provincial_cpi <- round(dat$provincial_cpi, 2) # round to 2 decimal
dat$total_cpi <- round(dat$total_cpi, 2) # round to 2 decimal
