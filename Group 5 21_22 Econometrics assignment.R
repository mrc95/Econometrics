################################################
##    Group 5 - Econometrics Assignment       ##
##             A.Y. 2021-2022                 ##
## Enrico Bacchetti, Junling Fu, Marco Lingua ##
##    Amedeo Micheletti, Walter Venanzetti    ##
################################################

library(stats)
library(readxl)
library(jtools)
library(robustbase)
library(restriktor)
library(lmtest)
library(sandwich)
library(ggplot2)
library(dplyr)
library(car)

cat("\014")
rm(list=ls())

hr <- c(3,6,9,12,24,36) # Vector of time horizons > 1

# You can download the excel datasheet here: https://tinyurl.com/rrkkvfmr
# Read data 

path = "/Users/Marco/Desktop/Mafinrisk/I Term/Econometrics/R"
path2data = file.path(path, "Group Assignment data")
data = read_xlsx('/Users/Marco/Desktop/Mafinrisk/I Term/Econometrics/R/Data/Group 5 Assignment data final.xlsx')

data[,8:22] <- scale(data[,8:22]) # Standardizes economic variables

# Plot time series sentiment index

my.ts <- ts(select(data, MSI, BW, HJTZ, CBC), start=c(2003,12), end=c(2014, 12), frequency=12)    
ts.plot(my.ts, ylab="Sentiment index", col=c('black','cyan','red','blue'))
legend(x="topright", inset=0.02, c("MS", "BW", "HJTZ", "CBC"), col = c('black','cyan','red','blue'), lty = c(1,1,1),cex=0.5)

# Creates void vectors of cumulative excess returns 

cum_ret <- matrix(NA,nrow = nrow(data), ncol = length(hr)+1)
names <- c("t1","t3","t6","t9","t12","t24","t36")
colnames(cum_ret) = names
data <- cbind(data, cum_ret)

# Computes cumulative excess returns for t+1 horizon

for (i in 1:nrow(data)){
  data[i,24] <- data[i+1,4]
}

# Computes cumulative excess returns for t+h horizons

i <- 1 
j <- 25
h <- 1
while(h <= 6 && j <= 30){
  for (i in 1:nrow(data)){
    data[i,j] <- prod(1+data[(i+1):(i+hr[h]),4])-1
  }
  i=i+1
  if(i <- nrow(data)){
    h = h+1
    j = j+1
}
}

# Task 2 ------------------------------------------------------------------

# Managers' sentiment index predictive regressions

reg1 <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)])
summary(reg1)$r.squared
coeftest(reg1, vcov=NeweyWest(reg1, lag=3, prewhite=FALSE))

reg3 <- lm(data$t3[1:(nrow(data)-3)] ~ data$MSI[1:(nrow(data)-3)])
summary(reg3)$r.squared
coeftest(reg3, vcov=NeweyWest(reg3, lag=3, prewhite=FALSE))

reg6 <- lm(data$t6[1:(nrow(data)-6)] ~ data$MSI[1:(nrow(data)-6)])
summary(reg6)$r.squared
coeftest(reg6, vcov=NeweyWest(reg6, lag=3, prewhite=FALSE))

reg9 <- lm(data$t9[1:(nrow(data)-9)] ~ data$MSI[1:(nrow(data)-9)])
summary(reg9)$r.squared
coeftest(reg9, vcov=NeweyWest(reg9, lag=3, prewhite=FALSE))

reg12 <- lm(data$t12[1:(nrow(data)-12)] ~ data$MSI[1:(nrow(data)-12)])
summary(reg12)$r.squared
coeftest(reg12, vcov=NeweyWest(reg12, lag=3, prewhite=FALSE))

reg24 <- lm(data$t24[1:(nrow(data)-24)] ~ data$MSI[1:(nrow(data)-24)])
summary(reg24)$r.squared
coeftest(reg24, vcov=NeweyWest(reg24, lag=3, prewhite=FALSE))

reg36 <- lm(data$t36[1:(nrow(data)-36)] ~ data$MSI[1:(nrow(data)-36)])
summary(reg36)$r.squared
coeftest(reg36, vcov=NeweyWest(reg36, lag=3, prewhite=FALSE))

# Baker-Wurgler Investor Sentiment Index predictive regressions

reg1a <- lm(data$t1[1:(nrow(data)-1)] ~ data$BW[1:(nrow(data)-1)])
summary(reg1a)$r.squared
coeftest(reg1a, vcov=NeweyWest(reg1a, lag=3, prewhite=FALSE))

reg3a <- lm(data$t3[1:(nrow(data)-3)] ~ data$BW[1:(nrow(data)-3)])
summary(reg3a)$r.squared
coeftest(reg3a, vcov=NeweyWest(reg3a, lag=3, prewhite=FALSE))

reg6a <-lm(data$t6[1:(nrow(data)-6)] ~ data$BW[1:(nrow(data)-6)])
summary(reg6a)$r.squared
coeftest(reg6a, vcov=NeweyWest(reg6a, lag=3, prewhite=FALSE))

reg9a <- lm(data$t9[1:(nrow(data)-9)] ~ data$BW[1:(nrow(data)-9)])
summary(reg9a)$r.squared
coeftest(reg9a, vcov=NeweyWest(reg9a, lag=3, prewhite=FALSE))

reg12a <- lm(data$t12[1:(nrow(data)-12)] ~ data$BW[1:(nrow(data)-12)])
summary(reg12a)$r.squared
coeftest(reg12a, vcov=NeweyWest(reg12a, lag=3, prewhite=FALSE))

reg24a <- lm(data$t24[1:(nrow(data)-24)] ~ data$BW[1:(nrow(data)-24)])
summary(reg24a)$r.squared
coeftest(reg24a, vcov=NeweyWest(reg24a, lag=3, prewhite=FALSE))

reg36a <- lm(data$t36[1:(nrow(data)-36)] ~ data$BW[1:(nrow(data)-36)])
summary(reg36a)$r.squared
coeftest(reg36a, vcov=NeweyWest(reg36a, lag=3, prewhite=FALSE))

# Huang, Jiang, Tu, and Zhou (2015) aligned investor sentiment index predictive regressions

reg1b <- lm(data$t1[1:(nrow(data)-1)] ~ data$HJTZ[1:(nrow(data)-1)])
summary(reg1b)$r.squared
coeftest(reg1b, vcov=NeweyWest(reg1b, lag=3, prewhite=FALSE))

reg3b <- lm(data$t3[1:(nrow(data)-3)] ~ data$HJTZ[1:(nrow(data)-3)])
summary(reg3b)$r.squared
coeftest(reg3b, vcov=NeweyWest(reg3b, lag=3, prewhite=FALSE))

reg6b <- lm(data$t6[1:(nrow(data)-6)] ~ data$HJTZ[1:(nrow(data)-6)])
summary(reg6b)$r.squared
coeftest(reg6b, vcov=NeweyWest(reg6b, lag=3, prewhite=FALSE))

reg9b <- lm(data$t9[1:(nrow(data)-9)] ~ data$HJTZ[1:(nrow(data)-9)])
summary(reg9b)$r.squared
coeftest(reg9b, vcov=NeweyWest(reg9b, lag=3, prewhite=FALSE))

reg12b <- lm(data$t12[1:(nrow(data)-12)] ~ data$HJTZ[1:(nrow(data)-12)])
summary(reg12b)$r.squared
coeftest(reg12b, vcov=NeweyWest(reg12b, lag=3, prewhite=FALSE))

reg24b <- lm(data$t24[1:(nrow(data)-24)] ~ data$HJTZ[1:(nrow(data)-24)])
summary(reg24b)$r.squared
coeftest(reg24b, vcov=NeweyWest(reg24b, lag=3, prewhite=FALSE))

reg36b <- lm(data$t36[1:(nrow(data)-36)] ~ data$HJTZ[1:(nrow(data)-36)])
summary(reg36b)$r.squared
coeftest(reg36b, vcov=NeweyWest(reg36b, lag=3, prewhite=FALSE))

# Conference Board consumer confidence index predictive regressions

reg1c <- lm(data$t1[1:(nrow(data)-1)] ~ data$CBC[1:(nrow(data)-1)])
summary(reg1c)$r.squared
coeftest(reg1c, vcov=NeweyWest(reg1c, lag=3, prewhite=FALSE))

reg3c <- lm(data$t3[1:(nrow(data)-3)] ~ data$CBC[1:(nrow(data)-3)])
summary(reg3c)$r.squared
coeftest(reg3c, vcov=NeweyWest(reg3c, lag=3, prewhite=FALSE))

reg6c <- lm(data$t6[1:(nrow(data)-6)] ~ data$CBC[1:(nrow(data)-6)])
summary(reg6c)$r.squared
coeftest(reg6c, vcov=NeweyWest(reg6c, lag=3, prewhite=FALSE))

reg9c<-lm(data$t9[1:(nrow(data)-9)] ~ data$CBC[1:(nrow(data)-9)])
summary(reg9c)$r.squared
coeftest(reg9c, vcov=NeweyWest(reg9c, lag=3, prewhite=FALSE))

reg12c <- lm(data$t12[1:(nrow(data)-12)] ~ data$CBC[1:(nrow(data)-12)])
summary(reg12c)$r.squared
coeftest(reg12c, vcov=NeweyWest(reg12c, lag=3, prewhite=FALSE))

reg24c <- lm(data$t24[1:(nrow(data)-24)] ~ data$CBC[1:(nrow(data)-24)])
summary(reg24c)$r.squared
coeftest(reg24c, vcov=NeweyWest(reg24c, lag=3, prewhite=FALSE))

reg36c <- lm(data$t36[1:(nrow(data)-36)] ~ data$CBC[1:(nrow(data)-36)])
summary(reg36c)$r.squared
coeftest(reg36c, vcov=NeweyWest(reg36c, lag=3, prewhite=FALSE))

# Task 3 ------------------------------------------------------------------

# Panel A: univariate regressions

regDPa <- lm(data$t1[1:(nrow(data)-1)] ~ data$DP[1:(nrow(data)-1)])
summary(regDPa)$r.squared
coeftest(regDPa, vcov=NeweyWest(regDPa, lag=3, prewhite=FALSE))

regDYa <- lm(data$t1[1:(nrow(data)-1)] ~ data$DY[1:(nrow(data)-1)])
summary(regDYa)$r.squared
coeftest(regDYa, vcov=NeweyWest(regDYa, lag=3, prewhite=FALSE))

regEPa <- lm(data$t1[1:(nrow(data)-1)] ~ data$EP[1:(nrow(data)-1)])
summary(regEPa)$r.squared
coeftest(regEPa, vcov=NeweyWest(regEPa, lag=3, prewhite=FALSE))

regDEa <- lm(data$t1[1:(nrow(data)-1)] ~ data$DE[1:(nrow(data)-1)])
summary(regDEa)$r.squared
coeftest(regDEa, vcov=NeweyWest(regDEa, lag=3, prewhite=FALSE))

regSVARa <- lm(data$t1[1:(nrow(data)-1)] ~ data$SVAR[1:(nrow(data)-1)])
summary(regSVARa)$r.squared
coeftest(regSVARa, vcov=NeweyWest(regSVARa, lag=3, prewhite=FALSE))

regBMa <- lm(data$t1[1:(nrow(data)-1)] ~ data$BM[1:(nrow(data)-1)])
summary(regBMa)$r.squared
coeftest(regBMa, vcov=NeweyWest(regBMa, lag=3, prewhite=FALSE))

regNTISa <- lm(data$t1[1:(nrow(data)-1)] ~ data$NTIS[1:(nrow(data)-1)])
summary(regNTISa)$r.squared
coeftest(regNTISa, vcov=NeweyWest(regNTISa, lag=3, prewhite=FALSE))

regTBLa <- lm(data$t1[1:(nrow(data)-1)] ~ data$TBL[1:(nrow(data)-1)])
summary(regTBLa)$r.squared
coeftest(regTBLa, vcov=NeweyWest(regTBLa, lag=3, prewhite=FALSE))

regLTYa <- lm(data$t1[1:(nrow(data)-1)] ~ data$LTY[1:(nrow(data)-1)])
summary(regLTYa)$r.squared
coeftest(regLTYa, vcov=NeweyWest(regLTYa, lag=3, prewhite=FALSE))

regLTRa <- lm(data$t1[1:(nrow(data)-1)] ~ data$LTR[1:(nrow(data)-1)])
summary(regLTRa)$r.squared
coeftest(regLTRa, vcov=NeweyWest(regLTRa, lag=3, prewhite=FALSE))

regTMSa <- lm(data$t1[1:(nrow(data)-1)] ~ data$TMS[1:(nrow(data)-1)])
summary(regTMSa)$r.squared
coeftest(regTMSa, vcov=NeweyWest(regTMSa, lag=3, prewhite=FALSE))

regDFYa <- lm(data$t1[1:(nrow(data)-1)] ~ data$DFY[1:(nrow(data)-1)])
summary(regDFYa)$r.squared
coeftest(regDFYa, vcov=NeweyWest(regDFYa, lag=3, prewhite=FALSE))

regDFRa <- lm(data$t1[1:(nrow(data)-1)] ~ data$DFR[1:(nrow(data)-1)])
summary(regDFRa)$r.squared
coeftest(regDFRa, vcov=NeweyWest(regDFRa, lag=3, prewhite=FALSE))

regINFLa <- lm(data$t1[1:(nrow(data)-1)] ~ data$INFL[1:(nrow(data)-1)])
summary(regINFLa)$r.squared
coeftest(regINFLa, vcov=NeweyWest(regINFLa, lag=3, prewhite=FALSE))

regECONa <- lm(data$t1[1:(nrow(data)-1)] ~ data$ECON[1:(nrow(data)-1)])
summary(regECONa)$r.squared
coeftest(regECONa, vcov=NeweyWest(regECONa, lag=3, prewhite=FALSE))

# Panel B: bivariate regressions, MS and each single economic variable

regDPm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$DP[1:(nrow(data)-1)])
summary(regDPm)$r.squared
coeftest(regDPm, vcov=NeweyWest(regDPm, lag=3, prewhite=FALSE))

regDYm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$DY[1:(nrow(data)-1)])
summary(regDYm)$r.squared
coeftest(regDYm, vcov=NeweyWest(regDYm, lag=3, prewhite=FALSE))

regEPm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$EP[1:(nrow(data)-1)])
summary(regEPm)$r.squared
coeftest(regEPm, vcov=NeweyWest(regEPm, lag=3, prewhite=FALSE))

regDEm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$DE[1:(nrow(data)-1)])
summary(regDEm)$r.squared
coeftest(regDEm, vcov=NeweyWest(regDEm, lag=3, prewhite=FALSE))

regSVARm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$SVAR[1:(nrow(data)-1)])
summary(regSVARm)$r.squared
coeftest(regSVARm, vcov=NeweyWest(regSVARm, lag=3, prewhite=FALSE))

regBMm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$BM[1:(nrow(data)-1)])
summary(regBMm)$r.squared
coeftest(regBMm, vcov=NeweyWest(regBMm, lag=3, prewhite=FALSE))

regNTISm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$NTIS[1:(nrow(data)-1)])
summary(regNTISm)$r.squared
coeftest(regNTISm, vcov=NeweyWest(regNTISm, lag=3, prewhite=FALSE))

regTBLm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$TBL[1:(nrow(data)-1)])
summary(regTBLm)$r.squared
coeftest(regTBLm, vcov=NeweyWest(regTBLm, lag=3, prewhite=FALSE))

regLTYm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$LTY[1:(nrow(data)-1)])
summary(regLTYm)$r.squared
coeftest(regLTYm, vcov=NeweyWest(regLTYm, lag=3, prewhite=FALSE))

regLTRm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$LTR[1:(nrow(data)-1)])
summary(regLTRm)$r.squared
coeftest(regLTRm, vcov=NeweyWest(regLTRm, lag=3, prewhite=FALSE))

regTMSm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$TMS[1:(nrow(data)-1)])
summary(regTMSm)$r.squared
coeftest(regTMSm, vcov=NeweyWest(regTMSm, lag=3, prewhite=FALSE))

regDFYm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$DFY[1:(nrow(data)-1)])
summary(regDFYm)$r.squared
coeftest(regDFYm, vcov=NeweyWest(regDFYm, lag=3, prewhite=FALSE))

regDFRm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$DFR[1:(nrow(data)-1)])
summary(regDFRm)$r.squared
coeftest(regDFRm, vcov=NeweyWest(regDFRm, lag=3, prewhite=FALSE))

regINFLm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$INFL[1:(nrow(data)-1)])
summary(regINFLm)$r.squared
coeftest(regINFLm, vcov=NeweyWest(regINFLm, lag=3, prewhite=FALSE))

regECONm <- lm(data$t1[1:(nrow(data)-1)] ~ data$MSI[1:(nrow(data)-1)] + data$ECON[1:(nrow(data)-1)])
summary(regECONm)$r.squared
coeftest(regECONm, vcov=NeweyWest(regECONm, lag=3, prewhite=FALSE))


# Multiple regression  with all the explanatory variables

Z = as.data.frame(cbind(data[-144,8:22])) # creates dataframe for a compact lm function
Z = cbind(data$MSI[1:(nrow(data)-1)],Z)

all_var_reg_1 <- lm(data$t1[1:(nrow(data)-1)] ~ . -DE-ECON-TMS,data=Z)
summary(all_var_reg_1)$adj.r.squared
coeftest(all_var_reg_1, vcov=NeweyWest(all_var_reg_1, lag=3, prewhite=FALSE))
alias(all_var_reg_1) # Finds linear dependencies
vif(all_var_reg_1) # Computes Variance Inflation Factor 
rankMM(cor(Z)) # Calculates rank of the correlation matrix

# Multiple regression after checked for multicollinearity 

all_var_reg_2 <- lm(data$t1[1:(nrow(data)-1)] ~ .-DE -TMS -ECON -DY -DP -DFY,data=Z)
summary(all_var_reg_2)$adj.r.squared
coeftest(all_var_reg_2, vcov=NeweyWest(all_var_reg_2, lag=3, prewhite=FALSE))
alias(all_var_reg_2)
vif(all_var_reg_2)

# generates squares of each variable in the "corrected" model

MSI2 <- data$MSI[1:(nrow(data)-1)]^2
EP2 <- data$EP[1:(nrow(data)-1)]^2
SVAR2 <- data$SVAR[1:(nrow(data)-1)]^2
BM2 <- data$BM[1:(nrow(data)-1)]^2
NTIS2 <- data$NTIS[1:(nrow(data)-1)]^2
TBL2 <- data$TBL[1:(nrow(data)-1)]^2
LTY2 <- data$LTY[1:(nrow(data)-1)]^2
LTR2 <- data$LTR[1:(nrow(data)-1)]^2
DFR2 <- data$DFR[1:(nrow(data)-1)]^2
INFL2 <- data$INFL[1:(nrow(data)-1)]^2

Z = cbind(Z,MSI2,EP2,SVAR2,BM2,NTIS2,TBL2,LTY2,LTR2,DFR2,INFL2)

# F-test: checking if there is some form of misspecification in the latter model

all_var_reg_test <- lm(data$t1[1:(nrow(data)-1)] ~ .-DE -TMS -ECON -DY -DP -DFY,data=Z) 
myH0 <- c("MSI2","EP2","SVAR2","BM2","NTIS2","TBL2","LTY2","LTR2","DFR2","INFL2")
linearHypothesis(all_var_reg_test,myH0)

# Task 4 ------------------------------------------------------------------

# Panel A: IS ----> MS

reg_BW_MS <- lm(data$MSI[6:(nrow(data)-1)] ~ 
                  data$MSI[5:(nrow(data)-2)] + data$MSI[4:(nrow(data)-3)] + 
                  data$MSI[3:(nrow(data)-4)] + data$MSI[2:(nrow(data)-5)] + 
                  data$MSI[1:(nrow(data)-6)] + data$BW[5:(nrow(data)-2)] + 
                  data$BW[4:(nrow(data)-3)] + data$BW[3:(nrow(data)-4)] + 
                  data$BW[2:(nrow(data)-5)] + data$BW[1:(nrow(data)-6)])
summary(reg_BW_MS)$adj.r.squared
coeftest(reg_BW_MS, vcov=NeweyWest(reg_BW_MS, lag=3, prewhite=FALSE))

reg_HJTZ_MS <- lm(data$MSI[6:(nrow(data)-1)] ~ 
                    data$MSI[5:(nrow(data)-2)] + data$MSI[4:(nrow(data)-3)] + 
                    data$MSI[3:(nrow(data)-4)] + data$MSI[2:(nrow(data)-5)] + 
                    data$MSI[1:(nrow(data)-6)] + data$HJTZ[5:(nrow(data)-2)] + 
                    data$HJTZ[4:(nrow(data)-3)] + data$HJTZ[3:(nrow(data)-4)] + 
                    data$HJTZ[2:(nrow(data)-5)] + data$HJTZ[1:(nrow(data)-6)])
summary(reg_HJTZ_MS)$adj.r.squared
coeftest(reg_HJTZ_MS, vcov=NeweyWest(reg_HJTZ_MS, lag=3, prewhite=FALSE))

# Panel B MS ----> IS

reg_MS_BW <- lm(data$BW[6:(nrow(data)-1)] ~ 
                  data$BW[5:(nrow(data)-2)] + data$BW[4:(nrow(data)-3)] + 
                  data$BW[3:(nrow(data)-4)] + data$BW[2:(nrow(data)-5)] + 
                  data$BW[1:(nrow(data)-6)] + data$MSI[5:(nrow(data)-2)] + 
                  data$MSI[4:(nrow(data)-3)] + data$MSI[3:(nrow(data)-4)] +
                  data$MSI[2:(nrow(data)-5)] + data$MSI[1:(nrow(data)-6)])
summary(reg_MS_BW)$adj.r.squared
coeftest(reg_MS_BW, vcov=NeweyWest(reg_MS_BW, lag=3, prewhite=FALSE))

reg_MS_HJTZ <- lm(data$HJTZ[6:(nrow(data)-1)] ~ 
                    data$HJTZ[5:(nrow(data)-2)] + data$HJTZ[4:(nrow(data)-3)] + 
                    data$HJTZ[3:(nrow(data)-4)] + data$HJTZ[2:(nrow(data)-5)] + 
                    data$HJTZ[1:(nrow(data)-6)] + data$MSI[5:(nrow(data)-2)] + 
                    data$MSI[4:(nrow(data)-3)] + data$MSI[3:(nrow(data)-4)] +
                    data$MSI[2:(nrow(data)-5)] + data$MSI[1:(nrow(data)-6)])
summary(reg_MS_HJTZ)$adj.r.squared
coeftest(reg_MS_HJTZ, vcov=NeweyWest(reg_MS_HJTZ, lag=3, prewhite=FALSE))

# Panel C: MCS <----> MS

reg_UMS_MSI <- lm(data$MSI[6:(nrow(data)-1)] ~ 
                    data$MSI[5:(nrow(data)-2)] + data$MSI[4:(nrow(data)-3)] + 
                    data$MSI[3:(nrow(data)-4)] + data$MSI[2:(nrow(data)-5)] + 
                    data$MSI[1:(nrow(data)-6)] + data$UMS[5:(nrow(data)-2)] + 
                    data$UMS[4:(nrow(data)-3)] + data$UMS[3:(nrow(data)-4)] + 
                    data$UMS[2:(nrow(data)-5)] + data$UMS[1:(nrow(data)-6)])
summary(reg_UMS_MSI)$adj.r.squared
coeftest(reg_UMS_MSI, vcov=NeweyWest(reg_UMS_MSI, lag=3, prewhite=FALSE))

reg_MSI_UMS <- lm(data$UMS[6:(nrow(data)-1)] ~ 
                    data$UMS[5:(nrow(data)-2)] + data$UMS[4:(nrow(data)-3)] + 
                    data$UMS[3:(nrow(data)-4)] + data$UMS[2:(nrow(data)-5)] + 
                    data$UMS[1:(nrow(data)-6)] + data$MSI[5:(nrow(data)-2)] + 
                    data$MSI[4:(nrow(data)-3)] + data$MSI[3:(nrow(data)-4)] +
                    data$MSI[2:(nrow(data)-5)] + data$MSI[1:(nrow(data)-6)])
summary(reg_MSI_UMS)$adj.r.squared
coeftest(reg_MSI_UMS, vcov=NeweyWest(reg_MSI_UMS, lag=3, prewhite=FALSE))
