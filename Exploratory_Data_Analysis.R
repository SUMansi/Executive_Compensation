#' ---
#' title: "What are the factors that impact the Compensation of executive"
#' author: 'From: Mansi Agarwal, 2nd TeamMember'
#' date: '`r format(Sys.time(), "%B %d, %Y")`'
#' output:
#'   html_document: 
#'     toc: true
#'     toc_depth: '3'
#' ---
#' *** 
#' ### Code Header
## ------------------------------------------------------------------------
# Course: ECON 5100
# Title: Final Project 

#' 
#' ### Clear Environment and Load packages
#' 
## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = TRUE)

# Clear working directory (remove all objects)
rm(list=ls(all=TRUE)) 

# Load packages
library(tidyverse)
library(gridExtra)
library(GGally)
library(knitr)
library(grid)
library(lmtest)
library(stargazer)

#' 
#' ### Load Data and perform data cleaning steps
#' 
## ------------------------------------------------------------------------

# Import data
renumeration_data <- read.csv("ECON5100_project_data.csv", header = TRUE)

# Variable Selection
renum_data <- renumeration_data %>% 
                select(CO_PER_ROL, GENDER, CEOANN, CFOANN, EXECDIR, TITLE, AGE,
                       BONUS, NONEQ_INCENT, OPTION_AWARDS_FV, OTHCOMP, SALARY,
                       SHRS_VEST_VAL, STOCK_AWARDS_FV,AT,DLTT, NI, TDC1)

#CO_PER_ROL : Unique ID number for each company/executive combination.
#TITLE : The title of the named executive officer for the most recent year on file.
#CEOANN : CEO indicates that this executive served as CEO for all or most of the indicated fiscal year.
#CFOANN : CFO indicates that this executive served as CFO for all or most of the indicated fiscal year.
#BONUS : The dollar value of a bonus earned by the named executive officer during the fiscal year.
#NONEQ_INCENT : Value of amounts earned during the year pursuant to non-equity incentive plans. The amount is disclosed in the year that the performance criteria was satisfied and the compensation was earned.
#OPTION_AWARDS_FV : Fair value of all options awarded during the year as detailed in the Plan Based Awards table. Valuation is based upon the grant-date fair value as detailed in FAS 123R
#OTHCOMP : Other compensation received by the director including perquisites and other personal benefits, contributions to defined  contribution plans (e.g. 401K plans), life insurance premiums, gross-ups and other tax reimbursements, discounted share purchases, consulting fees, awards under charitable award programs etc.
#SALARY : The dollar value of the base salary earned by the named executive officer during the fiscal year.
#SHRS_VEST_VAL : Value of restricted shares that vested during the year.
#STOCK_AWARDS_FV : Fair value of all stock awards during the year as detailed in the Plan Based Awards table. Valuation is based upon the grant-date fair value as detailed in FAS 123R
#AT : Total Assets of Company
#DLTT : Long-Term Debt - Total
#NI : Net Income (Loss)
#TDC1 : Total compensation comprised of the following: Salary, Bonus, Other Annual, Total Value of Restricted Stock Granted, Total Value of Stock Options Granted (using Black-Scholes), Long-Term Incentive Payouts, and All Other Total. 

# Create one column for the designation (1- CEO, 2-CFO, 3-Director, 4-Others)
renum_data <-
renum_data %>% mutate(Designation = ifelse((CEOANN == "CEO"), "CEO",
                                           ifelse((CEOANN != "CEO") & (CFOANN == "CFO") , "CFO",
                                                  ifelse((CEOANN != "CEO") & (CFOANN != "CFO") & (EXECDIR == 1), "Director", "Others"))))

# For Designation == Others, look at the Title Colum see if there are any CEO
renum_data <- renum_data %>% 
                mutate(Designation = ifelse( (Designation == "Others" & grepl("Chief Executive Officer", TITLE)), "CEO", Designation))

# For Designation == Others, look at the Title Colum see if there are any CFO
renum_data <- renum_data %>% 
                mutate(Designation = ifelse( (Designation == "Others" & grepl("Chief Financial Officer", TITLE)), "CFO", Designation))

# For Designation == Others, look at the Title Colum see if there are any Director                                 
renum_data <- renum_data %>% 
                mutate(Designation = ifelse( (Designation == "Others" & grepl("Director", TITLE)), "Director", Designation))

# Convert Designation into Factor                                
renum_data$Designation <- as.factor(renum_data$Designation)
# Convert Designation into Factor                                
renum_data$EXECDIR <- as.factor(renum_data$EXECDIR)

#renum_data 8300
# We have 41 executies who have worked in 2 companies. No case for more than two companies
(dup <- renum_data[duplicated(renum_data$EXECID),] %>% 
         arrange(EXECID) )

# LTIP is blank for all the entries
# BECOMECEO is not available for other Designations like CFO and is company specific ie. date at which the person became CEO in company X
# EXECID is unique across companies
# RSTKGRNT (Restricted Stock Grant) doesn't have any values, so take this coulmn out
# PENSION_PYMTS_TOT is other than 0 only for 112 entries, so doesn't make sense to have that column in our model
# OTHANN	(i.e. Other Annual) is blank for all the entries

#Remove the records with No Age
renum_data <- renum_data %>% 
               filter(!is.na(AGE)) %>% 
               filter(SALARY > 10) %>% 
               filter(TDC1 > 0) %>%
               filter(BONUS >= 0) %>%
               filter(OTHCOMP >= 0)

renum_data2 <- renum_data %>% 
               head(6000)


#' 
#' ### Logarithm of Total Compensation
#' 
## ----fig.width=10, fig.height=4------------------------------------------
par(mfrow = c(1,2))
hist(renum_data$TDC1, main="Histogram of Total Compensation", xlab="Total Compensation")
hist(log(renum_data$TDC1), main="Histogram of Log of Total Compensation", xlab="Log of Total Compensation")

#' 
#' ### Logarithm of Total Salary
#' 
## ----fig.width=10, fig.height=4------------------------------------------
par(mfrow = c(1,2))
hist(renum_data$SALARY, main="Histogram of Salary", xlab="Salary")
hist(log(renum_data$SALARY), main="Histogram of Log of Salary", xlab="Salary")

#' 
#' ### Correlation between Independent Variables
#' 
## ------------------------------------------------------------------------
#Correlation among Independent variables
cor_data <- renum_data %>% select(-c(CO_PER_ROL, CFOANN, CEOANN, TITLE, EXECDIR, Designation, GENDER, TDC1))
cor(cor_data)

#' 
#' ### Collinearity Test
#' 
## ------------------------------------------------------------------------
# Detecting collinearity on Base Model: Collinearity exists
# log(TDC1) ~ Age + Bonus + Noneq_Incent + Option_Awards_FV + Othcomp + log(Salary)+ Shrs_Vest_Val + Stock_Awards_FV + AT + DLTT + NI + Gender + Designation 

# Complete Data Set 
mod_21 <- lm(log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + log(SALARY)+ SHRS_VEST_VAL + STOCK_AWARDS_FV + AT + DLTT + NI + GENDER + Designation , data = renum_data )
summary(mod_21)

# Reduced Data Set
mod_22_data <- renum_data2 %>% select(-c(CO_PER_ROL, CFOANN, CEOANN, TITLE, EXECDIR))
mod_22 <- lm(log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + log(SALARY)+ SHRS_VEST_VAL + STOCK_AWARDS_FV + AT + DLTT + NI + GENDER + Designation , data = renum_data2 )
summary(mod_22)

# Summary of the final model
final_model <- renum_data %>% select(-c(CO_PER_ROL, TITLE, AT, DLTT, SHRS_VEST_VAL))
summary(final_model)

#' 
#' ### Hetroskedascity Test
## ----fig.width=12, fig.height=5------------------------------------------
# Estimate Final Model
final_mod <- lm(log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + log(SALARY) + STOCK_AWARDS_FV + NI + GENDER + Designation , data = final_model)
summary(final_mod)

# Hetroskedascity Test
par(mfrow = c(2,2))
plot(final_mod, cex.lab=1.5)

# Plot Actual vs. Fitted
par(mfrow = c(1,2))
hist(resid(final_mod), main="Histogram of Residual", xlim = c(-4,4), xlab = "Fitted Residual", cex.lab=1.5)
plot(predict(final_mod), log(final_model$TDC1), main = "Actual vs. Predicted", xlab = "Predicted Compensation", ylab = "Actual Compensation", cex.lab=1.5) 
abline(a=0,b=1)

#' 
#' ### Reset Test
#' 
## ------------------------------------------------------------------------
# Perform RESET Test on the final model
F.critical <- qf(1-0.05,2,8155) #2.996833

lmtest::resettest(final_mod, power = 2:3) #2669
F.stat <- 2669

if(F.stat > F.critical){
  paste("Reject Null Hypothesis. Model is potentially mis-specified. It is missing some regressors or inadequate.")
}else{
  paste("Do Not Reject Null Hypothesis. Model is adequate.")
}


#' 
#' ### Impact of Gender on CEOs Total Compensation
#' 
## ------------------------------------------------------------------------
New<-
renum_data %>% 
  mutate(Old = ifelse(AGE > 45, 1,
                      ifelse(AGE <= 45, 0, 99))) %>%
  mutate(CEO_Flag = ifelse((Designation == "CEO"), 1,0),
         CFO_Flag = ifelse((Designation == "CFO"), 1,0),
         Director_Flag = ifelse((Designation == "Director"), 1,0))

#Male/Female dummy
New$Female <- ifelse(New$GENDER == "MALE", 0, 1)
 
  
#A. What is the approximate difference in CEO Total compensation between Male and Female? Is this difference statistically significant?

A <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV + NI +AGE+ CEO_Flag * Female , data =New )

summary(A)

#' 
#' ### Impact of Age on Executive's Total Compensation (Chow test)
#' 
## ------------------------------------------------------------------------


B <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV + NI +GENDER + Designation + Old ,
        data = New )

summary(B)

#Chow Test
m <- 10    # number of restrictions 
  n <- dim(New)[1]
  
  
  full.1 <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV +
                 NI +GENDER + Designation, data=subset(New, Old == 1))
  
  full.2 <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV +
                 NI +GENDER + Designation, data=subset(New, Old == 0))
  
  full <-   lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV +
                 NI +GENDER + Designation + Old, data=New)
  
  rest <-   lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV +
                 NI +GENDER + Designation , data=New)
  
  SSE.1 <- sum( resid(full.1)^2 ) 
  SSE.2 <- sum( resid(full.2)^2 ) 
  SSE.full <- sum( resid(full)^2 ) 
  SSE.rm <- sum( resid(rest)^2 ) 
  
  chow <- (( SSE.rm - (SSE.1 + SSE.2)) / m ) / ( (SSE.1 + SSE.2) / (n - 2*m) )  #24.97031
  
  pval.chow <- 1 - pf(chow, m, n-2*m) 
  
  out <- list(SSE.1=SSE.1, SSE.2=SSE.2, SSE.rm=SSE.rm, chow=chow, pval.chow=pval.chow)
  
  sapply(out, cbind)

# F.critical Value
F.critical <- qf(1-0.05,m,n-2*m) 

if (F.critical > chow){
  print("Do not Reject Null hypothesis")
} else {
  print("Reject Null hypothesis")
}


#' 
#' ### Impact of Designation on Executive's Total Compensation  
#' 
## ------------------------------------------------------------------------

C <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV + NI + AGE + GENDER + CEO_Flag , data = New )
CC <- summary(C)


# Perform F Test to say that designation doesn't have any impact on the Compensation (H0: Coefficient of CEO_Flag  = 0  ,H1: Coefficient of CEO_Flag NotEqual 0)

m <- 1                  # number of restrictions
n <- dim(New)[1]        # sample size
p <- 10                  # number of regressors in full model

rest <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV + NI + AGE + GENDER , data = New )

SSE.fm <- sum( resid(C)^2 )
SSE.rm <- sum( resid(rest)^2 )
SSR.diff  <- SSE.rm - SSE.fm

F.rm <- (SSR.diff/m) / (SSE.fm / (n-p-1))  #89.208  
F.critical <- qf(1-0.05,m,n-p-1) #3.842599

if(F.rm > F.critical){
  paste("Reject Null Hypothesis. Adding designation to the model adds explanatory power and the difference in compensation for CEO's and Non-CEO's is statistically significant.")
}else{
  paste("Do Not Reject Null Hypothesis")
}


#' 
#' 
#' ### Impact of Company's performance on CEOs Total Compensation
#' 
## ------------------------------------------------------------------------

New<- New %>% 
        mutate(HighNI = ifelse(NI > 0, 1,
                               ifelse(NI <= 0, 0, 1))) 

D <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP +
          STOCK_AWARDS_FV + AGE + GENDER + CEO_Flag*HighNI ,data = New )
summary(D)

(exp(-0.0045595 + 0.0241013) - 1) * 100


#' 
#' ### To Verify the joint significance of Bonus & Other Compensation(i.e. OTHCOMP)
## ------------------------------------------------------------------------

New$BonusSq <-  New$BONUS^2
New$OTHCOMPSq <-  New$OTHCOMP^2
New$SALARYSq <-  New$SALARY^2

E <- lm(log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV +  OTHCOMP + log(SALARY) +
          STOCK_AWARDS_FV + GENDER + Designation + NI + BonusSq + OTHCOMPSq   ,data = New )
summary(E)


F.criticalE <- qf(1-0.05,2,8154) #2.996833

lmtest::resettest(E, power = 2:3)


