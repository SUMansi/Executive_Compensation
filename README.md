-   [Code Header](#code-header)
-   [Clear Environment and Load packages](#clear-environment-and-load-packages)
-   [Load Data and perform data cleaning steps](#load-data-and-perform-data-cleaning-steps)
-   [Logarithm of Total Compensation](#logarithm-of-total-compensation)
-   [Logarithm of Total Salary](#logarithm-of-total-salary)
-   [Correlation between Independent Variables](#correlation-between-independent-variables)
-   [Collinearity Test](#collinearity-test)
-   [Hetroskedascity Test](#hetroskedascity-test)
-   [Reset Test](#reset-test)
-   [Impact of Gender on CEOs Total Compensation](#impact-of-gender-on-ceos-total-compensation)
-   [Impact of Age on Executive's Total Compensation (Chow test)](#impact-of-age-on-executives-total-compensation-chow-test)
-   [Impact of Designation on Executive's Total Compensation](#impact-of-designation-on-executives-total-compensation)
-   [Impact of Company's performance on CEOs Total Compensation](#impact-of-companys-performance-on-ceos-total-compensation)
-   [To Verify the joint significance of Bonus & Other Compensation(i.e. OTHCOMP)](#to-verify-the-joint-significance-of-bonus-other-compensationi.e.-othcomp)

------------------------------------------------------------------------

### Code Header

``` r
# Course: ECON 5100
# Title: Final Project 
```

### Clear Environment and Load packages

``` r
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
```

### Load Data and perform data cleaning steps

``` r
# Import data
renumeration_data <- read.csv("ECON5100_project_data.csv", header = TRUE)

# Variable Selection
renum_data <- renumeration_data %>% select(CO_PER_ROL, GENDER, CEOANN, CFOANN, 
    EXECDIR, TITLE, AGE, BONUS, NONEQ_INCENT, OPTION_AWARDS_FV, OTHCOMP, SALARY, 
    SHRS_VEST_VAL, STOCK_AWARDS_FV, AT, DLTT, NI, TDC1)

# CO_PER_ROL : Unique ID number for each company/executive combination.
# TITLE : The title of the named executive officer for the most recent year
# on file. CEOANN : CEO indicates that this executive served as CEO for all
# or most of the indicated fiscal year. CFOANN : CFO indicates that this
# executive served as CFO for all or most of the indicated fiscal year.
# BONUS : The dollar value of a bonus earned by the named executive officer
# during the fiscal year. NONEQ_INCENT : Value of amounts earned during the
# year pursuant to non-equity incentive plans. The amount is disclosed in
# the year that the performance criteria was satisfied and the compensation
# was earned. OPTION_AWARDS_FV : Fair value of all options awarded during
# the year as detailed in the Plan Based Awards table. Valuation is based
# upon the grant-date fair value as detailed in FAS 123R OTHCOMP : Other
# compensation received by the director including perquisites and other
# personal benefits, contributions to defined contribution plans (e.g. 401K
# plans), life insurance premiums, gross-ups and other tax reimbursements,
# discounted share purchases, consulting fees, awards under charitable award
# programs etc. SALARY : The dollar value of the base salary earned by the
# named executive officer during the fiscal year. SHRS_VEST_VAL : Value of
# restricted shares that vested during the year. STOCK_AWARDS_FV : Fair
# value of all stock awards during the year as detailed in the Plan Based
# Awards table. Valuation is based upon the grant-date fair value as
# detailed in FAS 123R AT : Total Assets of Company DLTT : Long-Term Debt -
# Total NI : Net Income (Loss) TDC1 : Total compensation comprised of the
# following: Salary, Bonus, Other Annual, Total Value of Restricted Stock
# Granted, Total Value of Stock Options Granted (using Black-Scholes),
# Long-Term Incentive Payouts, and All Other Total.

# Create one column for the designation (1- CEO, 2-CFO, 3-Director,
# 4-Others)
renum_data <- renum_data %>% mutate(Designation = ifelse((CEOANN == "CEO"), 
    "CEO", ifelse((CEOANN != "CEO") & (CFOANN == "CFO"), "CFO", ifelse((CEOANN != 
        "CEO") & (CFOANN != "CFO") & (EXECDIR == 1), "Director", "Others"))))

# For Designation == Others, look at the Title Colum see if there are any
# CEO
renum_data <- renum_data %>% mutate(Designation = ifelse((Designation == "Others" & 
    grepl("Chief Executive Officer", TITLE)), "CEO", Designation))

# For Designation == Others, look at the Title Colum see if there are any
# CFO
renum_data <- renum_data %>% mutate(Designation = ifelse((Designation == "Others" & 
    grepl("Chief Financial Officer", TITLE)), "CFO", Designation))

# For Designation == Others, look at the Title Colum see if there are any
# Director
renum_data <- renum_data %>% mutate(Designation = ifelse((Designation == "Others" & 
    grepl("Director", TITLE)), "Director", Designation))

# Convert Designation into Factor
renum_data$Designation <- as.factor(renum_data$Designation)
# Convert Designation into Factor
renum_data$EXECDIR <- as.factor(renum_data$EXECDIR)

# renum_data 8300 We have 41 executies who have worked in 2 companies. No
# case for more than two companies
(dup <- renum_data[duplicated(renum_data$EXECID), ] %>% arrange(EXECID))
```

    ##  [1] CO_PER_ROL       GENDER           CEOANN           CFOANN          
    ##  [5] EXECDIR          TITLE            AGE              BONUS           
    ##  [9] NONEQ_INCENT     OPTION_AWARDS_FV OTHCOMP          SALARY          
    ## [13] SHRS_VEST_VAL    STOCK_AWARDS_FV  AT               DLTT            
    ## [17] NI               TDC1             Designation     
    ## <0 rows> (or 0-length row.names)

``` r
# LTIP is blank for all the entries BECOMECEO is not available for other
# Designations like CFO and is company specific ie. date at which the person
# became CEO in company X EXECID is unique across companies RSTKGRNT
# (Restricted Stock Grant) doesn't have any values, so take this coulmn out
# PENSION_PYMTS_TOT is other than 0 only for 112 entries, so doesn't make
# sense to have that column in our model OTHANN (i.e. Other Annual) is blank
# for all the entries

# Remove the records with No Age
renum_data <- renum_data %>% filter(!is.na(AGE)) %>% filter(SALARY > 10) %>% 
    filter(TDC1 > 0) %>% filter(BONUS >= 0) %>% filter(OTHCOMP >= 0)

renum_data2 <- renum_data %>% head(6000)
```

### Logarithm of Total Compensation

``` r
par(mfrow = c(1, 2))
hist(renum_data$TDC1, main = "Histogram of Total Compensation", xlab = "Total Compensation")
hist(log(renum_data$TDC1), main = "Histogram of Log of Total Compensation", 
    xlab = "Log of Total Compensation")
```

![alt text]( https://github.com/SUMansi/Executive_Compensation/blob/master/EDA_Figure/unnamed-chunk-4-1.png)

### Logarithm of Total Salary

``` r
par(mfrow = c(1, 2))
hist(renum_data$SALARY, main = "Histogram of Salary", xlab = "Salary")
hist(log(renum_data$SALARY), main = "Histogram of Log of Salary", xlab = "Salary")
```

![alt text]( https://github.com/SUMansi/Executive_Compensation/blob/master/EDA_Figure/unnamed-chunk-5-1.png)

### Correlation between Independent Variables

``` r
# Correlation among Independent variables
cor_data <- renum_data %>% select(-c(CO_PER_ROL, CFOANN, CEOANN, TITLE, EXECDIR, 
    Designation, GENDER, TDC1))
cor(cor_data)
```

    ##                          AGE       BONUS NONEQ_INCENT OPTION_AWARDS_FV
    ## AGE              1.000000000  0.02340426   0.11366152      0.045444204
    ## BONUS            0.023404262  1.00000000  -0.01202813      0.012140078
    ## NONEQ_INCENT     0.113661520 -0.01202813   1.00000000      0.216583861
    ## OPTION_AWARDS_FV 0.045444204  0.01214008   0.21658386      1.000000000
    ## OTHCOMP          0.057284603  0.13452838   0.09814509      0.065573227
    ## SALARY           0.204451325  0.15405074   0.48205024      0.260097352
    ## SHRS_VEST_VAL    0.058996362  0.09742428   0.25244376      0.113207365
    ## STOCK_AWARDS_FV  0.024230995  0.16364788   0.18861266      0.069825594
    ## AT               0.024910571  0.22939506   0.07800660      0.014574176
    ## DLTT             0.009865761  0.04646767   0.02608617      0.005236318
    ## NI               0.034248699  0.17068544   0.16660897      0.064438494
    ##                      OTHCOMP    SALARY SHRS_VEST_VAL STOCK_AWARDS_FV
    ## AGE              0.057284603 0.2044513    0.05899636      0.02423100
    ## BONUS            0.134528384 0.1540507    0.09742428      0.16364788
    ## NONEQ_INCENT     0.098145088 0.4820502    0.25244376      0.18861266
    ## OPTION_AWARDS_FV 0.065573227 0.2600974    0.11320737      0.06982559
    ## OTHCOMP          1.000000000 0.1499149    0.10753200      0.11509546
    ## SALARY           0.149914876 1.0000000    0.32964514      0.28017649
    ## SHRS_VEST_VAL    0.107532004 0.3296451    1.00000000      0.50613484
    ## STOCK_AWARDS_FV  0.115095460 0.2801765    0.50613484      1.00000000
    ## AT               0.029748788 0.2702056    0.13519916      0.12063170
    ## DLTT             0.008780281 0.1538285    0.02631269      0.02122466
    ## NI               0.065617494 0.3086809    0.44119763      0.28171375
    ##                          AT        DLTT         NI
    ## AGE              0.02491057 0.009865761 0.03424870
    ## BONUS            0.22939506 0.046467674 0.17068544
    ## NONEQ_INCENT     0.07800660 0.026086171 0.16660897
    ## OPTION_AWARDS_FV 0.01457418 0.005236318 0.06443849
    ## OTHCOMP          0.02974879 0.008780281 0.06561749
    ## SALARY           0.27020556 0.153828489 0.30868091
    ## SHRS_VEST_VAL    0.13519916 0.026312690 0.44119763
    ## STOCK_AWARDS_FV  0.12063170 0.021224657 0.28171375
    ## AT               1.00000000 0.713910148 0.53907052
    ## DLTT             0.71391015 1.000000000 0.24411022
    ## NI               0.53907052 0.244110219 1.00000000

### Collinearity Test

``` r
# Detecting collinearity on Base Model: Collinearity exists log(TDC1) ~ Age
# + Bonus + Noneq_Incent + Option_Awards_FV + Othcomp + log(Salary)+
# Shrs_Vest_Val + Stock_Awards_FV + AT + DLTT + NI + Gender + Designation

# Complete Data Set
mod_21 <- lm(log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + 
    log(SALARY) + SHRS_VEST_VAL + STOCK_AWARDS_FV + AT + DLTT + NI + GENDER + 
    Designation, data = renum_data)
summary(mod_21)
```

    ## 
    ## Call:
    ## lm(formula = log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + 
    ##     OTHCOMP + log(SALARY) + SHRS_VEST_VAL + STOCK_AWARDS_FV + 
    ##     AT + DLTT + NI + GENDER + Designation, data = renum_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.8259 -0.2899  0.0331  0.2883  3.7996 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2.706e+00  9.571e-02  28.273  < 2e-16 ***
    ## AGE                 -5.681e-03  8.457e-04  -6.717 1.98e-11 ***
    ## BONUS                1.033e-04  9.797e-06  10.541  < 2e-16 ***
    ## NONEQ_INCENT         1.494e-04  5.661e-06  26.386  < 2e-16 ***
    ## OPTION_AWARDS_FV     8.680e-05  4.170e-06  20.815  < 2e-16 ***
    ## OTHCOMP              9.484e-05  5.992e-06  15.828  < 2e-16 ***
    ## log(SALARY)          8.203e-01  1.286e-02  63.773  < 2e-16 ***
    ## SHRS_VEST_VAL        5.949e-06  1.719e-06   3.460 0.000542 ***
    ## STOCK_AWARDS_FV      5.185e-05  1.593e-06  32.538  < 2e-16 ***
    ## AT                   2.626e-07  6.651e-08   3.948 7.96e-05 ***
    ## DLTT                -5.682e-07  9.457e-08  -6.008 1.96e-09 ***
    ## NI                   1.064e-05  3.326e-06   3.199 0.001387 ** 
    ## GENDERMALE           7.011e-02  2.448e-02   2.864 0.004195 ** 
    ## DesignationCFO      -1.551e-01  2.015e-02  -7.700 1.52e-14 ***
    ## DesignationDirector -4.932e-02  2.656e-02  -1.857 0.063329 .  
    ## DesignationOthers   -1.871e-01  1.751e-02 -10.685  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5323 on 8154 degrees of freedom
    ## Multiple R-squared:  0.6866, Adjusted R-squared:  0.6861 
    ## F-statistic:  1191 on 15 and 8154 DF,  p-value: < 2.2e-16

``` r
# Reduced Data Set
mod_22_data <- renum_data2 %>% select(-c(CO_PER_ROL, CFOANN, CEOANN, TITLE, 
    EXECDIR))
mod_22 <- lm(log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + 
    log(SALARY) + SHRS_VEST_VAL + STOCK_AWARDS_FV + AT + DLTT + NI + GENDER + 
    Designation, data = renum_data2)
summary(mod_22)
```

    ## 
    ## Call:
    ## lm(formula = log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + 
    ##     OTHCOMP + log(SALARY) + SHRS_VEST_VAL + STOCK_AWARDS_FV + 
    ##     AT + DLTT + NI + GENDER + Designation, data = renum_data2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -9.5528 -0.2730  0.0364  0.2776  2.8548 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2.534e+00  1.072e-01  23.646  < 2e-16 ***
    ## AGE                 -5.915e-03  9.424e-04  -6.277 3.70e-10 ***
    ## BONUS                9.798e-05  1.081e-05   9.062  < 2e-16 ***
    ## NONEQ_INCENT         1.470e-04  5.926e-06  24.814  < 2e-16 ***
    ## OPTION_AWARDS_FV     7.589e-05  4.234e-06  17.925  < 2e-16 ***
    ## OTHCOMP              2.074e-04  9.874e-06  21.001  < 2e-16 ***
    ## log(SALARY)          8.384e-01  1.453e-02  57.691  < 2e-16 ***
    ## SHRS_VEST_VAL       -1.238e-05  1.994e-06  -6.205 5.84e-10 ***
    ## STOCK_AWARDS_FV      7.979e-05  2.374e-06  33.605  < 2e-16 ***
    ## AT                   1.760e-07  7.182e-08   2.450 0.014301 *  
    ## DLTT                -5.007e-07  9.619e-08  -5.206 2.00e-07 ***
    ## NI                   1.256e-05  3.480e-06   3.610 0.000309 ***
    ## GENDERMALE           7.266e-02  2.714e-02   2.677 0.007449 ** 
    ## DesignationCFO      -1.129e-01  2.259e-02  -5.000 5.91e-07 ***
    ## DesignationDirector -2.265e-02  3.004e-02  -0.754 0.450810    
    ## DesignationOthers   -1.378e-01  1.963e-02  -7.021 2.45e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5082 on 5984 degrees of freedom
    ## Multiple R-squared:  0.714,  Adjusted R-squared:  0.7133 
    ## F-statistic: 995.8 on 15 and 5984 DF,  p-value: < 2.2e-16

``` r
# Summary of the final model
final_model <- renum_data %>% select(-c(CO_PER_ROL, TITLE, AT, DLTT, SHRS_VEST_VAL))
summary(final_model)
```

    ##     GENDER     CEOANN     CFOANN     EXECDIR       AGE      
    ##  FEMALE: 507      :6619      :6606   0:6094   Min.   :29.0  
    ##  MALE  :7663   CEO:1551   CFO:1564   1:2076   1st Qu.:49.0  
    ##                                               Median :54.0  
    ##                                               Mean   :54.2  
    ##                                               3rd Qu.:59.0  
    ##                                               Max.   :96.0  
    ##      BONUS          NONEQ_INCENT     OPTION_AWARDS_FV     OTHCOMP        
    ##  Min.   :    0.0   Min.   :    0.0   Min.   :    0.0   Min.   :    0.00  
    ##  1st Qu.:    0.0   1st Qu.:   72.3   1st Qu.:    0.0   1st Qu.:   15.12  
    ##  Median :    0.0   Median :  321.6   Median :    0.0   Median :   40.25  
    ##  Mean   :  107.1   Mean   :  630.0   Mean   :  389.3   Mean   :  191.11  
    ##  3rd Qu.:    0.0   3rd Qu.:  740.9   3rd Qu.:  305.7   3rd Qu.:  110.18  
    ##  Max.   :32000.0   Max.   :46999.1   Max.   :77990.7   Max.   :58410.15  
    ##      SALARY        STOCK_AWARDS_FV          NI          
    ##  Min.   :  11.73   Min.   :     0.0   Min.   :-6177.00  
    ##  1st Qu.: 353.78   1st Qu.:   208.2   1st Qu.:   18.49  
    ##  Median : 480.00   Median :   684.5   Median :  104.63  
    ##  Mean   : 561.75   Mean   :  1557.1   Mean   :  601.18  
    ##  3rd Qu.: 687.50   3rd Qu.:  1700.0   3rd Qu.:  424.40  
    ##  Max.   :8303.23   Max.   :242449.1   Max.   :45687.00  
    ##       TDC1             Designation  
    ##  Min.   :    24.28   CEO     :1750  
    ##  1st Qu.:  1092.98   CFO     :1745  
    ##  Median :  2044.71   Director: 573  
    ##  Mean   :  3438.77   Others  :4102  
    ##  3rd Qu.:  3952.94                  
    ##  Max.   :243876.54

### Hetroskedascity Test

``` r
# Estimate Final Model
final_mod <- lm(log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + 
    OTHCOMP + log(SALARY) + STOCK_AWARDS_FV + NI + GENDER + Designation, data = final_model)
summary(final_mod)
```

    ## 
    ## Call:
    ## lm(formula = log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + 
    ##     OTHCOMP + log(SALARY) + STOCK_AWARDS_FV + NI + GENDER + Designation, 
    ##     data = final_model)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.4857 -0.2899  0.0341  0.2890  3.7780 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2.712e+00  9.573e-02  28.336  < 2e-16 ***
    ## AGE                 -5.659e-03  8.481e-04  -6.672 2.68e-11 ***
    ## BONUS                1.108e-04  9.555e-06  11.599  < 2e-16 ***
    ## NONEQ_INCENT         1.519e-04  5.651e-06  26.886  < 2e-16 ***
    ## OPTION_AWARDS_FV     8.709e-05  4.178e-06  20.848  < 2e-16 ***
    ## OTHCOMP              9.490e-05  6.003e-06  15.809  < 2e-16 ***
    ## log(SALARY)          8.202e-01  1.284e-02  63.893  < 2e-16 ***
    ## STOCK_AWARDS_FV      5.425e-05  1.462e-06  37.115  < 2e-16 ***
    ## NI                   1.730e-05  2.630e-06   6.579 5.02e-11 ***
    ## GENDERMALE           6.781e-02  2.454e-02   2.763  0.00574 ** 
    ## DesignationCFO      -1.606e-01  2.017e-02  -7.962 1.92e-15 ***
    ## DesignationDirector -5.012e-02  2.663e-02  -1.882  0.05988 .  
    ## DesignationOthers   -1.922e-01  1.752e-02 -10.969  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5338 on 8157 degrees of freedom
    ## Multiple R-squared:  0.6848, Adjusted R-squared:  0.6843 
    ## F-statistic:  1477 on 12 and 8157 DF,  p-value: < 2.2e-16

``` r
# Hetroskedascity Test
par(mfrow = c(2, 2))
plot(final_mod, cex.lab = 1.5)
```

![alt text]( https://github.com/SUMansi/Executive_Compensation/blob/master/EDA_Figure/unnamed-chunk-8-1.png)

``` r
# Plot Actual vs. Fitted
par(mfrow = c(1, 2))
hist(resid(final_mod), main = "Histogram of Residual", xlim = c(-4, 4), xlab = "Fitted Residual", 
    cex.lab = 1.5)
plot(predict(final_mod), log(final_model$TDC1), main = "Actual vs. Predicted", 
    xlab = "Predicted Compensation", ylab = "Actual Compensation", cex.lab = 1.5)
abline(a = 0, b = 1)
```

![alt text]( https://github.com/SUMansi/Executive_Compensation/blob/master/EDA_Figure/unnamed-chunk-8-2.png)

### Reset Test

``` r
# Perform RESET Test on the final model
F.critical <- qf(1 - 0.05, 2, 8155)  #2.996833

lmtest::resettest(final_mod, power = 2:3)  #2669
```

    ## 
    ##  RESET test
    ## 
    ## data:  final_mod
    ## RESET = 2669, df1 = 2, df2 = 8155, p-value < 2.2e-16

``` r
F.stat <- 2669

if (F.stat > F.critical) {
    paste("Reject Null Hypothesis. Model is potentially mis-specified. It is missing some regressors or inadequate.")
} else {
    paste("Do Not Reject Null Hypothesis. Model is adequate.")
}
```

    ## [1] "Reject Null Hypothesis. Model is potentially mis-specified. It is missing some regressors or inadequate."

### Impact of Gender on CEOs Total Compensation

``` r
New <- renum_data %>% mutate(Old = ifelse(AGE > 45, 1, ifelse(AGE <= 45, 0, 
    99))) %>% mutate(CEO_Flag = ifelse((Designation == "CEO"), 1, 0), CFO_Flag = ifelse((Designation == 
    "CFO"), 1, 0), Director_Flag = ifelse((Designation == "Director"), 1, 0))

# Male/Female dummy
New$Female <- ifelse(New$GENDER == "MALE", 0, 1)


# A. What is the approximate difference in CEO Total compensation between
# Male and Female? Is this difference statistically significant?

A <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + 
    OTHCOMP + STOCK_AWARDS_FV + NI + AGE + CEO_Flag * Female, data = New)

summary(A)
```

    ## 
    ## Call:
    ## lm(formula = log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + 
    ##     OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV + NI + AGE + 
    ##     CEO_Flag * Female, data = New)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.5507 -0.2948  0.0302  0.2881  3.7965 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       2.561e+00  8.445e-02  30.323  < 2e-16 ***
    ## BONUS             1.125e-04  9.570e-06  11.759  < 2e-16 ***
    ## log(SALARY)       8.206e-01  1.286e-02  63.798  < 2e-16 ***
    ## NONEQ_INCENT      1.535e-04  5.655e-06  27.143  < 2e-16 ***
    ## OPTION_AWARDS_FV  8.755e-05  4.186e-06  20.916  < 2e-16 ***
    ## OTHCOMP           9.614e-05  6.010e-06  15.997  < 2e-16 ***
    ## STOCK_AWARDS_FV   5.461e-05  1.463e-06  37.330  < 2e-16 ***
    ## NI                1.680e-05  2.633e-06   6.381 1.86e-10 ***
    ## AGE              -4.797e-03  8.300e-04  -5.779 7.77e-09 ***
    ## CEO_Flag          1.607e-01  1.697e-02   9.471  < 2e-16 ***
    ## Female           -8.485e-02  2.662e-02  -3.187  0.00144 ** 
    ## CEO_Flag:Female   9.849e-02  6.930e-02   1.421  0.15528    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5349 on 8158 degrees of freedom
    ## Multiple R-squared:  0.6835, Adjusted R-squared:  0.6831 
    ## F-statistic:  1602 on 11 and 8158 DF,  p-value: < 2.2e-16

### Impact of Age on Executive's Total Compensation (Chow test)

``` r
B <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + 
    OTHCOMP + STOCK_AWARDS_FV + NI + GENDER + Designation + Old, data = New)

summary(B)
```

    ## 
    ## Call:
    ## lm(formula = log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + 
    ##     OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV + NI + GENDER + 
    ##     Designation + Old, data = New)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.5389 -0.2925  0.0357  0.2921  3.8122 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2.425e+00  8.797e-02  27.572  < 2e-16 ***
    ## BONUS                1.109e-04  9.578e-06  11.577  < 2e-16 ***
    ## log(SALARY)          8.079e-01  1.287e-02  62.767  < 2e-16 ***
    ## NONEQ_INCENT         1.512e-04  5.663e-06  26.701  < 2e-16 ***
    ## OPTION_AWARDS_FV     8.746e-05  4.187e-06  20.889  < 2e-16 ***
    ## OTHCOMP              9.350e-05  6.015e-06  15.543  < 2e-16 ***
    ## STOCK_AWARDS_FV      5.491e-05  1.465e-06  37.487  < 2e-16 ***
    ## NI                   1.698e-05  2.636e-06   6.442 1.25e-10 ***
    ## GENDERMALE           6.959e-02  2.461e-02   2.828  0.00469 ** 
    ## DesignationCFO      -1.362e-01  2.003e-02  -6.801 1.11e-11 ***
    ## DesignationDirector -6.726e-02  2.657e-02  -2.531  0.01138 *  
    ## DesignationOthers   -1.762e-01  1.745e-02 -10.100  < 2e-16 ***
    ## Old                  4.766e-02  1.907e-02   2.500  0.01244 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5351 on 8157 degrees of freedom
    ## Multiple R-squared:  0.6833, Adjusted R-squared:  0.6829 
    ## F-statistic:  1467 on 12 and 8157 DF,  p-value: < 2.2e-16

``` r
# Chow Test
m <- 10  # number of restrictions 
n <- dim(New)[1]


full.1 <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + 
    OTHCOMP + STOCK_AWARDS_FV + NI + GENDER + Designation, data = subset(New, 
    Old == 1))

full.2 <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + 
    OTHCOMP + STOCK_AWARDS_FV + NI + GENDER + Designation, data = subset(New, 
    Old == 0))

full <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + 
    OTHCOMP + STOCK_AWARDS_FV + NI + GENDER + Designation + Old, data = New)

rest <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + 
    OTHCOMP + STOCK_AWARDS_FV + NI + GENDER + Designation, data = New)

SSE.1 <- sum(resid(full.1)^2)
SSE.2 <- sum(resid(full.2)^2)
SSE.full <- sum(resid(full)^2)
SSE.rm <- sum(resid(rest)^2)

chow <- ((SSE.rm - (SSE.1 + SSE.2))/m)/((SSE.1 + SSE.2)/(n - 2 * m))  #24.97031

pval.chow <- 1 - pf(chow, m, n - 2 * m)

out <- list(SSE.1 = SSE.1, SSE.2 = SSE.2, SSE.rm = SSE.rm, chow = chow, pval.chow = pval.chow)

sapply(out, cbind)
```

    ##     SSE.1     SSE.2    SSE.rm      chow pval.chow 
    ## 1738.6999  250.5378 2337.1209  142.5294    0.0000

``` r
# F.critical Value
F.critical <- qf(1 - 0.05, m, n - 2 * m)

if (F.critical > chow) {
    print("Do not Reject Null hypothesis")
} else {
    print("Reject Null hypothesis")
}
```

    ## [1] "Reject Null hypothesis"

### Impact of Designation on Executive's Total Compensation

``` r
C <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + 
    OTHCOMP + STOCK_AWARDS_FV + NI + AGE + GENDER + CEO_Flag, data = New)
CC <- summary(C)


# Perform F Test to say that designation doesn't have any impact on the
# Compensation (H0: Coefficient of CEO_Flag = 0 ,H1: Coefficient of CEO_Flag
# NotEqual 0)

m <- 1  # number of restrictions
n <- dim(New)[1]  # sample size
p <- 10  # number of regressors in full model

rest <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + 
    OTHCOMP + STOCK_AWARDS_FV + NI + AGE + GENDER, data = New)

SSE.fm <- sum(resid(C)^2)
SSE.rm <- sum(resid(rest)^2)
SSR.diff <- SSE.rm - SSE.fm

F.rm <- (SSR.diff/m)/(SSE.fm/(n - p - 1))  #89.208  
F.critical <- qf(1 - 0.05, m, n - p - 1)  #3.842599

if (F.rm > F.critical) {
    paste("Reject Null Hypothesis. Adding designation to the model adds explanatory power and the difference in compensation for CEO's and Non-CEO's is statistically significant.")
} else {
    paste("Do Not Reject Null Hypothesis")
}
```

    ## [1] "Reject Null Hypothesis. Adding designation to the model adds explanatory power and the difference in compensation for CEO's and Non-CEO's is statistically significant."

### Impact of Company's performance on CEOs Total Compensation

``` r
New <- New %>% mutate(HighNI = ifelse(NI > 0, 1, ifelse(NI <= 0, 0, 1)))

D <- lm(log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + OPTION_AWARDS_FV + 
    OTHCOMP + STOCK_AWARDS_FV + AGE + GENDER + CEO_Flag * HighNI, data = New)
summary(D)
```

    ## 
    ## Call:
    ## lm(formula = log(TDC1) ~ BONUS + log(SALARY) + NONEQ_INCENT + 
    ##     OPTION_AWARDS_FV + OTHCOMP + STOCK_AWARDS_FV + AGE + GENDER + 
    ##     CEO_Flag * HighNI, data = New)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.8240 -0.3003  0.0276  0.2868  3.8590 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       2.402e+00  8.781e-02  27.351  < 2e-16 ***
    ## BONUS             1.201e-04  9.519e-06  12.618  < 2e-16 ***
    ## log(SALARY)       8.338e-01  1.272e-02  65.527  < 2e-16 ***
    ## NONEQ_INCENT      1.557e-04  5.670e-06  27.461  < 2e-16 ***
    ## OPTION_AWARDS_FV  8.772e-05  4.196e-06  20.908  < 2e-16 ***
    ## OTHCOMP           9.696e-05  6.025e-06  16.092  < 2e-16 ***
    ## STOCK_AWARDS_FV   5.676e-05  1.428e-06  39.762  < 2e-16 ***
    ## AGE              -4.823e-03  8.324e-04  -5.794 7.11e-09 ***
    ## GENDERMALE        6.500e-02  2.463e-02   2.639  0.00834 ** 
    ## CEO_Flag          1.558e-01  3.550e-02   4.388 1.16e-05 ***
    ## HighNI            2.410e-02  1.747e-02   1.380  0.16771    
    ## CEO_Flag:HighNI  -4.560e-03  3.816e-02  -0.119  0.90490    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5362 on 8158 degrees of freedom
    ## Multiple R-squared:  0.682,  Adjusted R-squared:  0.6815 
    ## F-statistic:  1590 on 11 and 8158 DF,  p-value: < 2.2e-16

``` r
(exp(-0.0045595 + 0.0241013) - 1) * 100
```

    ## [1] 1.973399

### To Verify the joint significance of Bonus & Other Compensation(i.e. OTHCOMP)

``` r
New$BonusSq <- New$BONUS^2
New$OTHCOMPSq <- New$OTHCOMP^2
New$SALARYSq <- New$SALARY^2

E <- lm(log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + OTHCOMP + 
    log(SALARY) + STOCK_AWARDS_FV + GENDER + Designation + NI + BonusSq + OTHCOMPSq, 
    data = New)
summary(E)
```

    ## 
    ## Call:
    ## lm(formula = log(TDC1) ~ AGE + BONUS + NONEQ_INCENT + OPTION_AWARDS_FV + 
    ##     OTHCOMP + log(SALARY) + STOCK_AWARDS_FV + GENDER + Designation + 
    ##     NI + BonusSq + OTHCOMPSq, data = New)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -8.4848 -0.2795  0.0368  0.2837  3.8254 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          2.654e+00  9.235e-02  28.741  < 2e-16 ***
    ## AGE                 -5.934e-03  8.182e-04  -7.252 4.47e-13 ***
    ## BONUS                2.596e-04  1.459e-05  17.790  < 2e-16 ***
    ## NONEQ_INCENT         1.587e-04  5.457e-06  29.083  < 2e-16 ***
    ## OPTION_AWARDS_FV     8.762e-05  4.029e-06  21.747  < 2e-16 ***
    ## OTHCOMP              2.588e-04  9.719e-06  26.626  < 2e-16 ***
    ## log(SALARY)          8.223e-01  1.238e-02  66.421  < 2e-16 ***
    ## STOCK_AWARDS_FV      5.495e-05  1.411e-06  38.937  < 2e-16 ***
    ## GENDERMALE           6.764e-02  2.367e-02   2.858 0.004279 ** 
    ## DesignationCFO      -1.292e-01  1.949e-02  -6.631 3.54e-11 ***
    ## DesignationDirector -5.817e-02  2.568e-02  -2.265 0.023554 *  
    ## DesignationOthers   -1.607e-01  1.695e-02  -9.483  < 2e-16 ***
    ## NI                   8.742e-06  2.571e-06   3.400 0.000676 ***
    ## BonusSq             -1.004e-08  7.305e-10 -13.746  < 2e-16 ***
    ## OTHCOMPSq           -5.030e-09  2.431e-10 -20.688  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5148 on 8155 degrees of freedom
    ## Multiple R-squared:  0.7069, Adjusted R-squared:  0.7064 
    ## F-statistic:  1405 on 14 and 8155 DF,  p-value: < 2.2e-16

``` r
F.criticalE <- qf(1 - 0.05, 2, 8154)  #2.996833

lmtest::resettest(E, power = 2:3)
```

    ## 
    ##  RESET test
    ## 
    ## data:  E
    ## RESET = 2700.7, df1 = 2, df2 = 8153, p-value < 2.2e-16
