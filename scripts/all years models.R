# ========================================== LAPOP ECUADOR: ATTITUDES ON CORRUPTION ==========================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# May 2021
# Script for models with all years or other restrictions, for comparison with 

# ================================================= Preliminaries =============================================================

# No working directory needed as an Rproject is being used
# Here the df dataframe is used, result of running the join bases and data manipulation script

# Libraries used in the script ------------------------------------------------------------------------------------------------

library(tidyverse) # Dplyr, tibble
library(broom) # Tidy
library(car) # For heteroskedasticity
library(lmtest) # Same
library(stargazer) # Build text and LaTeX tables
library(sandwich) # To estimate robust errors and use them in a stargazer function
library(jtools) # For output to Word, Excel

# =============================================== Base Models =================================================================

# Economic Situation -----------------------------------------------------------------------------------------------------

# Estimate economic situation "effect" or particularities in the corruption tolerance variable
# All year dummies with the economic situation answer

# 1. LPM 

lpm_ecsit_all<-lm(ctol ~ year + econ_sit, 
                  data = df)
summary(lpm_ecsit_all)

lpm_ecsit_allh<-coeftest(lpm_ecsit_all, vcov = hccm(lpm_ecsit_all, type = 'hc1'))
print(lpm_ecsit_allh)

# Estimate errors for stargazer output

cov1<-vcovHC(lpm_ecsit_all, type = 'HC1')
robust1<-sqrt(diag(cov1))

# 2. Probit

prob_ecsit_all<-glm(ctol ~ year + econ_sit, 
                   data = df, 
                   family = binomial(link = 'probit'))
summary(prob_ecsit_all)

# 3. Logit 

log_ecsit_all<-glm(ctol ~ year + econ_sit, 
                    data = df, 
                    family = binomial(link = 'logit'))
summary(log_ecsit_all)

# Export to text and visualize all three at the same time

stargazer(lpm_ecsit_all,
          prob_ecsit_all,
          log_ecsit_all,
          se = list(robust1),
          type= 'text')

# Conclusion: at any given year, a worse economic situation seems to increase corruption tolerance

# Political Score --------------------------------------------------------------------------------------------------------

# 1. LPM 

lpm_pols_all<-lm(ctol ~ year + polscore, 
                  data = df)
summary(lpm_pols_all)

lpm_pols_allh<-coeftest(lpm_pols_all, vcov = hccm(lpm_pols_all, type = 'hc1'))
print(lpm_pols_allh)

# Estimate errors for stargazer output

cp1<-vcovHC(lpm_polscore_all, type = 'HC1')
rp1<-sqrt(diag(cp1))

# 2. Probit

prob_pols_all<-glm(ctol ~ year + polscore, 
                    data = df, 
                    family = binomial(link = 'probit'))
summary(prob_pols_all)

# 3. Logit 

log_pols_all<-glm(ctol ~ year + polscore, 
                   data = df, 
                   family = binomial(link = 'logit'))
summary(log_pols_all)

# Export to text and visualize all three at the same time

stargazer(lpm_pols_all,
          prob_pols_all,
          log_pols_all,
          se = list(rp1),
          type= 'text')

# Conclusion: throughout the years, a person with a more right-leaning ideology has been more tolerant of corruption

# Unemployment -----------------------------------------------------------------------------------------------------------

# Estimate a simple model with only year dummies and a (total) unemployment indicator

lpm_unemt_all<-lm(ctol ~ year + unem2_4a, 
                 data = df)
summary(lpm_unemt_all)

lpm_unemt_allh<-coeftest(lpm_unemt_all, vcov = hccm(lpm_unemt_all, type = 'hc1'))
print(lpm_unemt_allh)

# Estimate errors for stargazer output

cu<-vcovHC(lpm_unemt_all, type = 'HC1')
ru<-sqrt(diag(cu))

# 2. Probit

prob_unemt_all<-glm(ctol ~ year + unem2_4a, 
                   data = df, 
                   family = binomial(link = 'probit'))
summary(prob_unemt_all)

# 3. Logit 

log_unemt_all<-glm(ctol ~ year + unem2_4a, 
                  data = df, 
                  family = binomial(link = 'logit'))
summary(log_unemt_all)

# Export to text and visualize all three at the same time

stargazer(lpm_unemt_all,
          prob_unemt_all,
          log_unemt_all,
          se = list(ru),
          type = 'text')

# Interest in Politics ---------------------------------------------------------------------------------------------------

# Create a base model with only the interest in politics dichotomized variable

lpm_intpd_all<-lm(ctol ~ year + pint_dic, 
                  data = df)
summary(lpm_intpd_all)

lpm_intpd_allh<-coeftest(lpm_intpd_all, vcov = hccm(lpm_intpd_all, type = 'hc1'))
print(lpm_intpd_allh)

# Estimate errors for stargazer output

ci<-vcovHC(lpm_intpd_all, type = 'HC1')
ri<-sqrt(diag(ci))

# 2. Probit

prob_intpd_all<-glm(ctol ~ year + pint_dic, 
                    data = df, 
                    family = binomial(link = 'probit'))
summary(prob_intpd_all)

# 3. Logit 

log_intpd_all<-glm(ctol ~ year + pint_dic, 
                   data = df, 
                   family = binomial(link = 'logit'))
summary(log_intpd_all)

# Export to text and visualize all three at the same time

stargazer(lpm_intpd_all,
          prob_intpd_all,
          log_intpd_all,
          se = list(ri),
          type= 'text')

# Base Models Regression Table -------------------------------------------------------------------------------------------

# Create a table which includes the three simple models estimated below

sink('tables/single variable base models.txt') # Use the log to create a text file

stargazer(log_ecsit_all, 
          log_pols_all,
          log_intpd_all,
          log_unemt_all,
          type = 'text')
sink()

# Complex Base Model: Only significant variables -------------------------------------------------------------------------

# Calculate the first base model I estimate, with only variables that have seemed significant before

lpm_base<-lm(ctol~year+age+econ_sit+pol_int+unem2_4a*y16+polscore+prot3, data=df)
summary(lpm_base)

lpm_baseh<-coeftest(lpm_base, vcov=hccm)
print(lpm_baseh)

log_base<-glm(ctol~year+age+econ_sit+pol_int+unem2_4a+polscore+prot3+etid,
              family = binomial (link= 'logit'),
              data=df)

prob_base<-glm(ctol~year+age+econ_sit+pol_int+unem2_4a+polscore+prot3+etid,
               family = binomial (link= 'probit'),
               data=df)

# Create a table with all models

stargazer(lpm_base, log_base, prob_base, type='text')




# ============================================== Year 2019 Models ======================================================

# Estimate models but only for 2019

# 2019 Report Model ------------------------------------------------------------------------------------------------------

# Estimate a model LIKE the one estimated in the 2019 LAPOP report

log_l19<- glm(ctol~corrper +  gndr + ur + ed + econ_sit + ec_eval + pint_dic + eff1 + eff2,
              data = df_2019,
              family = binomial(link = 'logit'))
summary(log_l19)








