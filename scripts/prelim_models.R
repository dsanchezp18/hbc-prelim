# ==================================== LAPOP ECUADOR DATA MANIPULATION ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# May 2021
# Script for running preliminary models, LPM and Probit-Logit 

# ================================================= Preliminaries =============================================================

# No working directory needed as an Rproject is being used
# The initial dataframe used will be the one done from the join bases and data manipulation script

# Libraries used in the script ------------------------------------------------------------------------------------------------

library(tidyverse) # Dplyr, tibble
library(broom) # Tidy
library(car) # For heteroskedasticity
library(lmtest) # Same
library(stargazer) # Build text and LaTeX tables

# ================================================= LPMs ======================================================================
# Corruption Tolerance, Labor Market Status -----------------------------------------------------------------------------------

# Estimate a simple model which identifies probabilities for unemployed people or otherwise, no other regressors

lpm_unem<-lm(ctol~unem_4a, data=df)
summary(lpm_unem)

lpm_unemh<-coeftest(lpm_unem, vcov=hccm)
print(lpm_unemh)

# Do the same , but now add years

lpm_unemy<-lm(ctol~unem_4a+year, data=df)
summary(lpm_unem)

lpm_unemyh<-coeftest(lpm_unemy, vcov=hccm)
print(lpm_unemyh)

# Note that the base year is the first one 2008, as 2006 and 2004 do not have the ocup4a question, thus no UNEM effect can be 
# estimated for these years.

# An alternative procedure: the same but for employed variable

lpm_em<-lm(ctol~em_4a, data=df)
summary(lpm_em)

lpm_emh<-coeftest(lpm_em, vcov=hccm)
print(lpm_emh)

# Do the same , but now add years

lpm_emy<-lm(ctol~em_4a+year, data=df)
summary(lpm_em)

lpm_emyh<-coeftest(lpm_emy, vcov=hccm)
print(lpm_emyh)

# Estimate a simple model which identifies probabilities for kind of labor market status, the 7 types as defined by the 
# 2008-2019 LAPOP 

lpm_ocup4a<-lm(ctol~ocup4a, data=df)
summary(lpm_ocup4a)

lpm_ocup4ah<-coeftest(lpm_ocup4a, vcov= hccm)
print(lpm_ocup4ah)

# The levels are:
# 1- Works
# 2- Has a job, but not working right now (basically the same as 1)
# 3- Unemployed and actively looking for a job
# 4- Student
# 5- Taking care of the house
# 6- Retired, pensioner or unable to work
# 7- Unemployed, but not actively looking 

# Add year dummies to the previous model

lpm_ocup4ay<-lm(ctol~ocup4a+year, data=df)
summary(lpm_ocup4a)

lpm_ocup4ayh<-coeftest(lpm_ocup4ay, vcov= hccm)
print(lpm_ocup4ayh)

# Now do the same but for the labor market status variable I defined

lpm_work<-lm(ctol~work+year, data=df)
summary(lpm_work)

lpm_workh<-coeftest(lpm_work, vcov=hccm)
print(lpm_workh)

# The reference level is those who work

# Corruption tolerance LPM with economic situation ----------------------------------------------------------------------------

# Will analyze how corruption tolerance responds to people with worse economic situations relative to the previous year
# Must take into account, most of all, the big jump between 2014 and 2016 on corruption tolerance

# First, create a dataframe with only these two years

df_1416<-subset(df, year== 2014 | year== 2016) 
df_1416$year<-df_1416$year %>% as.factor()

# Now run a simple model, where the DID is the difference in probability of choosing yes for those in worse ec. conditions
# In the year differences

did_ec<-lm(ctol~year*econ_sit, data=df_1416)
summary(did_ec)

did_ech<-coeftest(did_ec, vcov=hccm)
print(did_ech)

# Try again, but with 2012 and 2014 vs 2016 and 2019, both periods have 2 years so they are balanced 

# Create dataframe

df_2469<-subset(df, year== 2014 | year== 2016 | year== 2012 | year== 2019)

# Create a dummy which equals 1 for the "post" period: 2016 and 2019 which is after we see the jump 

df_2469$post<-ifelse(df_2469$year==2016 |df_2469$year==2019,1,0)

# Rerun the model

did_ec_2y<-lm(ctol~post*econ_sit, data=df_2469)
summary(did_ec_2y)

did_ec_2yh<-coeftest(did_ec_2y, vcov=hccm)
print(did_ec_2yh)

# Add the year dummies to see if it changes anything

did_ec_2ye<-lm(ctol~post*econ_sit+year, data=df_2469)
summary(did_ec_2ye)

did_ec_2yeh<-coeftest(did_ec_2ye, vcov=hccm)
print(did_ec_2yeh)

# Try again but with all data available, 2004-2014 vs 2016 to 2019, use the dummies made before. 

# Now create the model, all year dummies and interacted with economic situation

did_ec_all<-lm(ctol~y06+y08+y10+y12+y14+y16*econ_sit+y19, data=df)
summary(did_ec_all)

did_ec_allh<-coeftest(did_ec_all, vcov=hccm)
print(did_ec_allh)

# Try defining a pre and post period

df$post<- ifelse( df$year == 2016 | df$year == 2019, 1, 0)

# Now repeat the regression 

did_ec_alp<-lm(ctol~year+post*econ_sit, data=df)
summary(did_ec_alp)

did_ec_alph<-coeftest(did_ec_alp, vcov=hccm)
print(did_ec_alph)

# Preliminary conclusion: no matter the year, a person that is in worse economic conditions tends to be more tolerant of bribes
# Gives a circumstantial explanation to corruption 
# There is still, however, no clear explanation to the jump in corruption as the DID estimate not significant

# Corruption Tolerance, Economic Situation with extra vars ---------------------------------------------------------------

# Add some basic variables to the three models that were estimated before
# Year dummies, economic situation, gender, age, unemployment indicator, education years. 

# The simple 2014 vs 2016

did_ec_dem<-lm(ctol~year*econ_sit+gndr+age+unem_4a+ed, data=df_1416)
summary(did_ec_dem)

did_ec_demh<-coeftest(did_ec_dem, vcov=hccm)
print(did_ec_demh)

# Note that we add an unemployed dummy, to account the differences for this labor market status

# Now adding 2012 and 2019

did_ec_dem2<-lm(ctol~post*econ_sit+gndr+age+unem_4a+ed, data=df_2469)
summary(did_ec_dem2)

did_ec_dem2h<-coeftest(did_ec_dem2, vcov=hccm)
print(did_ec_dem2h)

# Here, education years becomes important, is a deterrent to corruption

# Now with all the years available

did_ec_dem3<-lm(ctol~year+econ_sit+gndr+age+unem_4a+ed, data=df)
summary(did_ec_dem3)

did_ec_dem3h<-coeftest(did_ec_dem3, vcov=hccm)
print(did_ec_dem3h)

# Remember we still have no clear way of discerning unemployment in years prior to 2008

# Consider a special model that interacts education years with age, no DID

lpm_ageed<-lm(ctol~year+econ_sit+gndr+unem_4a+ed*age, data= df)
summary(lpm_ageed)

lpm_ageedh<-coeftest(lpm_ageed, vcov=hccm)
summary(lpm_ageed)

# A more educated and older person is less likely to justify corruption

# Add another DID, repeat for all years, now consider interaction between unemployment and year

# 2014-2016

did2_ec_1416<-lm(ctol~year*(econ_sit+unem_4a)+gndr+age+ed, data=df_1416)
summary(did2_ec_1416)

did2_ec_1416h<-coeftest(did2_ec_1416,vcov=hccm)
print(did2_ec_1416h)

# 2012, 2014 vs 2016, 2019

did2_ec_2469<-lm(ctol~post*(econ_sit+unem_4a)+gndr+age+ed, data=df_2469)
summary(did2_ec_2469)

did2_ec_2469h<-coeftest(did2_ec_2469,vcov=hccm)
print(did2_ec_2469h)

# All years available

did2_ec_all<-lm(ctol~post*(econ_sit+unem_4a)+gndr+age+ed, data=df)
summary(did2_ec_all)

did2_ec_allh<-coeftest(did2_ec_all, vcov=hccm)
print(did2_ec_allh)

# Corruption tolerance LPM with income situation -----------------------------------------------------------------------------------

# Same but now with the income question, which has only appeared in all years since 2015

# We have the 2014 and 2016 dataframe, just rerun the model

#  Now rerun the model 

did_inc<-lm(ctol~year*inc_sit, data=df_1416)
summary(did_inc)

did_inch<-coeftest(did_inc, vcov=hccm)
print(did_inch)

# Now for the 2012 and 2019 inclusions, rerun the model again as we still have that dataframe

did_inc_2y<-lm(ctol~post*inc_sit, data=df_2469)
summary(did_inc_2y)

did_inc_2yh<-coeftest(did_inc_2y, vcov=hccm)
print(did_inc_2yh)

# Now get a base only up to 2010 for the model with all available data for the income question

df_2010up<-subset(df, year== 2014 | year== 2016 | year== 2012 | year== 2019 | year == 2010)

# Rerun the previous model, with year dummies and the DID 

did_inc_all<-lm(ctol~y12+y14+y16*inc_sit+y19, data=df_2010up)
summary(did_inc_all)

did_inc_allh<-coeftest(did_inc_all, vcov=hccm)
print(did_inc_allh)

# This income variable is less significant

# Corruption Tolerance with income situation, other vars -----------------------------------------------------------------

# Add some basic variables to the three models that were estimated before

# The simple 2014 vs 2016

did_inc_dem<-lm(ctol~year*inc_sit+gndr+age+unem_4a+ed, data=df_1416)
summary(did_ec_dem)

did_inc_demh<-coeftest(did_inc_dem, vcov=hccm)
print(did_inc_demh)

# Now adding 2012 and 2019

did_ec_dem2<-lm(ctol~post*econ_sit+gndr+age+unem_4a+ed, data=df_2469)
summary(did_ec_dem2)

did_ec_dem2h<-coeftest(did_ec_dem2, vcov=hccm)
print(did_ec_dem2h)

# Now with all the years available

did_ec_dem3<-lm(ctol~post*econ_sit+gndr+age+unem+ed, data=df)
summary(did_ec_dem3)

did_ec_dem3h<-coeftest(did_ec_dem3, vcov=hccm)
print(did_ec_dem3h)

# Add another DID, repeat for all years, now consider interaction between unemployment and year

# 2014-2016

did2_ec_1416<-lm(ctol~year*(econ_sit+unem)+gndr+age+ed, data=df_1416)
summary(did2_ec_1416)

did2_ec_1416h<-coeftest(did2_ec_1416,vcov=hccm)
print(did2_ec_1416h)

# 2012, 2014 vs 2016, 2019

did2_ec_2469<-lm(ctol~post*(econ_sit+unem)+gndr+age+ed, data=df_2469)
summary(did2_ec_2469)

did2_ec_2469h<-coeftest(did2_ec_2469,vcov=hccm)
print(did2_ec_2469h)

# All years available

did2_ec_all<-lm(ctol~post*(econ_sit+unem)+gndr+age+ed, data=df)
summary(did2_ec_all)

did2_ec_allh<-coeftest(did2_ec_all, vcov=hccm)
print(did2_ec_allh)


# Corruption Tolerance LPMs, misc variables ------------------------------------------------------------------------------
# Here I try out multiple models with the general dataframe

pr1<-lm(ctol~year+unem+ed+age+gndr, data=df)
summary(pr1)




# ============================================= General Models ================================================================

# LAPOP 2019 Model -------------------------------------------------------------------------------------------------------

# 1. Linear Probability Model, with pooled OLS, replicates to an extent the one in 2019 LAPOP report

lpm_l19<-lm(ctol~year+gndr+age+ed+econ_sit+pol_int+exc7+eff1+eff2+ur+ec_eval, data=df)
summary(lpm_l19)

lpm_l19h<-coeftest(lpm_l19, vcov=hccm)
print(lpm_l19h)

# 2. Logit Model 

log_l19<-glm(ctol~year+gndr+age+ed+econ_sit+pol_int+exc7+eff1+eff2+ur+ec_eval, 
            data=df,
            family = binomial(link='logit'))
summary(log_l19)

# 3. Probit Model 

prob_l19<-glm(ctol~year+gndr+age+ed+econ_sit+pol_int+exc7+eff1+eff2+ur+ec_eval, 
             data=df,
             family = binomial(link='probit'))
summary(prob_l19)

# Create a table that shows all models 

stargazer(lpm_l19, log_l19, prob_l19, type='text')


# My base model ----------------------------------------------------------------------------------------------------------

# Create my base model, with all the variables that seemed important and significant

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

# DID models -------------------------------------------------------------------------------------------------------------


# Investigate: support/sympathy for a political party (vb10)

# Create a model that studies corruption tolerance based in political party afiliation

