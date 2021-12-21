# ========================================== LAPOP ECUADOR: ATTITUDES ON CORRUPTION ==========================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# May 2021
# Script for models regarding the 2014 and 2016 dataframe, of prime interest to explain corruption tolerance jump

# ================================================= Preliminaries =============================================================

# No working directory needed as an Rproject is being used
# Here the df46 dataframe is used, result of running the join bases and data manipulation script

# Libraries used in the script ------------------------------------------------------------------------------------------------

library(tidyverse) # Dplyr, tibble
library(broom) # Tidy
library(car) # For heteroskedasticity
library(lmtest) # Same
library(stargazer) # Build text and LaTeX tables
library(sandwich) # Calculate the errors for stargazer output
library(survey) # For complex-survey design effects
library(margins) # For average partial effects

# ================================================ Base Models ================================================================

# Complex Base Model: All obvious controls  ----------------------------------------------------------------

# Estimate the most basic model with only controls to be evaluated at every other model

# 1. LPM

lpm_base1416<-lm(ctol~ year + age + gndr + ed + region,
                 data = df46)
summary(lpm_base1416)

lpm_base1416h<-coeftest(lpm_base1416, 
                        vcov=hccm(lpm_base1416, 
                                  type='hc1'))
print(lpm_base1416h)

# Estimate errors for stargazer output

cv1<-vcovHC(lpm_base1416, type = 'HC1')
err1<-sqrt(diag(cv1))

# 2. Probit

prob_base1416<-glm(ctol~ year + age + gndr + ed + region,
                 data = df46,
                 family = binomial(link = 'probit') )
summary(prob_base1416)

# 3. Logit 

log_base1416<-glm(ctol~ year + age + gndr + ed + region, 
                   data = df46,
                   family = binomial(link = 'logit') )
summary(log_base1416)

# Create a table with all models

stargazer(lpm_base1416, 
          log_base1416, 
          prob_base1416, 
          se = list(err1),
          type = 'text')

# ========================================== DiD Base Models ==================================================

# Estimate simple models which only look at the DiD in isolation

# Economic Situation -----------------------------------------------------------------------------------------------------

# Try the DiD interacting the economic situation question with the year, nothing else

# LPM

lpm_did_ecsit<-lm(ctol~ year * econ_sit,
              data = df46)
summary(lpm_did_ecsit)

lpm_did_ecsith<-coeftest(lpm_did_ecsit, vcov = hccm(lpm_did_ecsit, type = 'hc1'))
print(lpm_did_ecsith)

# Estimate the errors

ve<-vcovHC(lpm_did_ecsit, type = 'HC1')
se<-diag(sqrt(ve))

# Probit

prob_did_ecsit<-glm(ctol~ year * econ_sit,
                  data = df46,
                  family = binomial(link = 'probit'))
summary(prob_did_ecsit)

# Logit

log_did_ecsit<-glm(ctol~ year * econ_sit,
                   data = df46,
                   family = binomial(link = 'logit'))
summary(log_did_ecsit)

# Show all three

stargazer(lpm_did_ecsit,
          prob_did_ecsit, 
          log_did_ecsit,
          se = list(se),
          type = 'text')

# Income situation -------------------------------------------------------------------------------------------------------

# Try the DiD interacting the income situation question with the year, nothing else

# LPM

lpm_did_incsit<-lm(ctol~ year * inc_sit,
                  data = df46)
summary(lpm_did_incsit)

lpm_did_incsith<-coeftest(lpm_did_incsit, vcov = hccm(lpm_did_incsit, type = 'hc1'))
print(lpm_did_incsith)

# Estimate the errors

vi<-vcovHC(lpm_did_incsit, type = 'HC1')
si<-diag(sqrt(vi))

# Probit

prob_did_incsit<-glm(ctol~ year * inc_sit,
                    data = df46,
                    family = binomial(link = 'probit'))
summary(prob_did_incsit)

# Logit

log_did_incsit<-glm(ctol~ year * inc_sit,
                   data = df46,
                   family = binomial(link = 'logit'))
summary(log_did_incsit)

# Show all three

stargazer(lpm_did_incsit,
          prob_did_incsit, 
          log_did_incsit,
          se = list(se),
          type = 'text')

# Note: on their own, economic situation of the person doesn't seem to be especially significant, see explanation on PPOINT

# Unemployment -----------------------------------------------------------------------------------------------------------

# Try the DiD interacting the open unemployment question with the year, nothing else

# LPM

lpm_did_unem1<-lm(ctol~ year * unem_4a,
                  data = df46)
summary(lpm_did_unem1)

lpm_did_unem1h<-coeftest(lpm_did_unem1, vcov = hccm(lpm_did_unem1, type = 'hc1'))
print(lpm_did_unem1h)

# Estimate the errors

vu1<-vcovHC(lpm_did_unem1, type = 'HC1')
su1<-diag(sqrt(vu1))

# Probit

prob_did_unem1<-glm(ctol~ year * unem_4a,
                     data = df46,
                     family = binomial(link = 'probit'))
summary(prob_did_unem1)

# Logit

log_did_unem1<-glm(ctol~ year * unem_4a,
                    data = df46,
                    family = binomial(link = 'logit'))
summary(log_did_unem1)

# Show all three

stargazer(lpm_did_unem1,
          prob_did_unem1, 
          log_did_unem1,
          se = list(su1),
          type = 'text')

# Now try the same, but with total unemployment, open and hidden

# LPM

lpm_did_unem2<-lm(ctol~ year * unem2_4a,
                  data = df46)
summary(lpm_did_unem2)

lpm_did_unem2h<-coeftest(lpm_did_unem2, vcov = hccm(lpm_did_unem2, type = 'hc1'))
print(lpm_did_unem2h)

# Estimate the errors

vu2<-vcovHC(lpm_did_unem2, type = 'HC1')
su2<-diag(sqrt(vu2))

# Probit

prob_did_unem2<-glm(ctol~ year * unem2_4a,
                    data = df46,
                    family = binomial(link = 'probit'))
summary(prob_did_unem2)

# Logit

log_did_unem2<-glm(ctol~ year * unem2_4a,
                   data = df46,
                   family = binomial(link = 'logit'))
summary(log_did_unem2)

# Show all three

stargazer(lpm_did_unem2,
          prob_did_unem2, 
          log_did_unem2,
          se = list(su2),
          type = 'text')

# Interest in Politics ---------------------------------------------------------------------------------------------------

# Same but with the dichotomized interest in politics variable 

# LPM

lpm_did_intp<-lm(ctol~ year * pint_dic,
                  data = df46)
summary(lpm_did_intp)

lpm_did_intph<-coeftest(lpm_did_intp, vcov = hccm(lpm_did_intp, type = 'hc1'))
print(lpm_did_intph)

# Estimate the errors

vin<-vcovHC(lpm_did_intp, type = 'HC1')
sin<-diag(sqrt(vin))

# Probit

prob_did_intp<-glm(ctol~ year * pint_dic,
                    data = df46,
                    family = binomial(link = 'probit'))
summary(prob_did_intp)

# Logit

log_did_intp<-glm(ctol~ year * pint_dic,
                   data = df46,
                   family = binomial(link = 'logit'))
summary(log_did_intp)

# Show all three

stargazer(lpm_did_intp,
          prob_did_intp, 
          log_did_intp,
          se = list(sin),
          type = 'text')

# Note: not significant, interst has not really changed according to the dichotomized variable 

# Political Score --------------------------------------------------------------------------------------------------------

# Same but with the discrete political score variable 

# LPM

lpm_did_pols<-lm(ctol~ year * polscore,
                 data = df46)
summary(lpm_did_pols)

lpm_did_polsh<-coeftest(lpm_did_pols, vcov = hccm(lpm_did_pols, type = 'hc1'))
print(lpm_did_polsh)

# Estimate the errors

vp<-vcovHC(lpm_did_pols, type = 'HC1')
sp<-diag(sqrt(vp))

# Probit

prob_did_pols<-glm(ctol~ year * polscore,
                   data = df46,
                   family = binomial(link = 'probit'))
summary(prob_did_pols)

# Logit

log_did_pols<-glm(ctol~ year * polscore,
                  data = df46,
                  family = binomial(link = 'logit'))
summary(log_did_pols)

# Show all three

stargazer(lpm_did_pols,
          prob_did_pols, 
          log_did_pols,
          se = list(sp),
          type = 'text')


# Labor Market Type - Natural Experiment DID -----------------------------------------------------------------------------

# Create a DiD with the self-employed variable 

# LPM

lpm_did_semp<-lm(ctol~ year * self_emp,
                 data = df46)
summary(lpm_did_semp)

lpm_did_semph<-coeftest(lpm_did_semp, vcov = hccm(lpm_did_semp, type = 'hc1'))
print(lpm_did_semph)

# Estimate the errors

vs<-vcovHC(lpm_did_semp, type = 'HC1')
ss<-diag(sqrt(vs))

# Probit

prob_did_semp<-glm(ctol~ year * self_emp,
                   data = df46,
                   family = binomial(link = 'probit'))
summary(prob_did_semp)

# Logit

log_did_semp<-glm(ctol~ year * self_emp,
                  data = df46,
                  family = binomial(link = 'logit'))
summary(log_did_semp)

# Show all three

stargazer(lpm_did_semp,
          prob_did_semp, 
          log_did_semp,
          se = list(ss),
          type = 'text')

# Political Efficiency 1- How much do politicians care about what we think -------------------------------------------------------------------------------------------------

# Make the DiD with this variable 

# LPM

lpm_did_ef1<-lm(ctol~ year * eff1,
                 data = df46)
summary(lpm_did_ef1)

lpm_did_ef1h<-coeftest(lpm_did_ef1, vcov = hccm(lpm_did_ef1, type = 'hc1'))
print(lpm_did_ef1h)

# Estimate the errors

vef<-vcovHC(lpm_did_ef1, type = 'HC1')
sef<-diag(sqrt(vef))

# Probit

prob_did_ef1<-glm(ctol~ year * eff1,
                   data = df46,
                   family = binomial(link = 'probit'))
summary(prob_did_ef1)

# Logit

log_did_ef1<-glm(ctol~ year * eff1,
                  data = df46,
                  family = binomial(link = 'logit'))
summary(log_did_ef1)

# Show all three

stargazer(lpm_did_ef1,
          prob_did_ef1, 
          log_did_ef1,
          se = list(sef),
          type = 'text')

# Political Efficiency 2-  How much do we understand about politics ------------------------------------------------------

# Make the DiD with this variable 

# LPM

lpm_did_ef2<-lm(ctol~ year * eff2,
                data = df46)
summary(lpm_did_ef2)

lpm_did_ef2h<-coeftest(lpm_did_ef2, vcov = hccm(lpm_did_ef2, type = 'hc1'))
print(lpm_did_ef2h)

# Estimate the errors

vef2<-vcovHC(lpm_did_ef2, type = 'HC1')
sef2<-diag(sqrt(vef2))

# Probit

prob_did_ef2<-glm(ctol~ year * eff2,
                  data = df46,
                  family = binomial(link = 'probit'))
summary(prob_did_ef2)

# Logit

log_did_ef2<-glm(ctol~ year * eff2,
                 data = df46,
                 family = binomial(link = 'logit'))
summary(log_did_ef2)

# Show all three

stargazer(lpm_did_ef2,
          prob_did_ef2, 
          log_did_ef2,
          se = list(sef2),
          type = 'text')


# Protest Participation DiD ----------------------------------------------------------------------------------------------

# Same with this variable

# LPM 

lpm_did_prot<-lm(ctol~ year * prot3,
                data = df46)
summary(lpm_did_prot)

lpm_did_proth<-coeftest(lpm_did_prot, vcov = hccm(lpm_did_prot, type = 'hc1'))
print(lpm_did_proth)

# Estimate the errors

vpt<-vcovHC(lpm_did_prot, type = 'HC1')
spt<-diag(sqrt(vpt))

# Probit

prob_did_prot<-glm(ctol~ year * prot3,
                  data = df46,
                  family = binomial(link = 'probit'))
summary(prob_did_prot)

# Logit

log_did_prot<-glm(ctol~ year * prot3,
                 data = df46,
                 family = binomial(link = 'logit'))
summary(log_did_prot)

# Show all three

stargazer(lpm_did_prot,
          prob_did_prot, 
          log_did_prot,
          se = list(spt),
          type = 'text')

# President Confidence DiD -----------------------------------------------------------------------------------------------

# Same with this variable

# LPM 

lpm_did_pconf<-lm(ctol~ year * pres_conf,
                 data = df46)
summary(lpm_did_pconf)

lpm_did_pconfh<-coeftest(lpm_did_pconf, vcov = hccm(lpm_did_pconf, type = 'hc1'))
print(lpm_did_pconfh)

# Estimate the errors

vpcf<-vcovHC(lpm_did_pconf, type = 'HC1')
spcf<-diag(sqrt(vpcf))

# Probit

prob_did_pconf<-glm(ctol~ year * pres_conf,
                   data = df46,
                   family = binomial(link = 'probit'))
summary(prob_did_pconf)

# Logit

log_did_pconf<-glm(ctol~ year * pres_conf,
                  data = df46,
                  family = binomial(link = 'logit'))
summary(log_did_pconf)

# Show all three

stargazer(lpm_did_pconf,
          prob_did_pconf, 
          log_did_pconf,
          se = list(spcf),
          type = 'text')

# Base DiD Table ---------------------------------------------------------------------------------------------------------

# Create a regressions table for the base DiDs

sink('tables/DiD base models.txt')

stargazer(log_did_unem1,
          log_did_unem2,
          log_did_pols,
          log_did_pconf,
          type = 'text')

sink()

# ============================================= Somewhat significative models =============================================

# Testing multiple variable models, that show some significance in some cases.

# So far only age seems to be the most stable determinant of corruption tolerance in 2014-2016 lapop models. 

# No complex functional forms --------------------------------------------------------------------------------------------

# Estimate a key base model with significant variables, no interactions/DiDs 

# 1. LPM

lpm_simp<-lm(ctol~ year + age + gndr + ed + ur + unem2_4a + eff1 + eff2 + prot3 + pint_dic + corrper + rlg_imp, 
                 data = df46)
summary(lpm_simp)

lpm_simph<-coeftest(lpm_simp, 
                        vcov=hccm(lpm_simp, 
                                  type='hc1'))
print(lpm_simph)

# Estimate errors for stargazer output

cvs<-vcovHC(lpm_simp, type = 'HC1')
ers<-sqrt(diag(cvs))

# 2. Probit

prob_simp<-glm(ctol~ year + age + gndr + ed + ur + unem2_4a + eff1 + eff2 + prot3 + pint_dic + corrper + rlg_imp, 
                   data = df46,
                   family = binomial(link = 'probit') )
summary(prob_simp)

# 3. Logit 

log_simp<-glm(ctol~ year + age + gndr + ed + ur + unem2_4a + eff1 + eff2 + prot3 + pint_dic + corrper + rlg_imp, 
                  data = df46,
                  family = binomial(link = 'logit') )
summary(log_simp)

# Create a table with all models

stargazer(lpm_simp, 
          log_simp, 
          prob_simp, 
          se = list(ers),
          type = 'text')

# Incorporate DiDs -------------------------------------------------------------------------------------------------------

# Estimate a model incorporating the DiDs

# Estimate a key base model with significant variables, no interactions/DiDs 

# 1. LPM

lpm_incd<-lm(ctol~ year*(unem2_4a + pres_conf + polscore) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + 
             corrper + rlg_imp, 
             data = df46)
summary(lpm_incd)

lpm_incdh<-coeftest(lpm_incd, 
                    vcov=hccm(lpm_incd, 
                              type='hc1'))
print(lpm_incdh)

# Estimate errors for stargazer output

cinc<-vcovHC(lpm_incd, type = 'HC1')
einc<-sqrt(diag(cinc))

# 2. Probit

prob_incd<-glm(ctol~ year*(unem2_4a + pres_conf + polscore) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + 
               corrper + rlg_imp, 
               data = df46,
               family = binomial(link = 'probit') )
summary(prob_incd)

# 3. Logit 

log_incd<-glm(ctol~ year*(unem2_4a + pres_conf + polscore) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + 
              corrper + rlg_imp, 
              data = df46,
              family = binomial(link = 'logit') )
summary(log_incd)

# Create a table with all models

sink('tables/DiD Complex Model.txt')

stargazer(lpm_incd, 
          log_incd, 
          prob_incd, 
          se = list(einc),
          type = 'text')

sink()

# ============================================= Incorporating design effects =============================================

# Incorporate design effects through the survey package. 

# First, define the survey design: 

ab_des_ec1416<-svydesign(ids=~upm, 
                         strata=~estratopri, 
                         weights=~weight1500, 
                         nest=TRUE,
                         na.action = 'na.exclude',
                         data=df46)

# Base DiD Models --------------------------------------------------------------------------------------------------------

# Estimate base DiD models, with design effects.

# Unemployment DiD

log_unemdid<-svyglm(ctol ~ year*unem_4a, 
                    design = ab_des_ec1416,
                    family = binomial(link = 'logit') # Logit
)

prob_unemdid<-svyglm(ctol ~ year*unem_4a, 
                     design = ab_des_ec1416,
                     family = binomial(link = 'probit') # Probit
)

# Political Score (Higher scores means more identified with right wing)

log_polsdid<-svyglm(ctol ~ year*polscore, 
                    design = ab_des_ec1416,
                    family = binomial(link = 'logit') # Logit
)

prob_polsdid<-svyglm(ctol ~ year*polscore, 
                     design = ab_des_ec1416,
                     family = binomial(link = 'probit') # Probit
)

# Confidence in the president 

log_pconfdid<-svyglm(ctol ~ year*pres_conf, 
                     design = ab_des_ec1416,
                     family = binomial(link = 'logit') # Logit
)

prob_pconfdid<-svyglm(ctol ~ year*pres_conf, 
                      design = ab_des_ec1416,
                      family = binomial(link = 'probit') # Logit
)

# Estimate marginal effects for both probit and logit versions of all models

me_unemdid_log<-margins_summary(log_unemdid)

me_unemdid_prob<-margins_summary(prob_unemdid)

me_polsdid_log<-margins_summary(log_polsdid)

me_polsdid_prob<-margins_summary(prob_polsdid)

me_pconfdid_log<-margins_summary(log_pconfdid)

me_pconfdid_prob<-margins_summary(prob_pconfdid)


# Complex Models ----------------------------------------------------------------------------------------------------------

# Logit models to assess importance of several different variables

# Base complex model, with DiDs

log_incd<-svyglm(ctol~ year*(unem2_4a + pres_conf + polscore) + age + gndr + ed + ur + eff1 + eff2 + prot3 + pint_dic + 
                   corrper, 
                 design = ab_des_ec1416,
                 family = binomial(link = 'logit'))
summary(log_incd)

# Add economic situation and income



