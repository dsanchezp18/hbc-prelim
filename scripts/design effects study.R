# ========================================== LAPOP ECUADOR: ATTITUDES ON CORRUPTION ==========================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# May 2021
# Script for working and investigating the way that design effects change the conclusions when being accounted for

# ================================================= Preliminaries =============================================================

# No working directory needed as an Rproject is being used
# The multiyear and single year dataframes from the joinbases and data manipulation scripts will be used

# Libraries used in the script ------------------------------------------------------------------------------------------------

library(tidyverse) # Dplyr, tibble
library(broom) # Tidy
library(car) # For heteroskedasticity
library(lmtest) # Same
library(stargazer) # Build text and LaTeX tables
library(sandwich) # Calculate the errors for stargazer output
library(survey) # For design effects
library(margins) # For marginal effecgts

# ============================================= Regression Model Comparisons ================================================

# Compare how the results (coefficients and standard errors) change as we account for design effects

# Base Models for single predictors --------------------------------------------------------------------------------------

# Base DiD Models ------------------------------------------------------------------------------------------------------------

# Compare the models that use DiDs for 2014 and 2016 

# First calculate the unemployment Did thru logit without design effects

log_did_unem1<-glm(ctol~ year * unem_4a,
                   data = df46,
                   family = binomial(link = 'logit'))
summary(log_did_unem1)

# Now declare the design

lapop.design<-svydesign(ids = ~ upm, strata = ~ estratopri, weights = ~ weight1500, nest = TRUE, data = df46)

# Now recalculate the logit model with design effects

log_did_unem1_de<-svyglm(ctol~ year * unem_4a,
                   design = lapop.design,
                   family = binomial(link = 'logit'))
summary(log_did_unem1)

# Stargazer to compare

stargazer(log_did_unem1, log_did_unem1_de, type = 'text')

# Coefficients equal as well as significance, standard errors all somewhat larger

# Now make the confidence in president DiD comparison

log_did_pconf<-glm(ctol~ year * pres_conf,
                   data = df46,
                   family = binomial(link = 'logit'))
summary(log_did_pconf)

# Again but with design effects

log_did_pconf.de<-svyglm(ctol~ year * pres_conf,
                   design = lapop.design,
                   family = binomial(link = 'logit'))
summary(log_did_pconf.de)

# Stargazer to compare

stargazer(log_did_pconf, log_did_pconf.de, type = 'text')


