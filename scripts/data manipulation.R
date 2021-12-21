# ==================================== LAPOP ECUADOR DATA MANIPULATION ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# May 2021
# Script for data manipulation

# ================================================= Preliminaries =============================================================

# No working directory needed as an Rproject is being used
# No need for importing a dataframe as it uses the one from the "join databases" script, must run that script before

# Load libraries

library(tidyverse) # Ggplot, deployer, tibble
library(broom) # Tidy function
library(car) # for recode

# Start logging in a text file

sink('logs/data manipulation.txt')

# ================================================= GENERAL  =============================================================
# Year -------------------------------------------------------------------------------------------------------------------

# Make the year variable, created or remapped in the join bases script, a factor so it works in the LPMS

df$year<- as.factor(df$year)

# Create year dummies for the dataframe, consider 2004 as the base year in LPMs, to interact specific years with variables

df$y06<-ifelse(df$year == 2006, 1, 0 )
df$y08<-ifelse(df$year == 2008, 1, 0 )
df$y10<-ifelse(df$year == 2010, 1, 0 )
df$y12<-ifelse(df$year == 2012, 1, 0 )
df$y14<-ifelse(df$year == 2014, 1, 0 )
df$y16<-ifelse(df$year == 2016, 1, 0 )
df$y19<-ifelse(df$year == 2019, 1, 0 )

# ================================================= Basic Questions ================================================================

# Rename and remap some basic questions for analysis

# Gender -----------------------------------------------------------------------------------------------------------------

df$gndr<-ifelse(df$q1==1,'Man', 'Woman')

# Age --------------------------------------------------------------------------------------------------------------------

df<-rename(df, 'age'=q2) 

df$age<-as.integer(df$age) # Transform to a number

# ================================================= Sociodemographic ==============================================================
# Urban/Rural ------------------------------------------------------------------------------------------------------------

# Relabel variable 

df$ur<-ifelse(df$ur == 1, 'Urban', 'Rural')

# Age-Related Variables --------------------------------------------------------------------------------------------------

# A dummy variable signalling the people who are less than 18 (16 or 17) at the time of the survey

df$new<-ifelse(df$age < 18, 1, 0) 

# Same but as a factor

df$new_f<-ifelse(df$age < 18, 'New', 'Older') %>% as.factor()

# Age groups:

agegroups<-c(16,19,30,40,50,60,70,80,90) # 16-19, 20-30, 31-40,41-50, etc.

df$age_group<-cut(df$age,agegroups)

# Region -----------------------------------------------------------------------------------------------------------------

# Relabel the region variables 

df$region<- ifelse( df$estratopri == 901, 'Costa', 
                    ifelse ( df$estratopri == 902, 'Sierra', 'Oriente'))

# Religion ---------------------------------------------------------------------------------------------------------------

# Relabel the kind of religion 

df$rlg<-ifelse(df$q3c == 1, 'Catholic',
               ifelse(df$q3c == 2 | df$q3c == 5, 'Christian',
                      ifelse(df$q3c == 3 | df$q3c == 4 | df$q3c == 77, 'Other',
                      ifelse(df$q3c == 6 | df$q3c == 12, 'JW/MOR', 'Atheist'))))

df$cthlic<-ifelse(df$q3c == 1, 1, 0) # Catholic Dummy 

df$christian<-ifelse(df$q3c == 2 | df$q3c == 5, 1, 0) # Christian Dummy

# Relabel the importance of religion, dichotomizing it 

df$rlg_imp<-ifelse(df$q5b < 2, 1, 0) # Very and somewhat important are labelled as 1, little and nothing are 0. 

# Race -------------------------------------------------------------------------------------------------------------------

# Create a white dummy

df$white<-ifelse(df$etid == 1, 1, 0)

# News -------------------------------------------------------------------------------------------------------------------

# Rename and relabel the exposure to news variable

df<-rename(df, 'news_exp'=gi0n)

df$news_exp<- factor(df$news_exp,
                  levels = c(1,2,3,4,5),
                  labels = c('Daily', 'Weekly', 'Monthly', 'Yearly', 'Never'))

# Same but with the other question for previous years

df<-rename(df, 'news_exp1'=gi0)

df$news_exp1<- factor(df$news_exp1,
                     levels = c(1,2,3,4,5),
                     labels = c('Daily', 'Weekly', 'Monthly', 'Rarely', 'Never'))

# ================================================= Economic Variables========================================================

# idio2 Question ---------------------------------------------------------------------------------------------------------

# Question which asks if the economic situation of the respondent is the same, worse or better

# Make a dummy that equals 1 if the economical situation of the person answering is WORSE than the one a year ago

df$econ_sit<-ifelse(df$idio2==3,'Worse','Same or Better') %>% as.factor()

# q10e, income question --------------------------------------------------------------------------------------------------

# Only available up to 2008, make a dummy which equals down if income has gone down relative to last year

df$inc_sit<-ifelse(df$q10e==3, 'Decreased', 'Same or Increased') %>% as.factor()

# Relevel to show lesser income as the one in the regressions

df$inc_sit<-relevel(df$inc_sit, 'Same or Increased') # We change to the reference level that we want to compare to

# Government Aid ---------------------------------------------------------------------------------------------------------

# Rename and relabel the government aid variables

df<-rename(df, 'govaid'=wf1, 'hdb'=cct1b)

df$govaid<-ifelse(df$govaid == 2, 'No', 'Yes') # Any kind of government aid

df$hdb<- ifelse(df$hdb == 2, 'No', 'Yes') # Bono de Desarrollo Humano

# The reference group (base group) is always "No", those who do not receive any aid

# Economic evaluation of the economy -------------------------------------------------------------------------------------

# Rename and relabel the evaluation of the economy of the respondent, turn into a factor

df<-rename(df, 'ec_eval'=soct2)

df$ec_eval<-ifelse(df$ec_eval == 3, 'Worse', 'Same or Better') %>% as.factor()

# Relevel so the reference group is same or better 

df$ec_eval<-relevel(df$ec_eval, 'Same or Better')

# ================================================= Corruption ================================================================

# Exposure to corruption  ------------------------------------------------------------------------------------------------

# My own version of the corruption victimization variable
# Will equal 1 if in any of the corruption question the person answers having been offered to bribe or actually bribed
# Will equal 0 for all people who answer "No" or who have not used the services

# First, rename the variables that were not manipulated in the join bases script

df<-rename(df, 'corr_pol'= exc2, 'corr_pub' = exc6)

# All the variables have already been created, so just create the conditional at this point 

df$corr_exp<-ifelse(df$corr_pol == 1| df$corr_pub == 1| df$corr_mun == 1| df$corr_work == 1| df$corr_court== 1
                    | df$corr_health == 1 | df$corr_school== 1, 1, 0)

# Corruption Tolerance ----------------------------------------------------------------------------------------------------

# Key variable for the study

# Rename it and turn to a factor

df<-rename(df, 'ctol'=exc18)

# Corruption Perceptions -------------------------------------------------------------------------------------------------

# Dichotomize the corruption perceptions variable in a way comparable for 2014 and 2016

# First for 2019, 2014 and all previous years

df$corrper<- ifelse(df$exc7 < 3, 1, 0) # Equals 1 if the person determines a higher corruption perception 

# Now for 2016, but create a new variable to not overwrite the other variable

df$corrper16<-ifelse(df$exc7new < 3, 0, 1 )

# Now, I will join the two variables in a sole column to use it in a regression

df$corrper<- ifelse(is.na(df$corrper) == T, df$corrper16, df$corrper)


# ================================================= Labor Market ==============================================================

# Labor Market Status OCUP4A  --------------------------------------------------------------------------------------------

# For the ocup4a question, available 2008 through 2019, convert the numbers to factors for regression analysis

df$ocup4a <- as.factor(df$ocup4a)

# Create a variable that signals employment, 1 if employed 

df$em_4a<-ifelse(df$ocup4a == 1 | df$ocup4a == 2 ,1,0)

# Create a variable that signals open unemployment (only those that are looking for job)

df$unem_4a<-ifelse(df$ocup4a == 3,1,0 ) %>% as.factor()

# Create a variable that signals both kinds of unemployed, looking and not looking

df$unem2_4a<-ifelse(df$ocup4a == 3 | df$ocup4a == 7, 1, 0)

# Create a new labor market variable, with the following categories

# Works (either working right now or has a job but not working right now)
# Unemployed, either type (looking or not looking)
# Not in the workforce (Student, Retired, Works the home)

df$work_2a<-ifelse(df$ocup4a == 1 | df$ocup4a == 2, 'Employed',
                ifelse(df$ocup4a == 3 | df$ocup4a == 7, 'Unemployed','Not WF')) %>% as.factor()

# Relevel so reference is not on the workforce

df$work_2a<-relevel(df$work_2a, 'Not WF')

# Create a dummy that indicates if the person is or not in the economically active population (works or able to work)

df$eap<-ifelse(df$ocup4a == 1 | df$ocup4a == 2 | df$ocup4a == 3 | df$ocup4a == 7, 'EAP', 'Not EAP')

# Labor Market Sector -------------------------------------------------------------------------------------------------

# Create a new factor, with all 5 groups in them. 

df$ocup_sec<-factor(df$ocup1a,
                     levels = c(seq(1:5)),
                     labels = c('Public', 'Private','Owner/Partner','Self-Employed','Unpaid')) %>% as.factor()

# Create a self-employed dummy for the DiD

df$self_emp<-ifelse(df$ocup_sec == 'Self-Employed', 1, 0) %>%  as.factor()

# Create a new group/factor variable simply separating private from public. 

df$privpub<-ifelse(df$ocup_sec == 'Public', 'Public', 'Private') %>% as.factor()

# Relevel from public sector. 

df$privpub<-relevel(df$privpub, 'Private') 

# ================================================= Political vars ==============================================================

# Political Party and Ideals --------------------------------------------------------------------------------------------------------

# Rename the political ideology discrete variable 

df<-rename(df, 'polscore'=l1)

# The larger the value answered, the more to the right the person is

# Add a LEFT/RIGHT dummy, which will equal 1 if the person identifies with the right wing scores 5 and higher, left otherwise

df$pol_wing<-ifelse(df$polscore < 5, 'Left', 'Right')

# Add another, similar dummy but the 5 score is now given to the left

df$pol_wing2<-ifelse(df$polscore < 6, 'Left', 'Right')

# Add other classification, with Left, Center and Right (As in the LAPOP report)

# First, I need to add the NA values as a "None" so that the %'s are computed correctly. 

df$pol_group<-ifelse(is.na(df$polscore), 'None',
                     ifelse(df$polscore < 4, 'Left', 
                            ifelse(df$polscore < 8, 'Center', 'Right')))

# Add a politicized dummy (if he answered or not the polscore question)

df$plscr_na<-ifelse(is.na(df$polscore), 1, 0)

# Political right wing dummy, as considered by LAPOP

df$rwing<-ifelse(df$polscore > 6, 1, 0)

# Rename the vb10 question, which shows if there is any sympathy with political parties

df<-rename(df, 'pol_symp'=vb10)

df$pol_symp<-ifelse(df$pol_symp == 1, 'Yes', 'No') 

# For year 2019, rename the vb11neg variable and apply labels, what party is DISLIKED

df<-rename(df, 'pol_dis'=vb11neg)
df$pol_dis<-as.factor(df$pol_dis)

# Interest in Politics -----------------------------------------------------------------------------------------------------

# Create an interest in politics factor to difference groups of interest

df$pol_int<-ifelse(df$pol1 == 1, 'A lot', 
                   ifelse(df$pol1 == 2, 'Some',
                          ifelse(df$pol1 == 3, 'Little', 'None'))) %>% as.factor()

df$pol_int<-relevel(df$pol_int, 'None')

# Create a dummy which equals 1 if interest is a lot or some, and 0 if interest is little or none

df$pint_dic<-ifelse(df$pol_int == 'A lot' | df$pol_int == 'Some', 1, 0)

# Participation in protests ---------------------------------------------------------------------------------------------

# Relabel the variable
df$prot3<- ifelse(df$prot3 == 1, 'Yes','No') %>% as.factor()

# Confidence in President ------------------------------------------------------------------------------------------------

# Rename the variable 

df<-rename(df, 'pres_conf' = b21a)

# Dichotomize the variable as the LAPOP articles do

df$pres_conf_dic<-ifelse(df$pres_conf >= 5, 'Yes', 'No') %>% as.factor()

# Job Approval Rating ----------------------------------------------------------------------------------------------------

# Rename the variable

df<-rename(df, 'pres_aprov' = m1)

# Recode it so that a higher number means more approval

df$pres_aprov<-6-df$pres_aprov

# Dichotomize so the two higher scores represent a lot of confidence

df$pres_aprov_dic<-ifelse(df$pres_aprov > 3, 'Yes', 'No')

# ================================================= Exporting/Saving =======================================================

# Export all changes done to the dataframes in this script

# Dataframe Overwriting -------------------------------------------------------------------------------------------------

# Rewrite the dataframes so that all changes are also saved to the dataframes on their own 

df_2019<-subset(df, year == 2019)
df_2016<-subset(df, year == 2016)
df_2014<-subset(df, year == 2014)
df_2012<-subset(df, year == 2012)
df_2010<-subset(df, year == 2010)
df_2008<-subset(df, year == 2008)
df_2006<-subset(df, year == 2006)
df_2004<-subset(df, year == 2004)
df46<-subset(df, year == 2014 | year == 2016)

# Save them as R objects to use them in other Rprojects

save(df_2019,file = 'bases/rdata/LAPOP 2019 Manipulated Dataframe.Rdata')
save(df_2016,file = 'bases/rdata/LAPOP 2016 Manipulated Dataframe.Rdata')
save(df_2014,file = 'bases/rdata/LAPOP 2014 Manipulated Dataframe.Rdata')
save(df_2012,file = 'bases/rdata/LAPOP 2012 Manipulated Dataframe.Rdata')
save(df_2010,file = 'bases/rdata/LAPOP 2010 Manipulated Dataframe.Rdata')
save(df_2008,file = 'bases/rdata/LAPOP 2008 Manipulated Dataframe.Rdata')
save(df_2006,file = 'bases/rdata/LAPOP 2006 Manipulated Dataframe.Rdata')
save(df_2004,file = 'bases/rdata/LAPOP 2008 Manipulated Dataframe.Rdata')
save(df46, file = 'bases/rdata/LAPOP 2014-2016 Manipulated Dataframe.Rdata')
save(df, file = 'bases/rdata/LAPOP 2004-2019 Manipulated Dataframe.Rdata')

# ================================================= Closing Remarks =======================================================

# Finish logging ---------------------------------------------------------------------------------------------------------

sink()


