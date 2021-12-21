# ==================================== LAPOP ECUADOR DATA MANIPULATION ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# May 2021
# Script for calculations

# ================================================= Preliminaries =============================================================

# No working directory needed as an Rproject is being used
# No need for importing a dataframe as it uses the one from the "join databases" script, must run that script before

# Libraries used in the script -------------------------------------------------------------------------------------------

library(tidyverse) # Ggplot, dplyr, tibble
library(broom) # Tidy
library(psych) # Summary stats by group and others

# Error Margins ----------------------------------------------------------------------------------------------------------------

# Enter the error margins defined by the sign 

me_year<-data.frame(year=c(seq(2004,2016,2),2019), error_marg=c(1.8,1.8,1.79,1.79,2.52,2.50,2.5,2.5))

# ================================================= Corruption Tolerance ======================================================

# Make computations on the ctol answer, or the tolerance to making bribes

# Percentage of responses, justify or not, by year ----------------------------------------------------------------------------------

cor_tol_year<-table(df$year, df$ctol) %>% prop.table(margin=1)
cor_tol_year

# Results are consistent with the graph made on the report spotlight, except with some years
# Years that are not the exact same because some years require a sample rebalancing which I do not do here
# Margin 1 calculates the proportions within rows

# Bribe justification by economic situation -----------------------------------------------------------------------------------------------------------------------

# Subset the dataframe for people who answered "Worse"

df_ec_worse<-subset(df, df$econ_sit=='Worse')

# Now calculate the table for this dataset, same as before

cor_tol_year_worse<-table(df_ec_worse$year, df_ec_worse$ctol) %>% prop.table(margin=1)
cor_tol_year_worse

# Subset the dataframe for people who answered "Same or Better"

df_ec_betters<-subset(df, df$econ_sit=='Same or Better')

# Now calculate the table for this dataset, same as before

cor_tol_year_betters<-table(df_ec_betters$year, df_ec_betters$ctol) %>% prop.table(margin=1)
cor_tol_year_betters

# Note that for most years people who have reported a worse economic situation are somewhat more tolerant to bribes

# Bribe justification by income situation ---------------------------------------------------------------------------------

# Subset dataframe by the two situation categories

# Income decreased 

df_inc_worse<-subset(df, df$inc_sit=='Decreased')

# Now calculate the table for this dataset, same as before

cor_tol_year_iworse<-table(df_inc_worse$year, df_inc_worse$ctol) %>% prop.table(margin=1)
cor_tol_year_iworse

# Income increased or constant

df_inc_betters<-subset(df, df$inc_sit=='Same or Increased')

# Now calculate the table for this dataset, same as before

cor_tol_year_ibetters<-table(df_inc_betters$year, df_inc_betters$ctol) %>% prop.table(margin=1)
cor_tol_year_ibetters


# Breaking down corruption tolerance -------------------------------------------------------------------------------------

# Break down the corruption tolerance answers by groups

# First, create a dataframe with only "Yes" answers to the corruption tolerance variable

df_ctol<-subset(df, df$ctol == 1)

# Now, tabulate by year and % of unemployed people (This is the percentage of people who tolerate bribes who are unemployed)

ctol_unem2_year<-table(df_ctol$year, df_ctol$unem2_4a) %>% prop.table(margin = 1)
ctol_unem2_year

# Tabulate by year and % people who tolerate corruption which are right wing (center is left)

ctol_pwing_year<-table(df_ctol$year, df_ctol$pol_wing2) %>% prop.table(margin = 1)
ctol_pwing_year

# Tabulate by year and % that report worse economic situation

ctol_ecsit_year<-table(df_ctol$year, df_ctol$econ_sit) %>% prop.table(margin = 1)
ctol_ecsit_year

# Tabulate by year and % that report some interest in politics

ctol_pint_year<-table(df_ctol$year, df_ctol$pint_dic) %>% prop.table(margin = 1)
ctol_pint_year

# Same as the previous but with the actual groups of political interest

ctol_polint_year<-table(df_ctol$year, df_ctol$pol_int) %>% prop.table(margin = 1)
ctol_polint_year

# Tabulate by year and labor market status

ctol_lstatus_year<-table(df_ctol$year, df_ctol$ocup4a) %>% prop.table(margin =1)
ctol_lstatus_year

# Tabulate by year and protest participation

ctol_prot_year<-table(df_ctol$year, df_ctol$prot3) %>% prop.table(margin =1)
ctol_prot_year

# Tabulate by year and by news exposure

ctol_news_year<-table(df_ctol$year, df_ctol$news_exp1) %>% prop.table(margin =1)
ctol_news_year

# Tabulate by year and confidence in the president

ctol_presconf_year<-table(df_ctol$year, df_ctol$pres_conf) %>% prop.table(margin = 1)
ctol_presconf_year

# ================================================ Other corruption indicators ==============================================


# Corruption Perceptions -------------------------------------------------------------------------------------------------

# Tabulate the exc7 question through time  

corr_perc_year<- table(df$year, df$exc7) %>% prop.table(margin = 1)
corr_perc_year

# Tabulate the combined corruption tolerance variable to see changes in perception

corr_percd_year<- table(df$year, df$corrper) %>% prop.table(margin = 1)
corr_percd_year

# ================================================= Economic/ Inc Situation ======================================================

# Economic situation question idio2 --------------------------------------------------------------------------------------

# Make a table summarizing how many people answer if their economic situation has been worse 

ec_sit_year<-table(df$year, df$econ_sit) %>% prop.table(margin=1)
ec_sit_year

# Same as before but for income situation: has your income decreased throughout the years

inc_sit_year<-table(df$year, df$inc_sit) %>% prop.table(margin=1)
inc_sit_year

# Economic and Income Situation by Ocupation Status ----------------------------------------------------------------------

# Summarize, for a given year, the percentage of people who report having a worse economic situation

# We only analyze 2016 for now, and for the 7 categories in the labor market

ec_sit_labor<-table(df_2016$ocup4a, df_2016$econ_sit) %>% prop.table(margin=1)

# Change the row names so it makes sense 

labormkt_names<-c('Working', 'Has Job', 'Unemployed, looking', 'Student', 'Home', 'Retired', 'Doesn\'t Work' )

row.names(ec_sit_labor)<-labormkt_names
ec_sit_labor

# Same but for the income question

inc_sit_labor<-table(df_2016$ocup4a, df_2016$inc_sit) %>% prop.table(margin=1)

# Change the row names so it makes sense 

row.names(inc_sit_labor)<-labormkt_names
inc_sit_labor

# Similar one but for the other labor market levels made by me

ec_sit_labor1<-table(df_2016$work_2a, df_2016$econ_sit) %>% prop.table(margin=1)
ec_sit_labor1

# Now for income

inc_sit_labor1<-table(df_2016$work_2a, df_2016$inc_sit) %>% prop.table(margin=1)
inc_sit_labor1

# Economic/Income Situation by Labor Market Hierarchy ----------------------------------------------------------------------------------

# The same as before, however now consider the ocup1a question which signals the kind of work that individuals do. 

ec_sit_olevel<-table(df_2016$ocup1a, df_2016$econ_sit) %>% prop.table(margin=1)

# Change row names so it makes more sense

ocplevel_names<-c('Public', 'Private', 'Owner/Partner', 'Self','Unpaid')
row.names(ec_sit_olevel)<-ocplevel_names
ec_sit_olevel

# Same with income 

inc_sit_olevel<-table(df_2016$ocup1a, df_2016$inc_sit) %>% prop.table(margin=1)

# Change row names so it makes more sense

row.names(inc_sit_olevel)<-ocplevel_names
inc_sit_olevel

# ================================================= Demographic and others=============================================================

# Urban-Rural ------------------------------------------------------------------------------------------------------------

# Compute percentages of urb rural by year

urb_year<-table(df$year, df$ur) %>% prop.table(margin=1)
urb_year

# Education --------------------------------------------------------------------------------------------------------------

# Compute education year averages per year

year_educ<- describeBy(df$ed, df$year )
year_educ


# Age --------------------------------------------------------------------------------------------------------------------

# Compute year averages and median per years

year_age<-describeBy(df$age, df$year)
year_age

# Race -------------------------------------------------------------------------------------------------------------------

etid_year<-table(df$year, df$etid) %>% prop.table(margin = 1)
etid_year
# News -------------------------------------------------------------------------------------------------------------------

# Tabulate by year

news_exp_year<-table(df$year, df$news_exp1) %>% prop.table(margin = 1)
news_exp_year

# Cross Tabulate with labor market status

news_exp_labor<-table(df$year, df$ocup4a, df$news_exp1) %>% prop.table(margin = 1)
news_exp_labor

# ================================================= Job Market  =============================================================

# Economically Active Population -----------------------------------------------------------------------------------------

# Track the EAP percentage through time

eap_year<- table(df$year, df$eap) %>% prop.table (margin = 1)
eap_year

# Unemployment -----------------------------------------------------------------------------------------------------------

# Calculate a time series of % of unemployed people, who are looking for a job 

unem_year<-table(df$year, df$unem_4a) %>% prop.table(margin=1)
unem_year

# Calculate the same thing but for unemployed who are and are not looking for a job 

unem2_year<-table(df$year, df$unem2_4a) %>% prop.table(margin=1)
unem2_year

# Subset a base with only unemployed workers

df_unem<-subset(df, df$unem_4a == 'Unemployed')

# Now calculate the % of unemployed people that answer the corruption tolerance question as "Yes"

table(df_unem$year, df_unem$ctol) %>% prop.table(margin=1)

# Labor Market Status ----------------------------------------------------------------------------------------------------

# Same with the ocup4a question

ocup4a_year<-table(df$year, df$ocup4a) %>% prop.table(margin = 1)
ocup4a_year

# Employment -------------------------------------------------------------------------------------------------------------

# Same as the other ones

em_year<-table(df$year, df$em_4a) %>% prop.table(margin=1)
em_year

# Labor Market Hierarchy Time Series -------------------------------------------------------------------------------------

# Do the same but with the ocup1a question

lmh_year<-table(df$year, df$ocup_sec) %>% prop.table(margin = 1) 
lmh_year

# ================================================= Political ================================================================

# Calculations with political preferences of the population

# Interest in Politics ---------------------------------------------------------------------------------------------------

# Tabulate the interest in politics question by year and see the time series

pol_int_time<-table(df$year, df$pol_int) %>% prop.table(margin=1)
pol_int_time

# Do the same but with the dummy variable I created before

pol_int2_time<-table(df$year, df$pint_dic) %>% prop.table(margin=1)
pol_int2_time



# Sympathy with Political Party ------------------------------------------------------------------------------------------

# Tabulate the question that asks whether ANY party is object of sympathy of the respondent

pol_simp1_time<-table(df$year, df$pol_symp) %>% prop.table(margin = 1)
pol_simp1_time

# Tabulate the created variable of sympathy with different parties

pol_party_time<-table(df$year, df$party) %>% prop.table(margin = 1)
pol_party_time

# Dislike political parties ----------------------------------------------------------------------------------------------

# Tabulate whether or not you dislike a political party

table(df$vb10neg) %>% prop.table()

# For the disliking of political parties, according to each party

pol_dis_time<-table(df$pol_dis) %>% prop.table()
pol_dis_time

# Overall Political Identification ---------------------------------------------------------------------------------------

# Tabulate, over the years, the political leanings of the people (left vs. right), all variables

pol_wing_time<-table(df$year, df$pol_wing) %>% prop.table( margin = 1)
pol_wing_time

pol_wing_time2<-table(df$year, df$pol_wing2) %>% prop.table( margin = 1)
pol_wing_time2

pol_group_time<-table(df$year, df$pol_group) %>% prop.table( margin = 1)
pol_group_time

# The same but with all scores

pol_score_time<- table(df$year, df$polscore) %>% prop.table(margin = 1)
pol_score_time

# Participation in Protests ----------------------------------------------------------------------------------------------
# Tabulate participation in protests by year

prot_year<-table(df$year, df$prot3) %>% prop.table(margin = 1)
prot_year 

# Confidence in President ------------------------------------------------------------------------------------------------

pres_year<-table(df$year, df$pres_conf) %>% prop.table(margin = 1)
pres_year

