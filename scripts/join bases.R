# ==================================== LAPOP ECUADOR DATA MANIPULATION ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# May 2021
# Script for the joining of dataframes for each year for Ecuador in a big, merged file

# ================================================= Preliminaries =============================================================

# No working directory specification needed since an Rproject is being used
# Note that, to avoid overwriting of the base joining, one must run the FULL script everytime.
# In order to get the latest data, run both the join databases and the data manipulation scripts

# Load libraries --------------------------------------------------------------------------------------------------------------

library(haven) # For stata reading
library(tidyverse) # Ggplot, deployer, tibble
library(arsenal) # Compare datasets
library(openxlsx) # Import and export XL files
library(broom) # Tidy

# Load dataframes -------------------------------------------------------------------------------------------------------------

# These are the stata dataframes with no value labels for each year

df_2019<-read_dta('bases/dta/ecu2019.dta')
df_2016<-read_dta('bases/dta/ecu2016.dta')
df_2014<-read_dta('bases/dta/ecu2014.dta')
df_2012<-read_dta('bases/dta/ecu2012.dta')
df_2010<-read_dta('bases/dta/ecu2010.dta')
df_2008<-read_dta('bases/dta/ecu2008.dta')
df_2006<-read_dta('bases/dta/ecu2006.dta')
df_2004<-read_dta('bases/dta/ecu2004.dta')

# These are csv dataframes, exported from Stata. These do have value labels, might be necessary for some variables

dfc_2019<-read_csv('bases/csv/df2019.csv', show_col_types = F)
dfc_2016<-read_csv('bases/csv/df2016.csv', show_col_types = F)
dfc_2014<-read_csv('bases/csv/df2014.csv', show_col_types = F)
dfc_2012<-read_csv('bases/csv/df2012.csv', show_col_types = F)
dfc_2010<-read_csv('bases/csv/df2010.csv', show_col_types = F)
dfc_2008<-read_csv('bases/csv/df2008.csv', show_col_types = F)
dfc_2006<-read_csv('bases/csv/df2006.csv', show_col_types = F)
dfc_2004<-read_csv('bases/csv/df2004.csv', show_col_types = F)

# Below the full, merged dataframe as found in the LAPOP website, loaded as an R object from the other project

load('bases/rdata/LAPOP 2004-2019 Merged.Rdata') 

# No need to assign for the load command, it comes with the df_all name 

# Start logging ----------------------------------------------------------------------------------------------------------

# Start logging all output of the console in a text file

sink('logs/join bases.txt')

# ================================================= Starting with 2019 ====================================================

# I will start with the 2019 base and go backwards

df<-df_2019

#  Year -----------------------------------------------------------------------------------------------------------------------

# I need a year variable to signal CLEARLY the year the survey was responded

df$year<-2019

# Relocate it to the beginning

# Reorder to see year

df<-relocate(df,year, .after=idnum)

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight1500 variable according to LAPOP's advice

df$weight1500<- (df$wt * 1500)/(nrow(df_2019))

# Political Parties ------------------------------------------------------------------------------------------------------

# Create the political party label, with clear text values so that it is easier to track support across time

df$party<-ifelse(df$vb11 == 913,  'PAIS',
                 ifelse(df$vb11 == 901, 'CREO', 
                        ifelse(df$vb11 == 903, 'PSC',
                               ifelse(df$vb11 == 915, 'ID', 
                                      ifelse(df$vb11 == 907, 'PK', 'Others'))))) %>% as.factor

# Variables for the exposure to corruption -------------------------------------------------------------------------------

# Later I will need to be able to difference from different kinds of NAs for these variables, namely, difference from 
# "Not applicable" of the usual "NA"

# Thus, for every year I will replace these variables from their csv counterparts, which do have applied labels.

df$exc11<-dfc_2019$exc11 %>% as.factor()
df$exc13<-dfc_2019$exc13 %>% as.factor()
df$exc14<-dfc_2019$exc14 %>% as.factor()
df$exc15<-dfc_2019$exc15 %>% as.factor()
df$exc16<-dfc_2019$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df$corr_mun<-ifelse(df$exc11 == 'Yes', 1,
                    ifelse(df$exc11 == 'Not Applicable'| df$exc11 == 'No', 0, NA))

df$corr_work<-ifelse(df$exc13 == 'Yes', 1,
                     ifelse(df$exc13== 'Not Applicable'| df$exc13 == 'No', 0, NA))

df$corr_court<-ifelse(df$exc14 == 'Yes', 1,
                      ifelse(df$exc14== 'Not Applicable'| df$exc14 == 'No', 0, NA))

df$corr_health<-ifelse(df$exc15 == 'Yes', 1,
                      ifelse(df$exc15== 'Not Applicable'| df$exc15 == 'No', 0, NA))

df$corr_school<-ifelse(df$exc16 == 'Yes', 1,
                       ifelse(df$exc16== 'Not Applicable'| df$exc16 == 'No', 0, NA))

# ================================================= Appending 2016 ============================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2016$year<-2016

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2016$weight1500<- (df_2016$wt * 1500)/(nrow(df_2016))

# Political Sympathy -----------------------------------------------------------------------------------------------------

# Create the political sympathy variable in 2016, where numbers for parties are possibly different

df_2016$party<-ifelse(df_2016$vb11 == 913,  'PAIS',
                 ifelse(df_2016$vb11 == 901, 'CREO', 
                        ifelse(df_2016$vb11 == 903, 'PSC', 'Others'))) %>% as.factor()


# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2016$exc11<-dfc_2016$exc11 %>% as.factor()
df_2016$exc13<-dfc_2016$exc13 %>% as.factor()
df_2016$exc14<-dfc_2016$exc14 %>% as.factor()
df_2016$exc15<-dfc_2016$exc15 %>% as.factor()
df_2016$exc16<-dfc_2016$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2016$corr_mun<-ifelse(df_2016$exc11 == 'Yes', 1,
                    ifelse(df_2016$exc11 == 'Not Applicable'| df_2016$exc11 == 'No', 0, NA))

df_2016$corr_work<-ifelse(df_2016$exc13 == 'Yes', 1,
                     ifelse(df_2016$exc13== 'Not Applicable'| df_2016$exc13 == 'No', 0, NA))

df_2016$corr_court<-ifelse(df_2016$exc14 == 'Yes', 1,
                      ifelse(df_2016$exc14== 'Not Applicable'| df_2016$exc14 == 'No', 0, NA))

df_2016$corr_health<-ifelse(df_2016$exc15 == 'Yes', 1,
                       ifelse(df_2016$exc15== 'Not Applicable'| df_2016$exc15 == 'No', 0, NA))

df_2016$corr_school<-ifelse(df_2016$exc16 == 'Yes', 1,
                       ifelse(df_2016$exc16== 'Not Applicable'| df_2016$exc16 == 'No', 0, NA))

# Comparison table  ------------------------------------------------------------------

# Create a comparison table between the two df's to see what columns will generate NA values for each year

# compare_20192016<-comparedf(df_2019,df_2016) %>% summary()

# Export to XL

# write.xlsx(compare_20192016['vars.ns.table'],'comparisons/compare 2016-2019.xlsx')

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2016 observations to the 2019 base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2016)

# ================================================= Appending 2014 ============================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2014$year<-2014

# Change the data format of the ID column to character, so it is compatible with the bases of the following years

df$idnum<-df$idnum %>% as.character()

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2014$weight1500<- (df_2014$wt * 1500)/(nrow(df_2014))

# Political Sympathy --------------------------------------------------------------------------------------------------------------

# Create the political sympathy variable in 2014, where numbers for parties are possibly different

df_2014$party<-ifelse(df_2014$vb11 == 913,  'PAIS',
                      ifelse(df_2014$vb11 == 901, 'CREO', 
                             ifelse(df_2014$vb11 == 903, 'PSC',
                                    ifelse(df_2014$vb11 == 907, 'PK' ,'Others')))) %>% as.factor()
# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2014$exc11<-dfc_2014$exc11 %>% as.factor()
df_2014$exc13<-dfc_2014$exc13 %>% as.factor()
df_2014$exc14<-dfc_2014$exc14 %>% as.factor()
df_2014$exc15<-dfc_2014$exc15 %>% as.factor()
df_2014$exc16<-dfc_2014$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2014$corr_mun<-ifelse(df_2014$exc11 == 'Yes', 1,
                         ifelse(df_2014$exc11 == 'Not Applicable'| df_2014$exc11 == 'No', 0, NA))

df_2014$corr_work<-ifelse(df_2014$exc13 == 'Yes', 1,
                          ifelse(df_2014$exc13== 'Not Applicable'| df_2014$exc13 == 'No', 0, NA))

df_2014$corr_court<-ifelse(df_2014$exc14 == 'Yes', 1,
                           ifelse(df_2014$exc14== 'Not Applicable'| df_2014$exc14 == 'No', 0, NA))

df_2014$corr_health<-ifelse(df_2014$exc15 == 'Yes', 1,
                            ifelse(df_2014$exc15== 'Not Applicable'| df_2014$exc15 == 'No', 0, NA))

df_2014$corr_school<-ifelse(df_2014$exc16 == 'Yes', 1,
                            ifelse(df_2014$exc16== 'Not Applicable'| df_2014$exc16 == 'No', 0, NA))

# Comparison table  ----------------------------------------------------------------------------------------------------------

# Create a comparison table between the two df's to see what columns will generate NA values for each year

# compare_df2014<-comparedf(df,df_2014) %>% summary()

# Export to XL

# write.xlsx(compare_df2014['vars.ns.table'],'comparisons/compare 2014 vs 2019-16.xlsx')

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2014 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2014)

# ================================================= Appending 2012 ============================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2012$year<-2012

# Change the idnum variable to character in the 2012 base

df_2012$idnum<-df_2012$idnum %>% as.character()

# Eliminate dates as format is conflicting with the other dataframes, fix later if needed

df_2012$fecha<-NA

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2012$weight1500<- (df_2012$wt * 1500)/(nrow(df_2012))

# Political Sympathy -----------------------------------------------------------------------------------------------------

# Create the political sympathy variable in 2012, where numbers for parties are possibly different

df_2012$party<-ifelse(df_2012$vb11 == 913,  'PAIS',
                      ifelse(df_2012$vb11 == 904, 'ID', 
                             ifelse(df_2012$vb11 == 903, 'PSC',
                                    ifelse(df_2012$vb11 == 907, 'PK' ,'Others')))) %>% as.factor()

# Note that there is no CREO movement in this year, only for 2014, 2016 and 2019. Movement founded January 20th, 2012.

# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2012$exc11<-dfc_2012$exc11 %>% as.factor()
df_2012$exc13<-dfc_2012$exc13 %>% as.factor()
df_2012$exc14<-dfc_2012$exc14 %>% as.factor()
df_2012$exc15<-dfc_2012$exc15 %>% as.factor()
df_2012$exc16<-dfc_2012$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2012$corr_mun<-ifelse(df_2012$exc11 == 'Si', 1,
                         ifelse(df_2012$exc11 == 'N/A'| df_2012$exc11 == 'No', 0, NA))

df_2012$corr_work<-ifelse(df_2012$exc13 == 'Si', 1,
                          ifelse(df_2012$exc13== 'N/A'| df_2012$exc13 == 'No', 0, NA))

df_2012$corr_court<-ifelse(df_2012$exc14 == 'Yes', 1,
                           ifelse(df_2012$exc14== 'N/A'| df_2012$exc14 == 'No', 0, NA))

df_2012$corr_health<-ifelse(df_2012$exc15 == 'Yes', 1,
                            ifelse(df_2012$exc15== 'N/A'| df_2012$exc15 == 'No', 0, NA))

df_2012$corr_school<-ifelse(df_2012$exc16 == 'Yes', 1,
                            ifelse(df_2012$exc16== 'N/A'| df_2012$exc16 == 'No', 0, NA))

# Comparison table  ----------------------------------------------------------------------------------------------------------

# Create a comparison table between the two df's to see what columns will generate NA values for each year

# compare_df2012<-comparedf(df,df_2012) %>% summary()

# Export to XL

# write.xlsx(compare_df2012['vars.ns.table'],'comparisons/compare with 2012.xlsx')

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2012 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2012)

# ================================================= Appending 2010 ============================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2010$year<-2010

# Change the idnum variable to character in the 2012 base

df_2010$idnum<-df_2010$idnum %>% as.character()

# Eliminate dates as format is conflicting with the other dataframes, fix later if needed

df_2010$fecha<-NA

# Political Sympathy -----------------------------------------------------------------------------------------------------

# Create the political sympathy variable in 2010, where numbers for parties are possibly different

df_2010$party<-ifelse(df_2010$vb11 == 913,  'PAIS',
                      ifelse(df_2010$vb11 == 904, 'ID', 
                             ifelse(df_2010$vb11 == 903, 'PSC',
                                    ifelse(df_2010$vb11 == 907, 'PK' ,'Others')))) %>% as.factor()
# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2010$weight1500<- (df_2010$wt * 1500)/(nrow(df_2010))

# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2010$exc11<-dfc_2010$exc11 %>% as.factor()
df_2010$exc13<-dfc_2010$exc13 %>% as.factor()
df_2010$exc14<-dfc_2010$exc14 %>% as.factor()
df_2010$exc15<-dfc_2010$exc15 %>% as.factor()
df_2010$exc16<-dfc_2010$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2010$corr_mun<-ifelse(df_2010$exc11 == 'Sí', 1,
                         ifelse(df_2010$exc11 == 'N/A'| df_2010$exc11 == 'No', 0, NA))

df_2010$corr_work<-ifelse(df_2010$exc13 == 'Sí', 1,
                          ifelse(df_2010$exc13== 'N/A'| df_2010$exc13 == 'No', 0, NA))

df_2010$corr_court<-ifelse(df_2010$exc14 == 'Sí', 1,
                           ifelse(df_2010$exc14== 'N/A'| df_2010$exc14 == 'No', 0, NA))

df_2010$corr_health<-ifelse(df_2010$exc15 == 'Sí', 1,
                            ifelse(df_2010$exc15== 'N/A'| df_2010$exc15 == 'No', 0, NA))

df_2010$corr_school<-ifelse(df_2010$exc16 == 'Sí', 1,
                            ifelse(df_2010$exc16== 'N/A'| df_2010$exc16 == 'No', 0, NA))

# Comparison table  ----------------------------------------------------------------------------------------------------------

# Create a comparison table between the two df's to see what columns will generate NA values for each year

# compare_df2010<-comparedf(df,df_2010) %>% summary()

# Export to XL

# write.xlsx(compare_df2010['vars.ns.table'],'comparisons/compare with 2010.xlsx')

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2012 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2010)

# ================================================= Appending 2008 ============================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2008$year<-2008

# Change the idnum variable to character in the 2012 base

df_2008$idnum<-df_2008$idnum %>% as.character()

# Eliminate dates as format is conflicting with the other dataframes, fix later if needed

df_2008$fecha<-NA

# Political Sympathy -----------------------------------------------------------------------------------------------------

# Create the political sympathy variable in 2008, where numbers for parties are possibly different

df_2008$party<-ifelse(df_2008$vb11 == 913,  'PAIS',
                      ifelse(df_2008$vb11 == 904, 'ID', 
                             ifelse(df_2008$vb11 == 903, 'PSC',
                                    ifelse(df_2008$vb11 == 907, 'PK' ,'Others')))) %>% as.factor()

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2008$weight1500<- (df_2008$wt * 1500)/(nrow(df_2008))

# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2008$exc11<-dfc_2008$exc11 %>% as.factor()
df_2008$exc13<-dfc_2008$exc13 %>% as.factor()
df_2008$exc14<-dfc_2008$exc14 %>% as.factor()
df_2008$exc15<-dfc_2008$exc15 %>% as.factor()
df_2008$exc16<-dfc_2008$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2008$corr_mun<-ifelse(df_2008$exc11 == 'Sí', 1,
                         ifelse(df_2008$exc11 == 'Not Applicable'| df_2008$exc11 == 'No', 0, NA))

df_2008$corr_work<-ifelse(df_2008$exc13 == 'Sí', 1,
                          ifelse(df_2008$exc13== 'Not Applicable'| df_2008$exc13 == 'No', 0, NA))

df_2008$corr_court<-ifelse(df_2008$exc14 == 'Sí', 1,
                           ifelse(df_2008$exc14== 'Not Applicable'| df_2008$exc14 == 'No', 0, NA))

df_2008$corr_health<-ifelse(df_2008$exc15 == 'Sí', 1,
                            ifelse(df_2008$exc15== 'Not Applicable'| df_2008$exc15 == 'No', 0, NA))

df_2008$corr_school<-ifelse(df_2008$exc16 == 'Sí', 1,
                            ifelse(df_2008$exc16== 'Not Applicable'| df_2008$exc16 == 'No', 0, NA))

# Comparison table  ----------------------------------------------------------------------------------------------------------

# Create a comparison table between the two df's to see what columns will generate NA values for each year

# compare_df2008<-comparedf(df,df_2008) %>% summary()

# Export to XL

# write.xlsx(compare_df2008['vars.ns.table'],'comparisons/compare with 2008.xlsx')

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2012 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2008)

# ================================================= Appending 2006 ===========================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2006$year<-2006

# Eliminate dates as format is conflicting with the other dataframes, fix later if needed

df_2006$fecha<-NA

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2006$weight1500<- (df_2006$wt * 1500)/(nrow(df_2006))

# Sampling design --------------------------------------------------------------------------------------------------------

# Change the name of the strata variable for it to be consequent with other years

df_2006<-rename(df_2006, 'estratopri'= estrato)

# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2006$exc11<-dfc_2006$exc11 %>% as.factor()
df_2006$exc13<-dfc_2006$exc13 %>% as.factor()
df_2006$exc14<-dfc_2006$exc14 %>% as.factor()
df_2006$exc15<-dfc_2006$exc15 %>% as.factor()
df_2006$exc16<-dfc_2006$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2006$corr_mun<-ifelse(df_2006$exc11 == 'Sí', 1,
                         ifelse(df_2006$exc11 == 'Not Applicable'| df_2006$exc11 == 'No', 0, NA))

df_2006$corr_work<-ifelse(df_2006$exc13 == 'Sí', 1,
                          ifelse(df_2006$exc13== 'Not Applicable'| df_2006$exc13 == 'No', 0, NA))

df_2006$corr_court<-ifelse(df_2006$exc14 == 'Si', 1,
                           ifelse(df_2006$exc14== 'Not Applicable'| df_2006$exc14 == 'No', 0, NA))

df_2006$corr_health<-ifelse(df_2006$exc15 == 'Si', 1,
                            ifelse(df_2006$exc15== 'Not Applicable'| df_2006$exc15 == 'No', 0, NA))

df_2006$corr_school<-ifelse(df_2006$exc16 == 'Si', 1,
                            ifelse(df_2006$exc16== 'N/A'| df_2006$exc16 == 'No', 0, NA))

# Comparison table  ----------------------------------------------------------------------------------------------------------

# Create a comparison table between the two df's to see what columns will generate NA values for each year

# compare_df2006<-comparedf(df,df_2006) %>% summary()

# Export to XL

# write.xlsx(compare_df2006['vars.ns.table'],'comparisons/compare with 2006.xlsx')

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2006 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2006)

# ================================================= Appending 2004 ===========================================================

# We will add the 2004 variable from the merged dataframe, it has all observations and includes the UPM, which ain't present
# in the sole 2004 dataframe.

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Subset the Ecuador 2004 entries from the merged dataframe

df_2004_ec<-subset(df_all, pais == 9 & year == 2004)

# Remove empty columns in this df
emptycols<- sapply(df_2004_ec, function (k) all(is.na(k)))
df_2004_ec<- df_2004_ec[!emptycols]

# Change the id variable so it does not give out a problem 

df_2004_ec$idnum<-NA

# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2004_ec$exc11<-dfc_2004$exc11 %>% as.factor()
df_2004_ec$exc13<-dfc_2004$exc13 %>% as.factor()
df_2004_ec$exc14<-dfc_2004$exc14 %>% as.factor()
df_2004_ec$exc15<-dfc_2004$exc15 %>% as.factor()
df_2004_ec$exc16<-dfc_2004$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2004_ec$corr_mun<-ifelse(df_2004_ec$exc11 == 'Yes', 1,
                         ifelse(df_2004_ec$exc11 == 'Not Applicable'| df_2004_ec$exc11 == 'No', 0, NA)) 

df_2004_ec$corr_work<-ifelse(df_2004_ec$exc13 == 'Yes', 1,
                          ifelse(df_2004_ec$exc13== 'Not Applicable'| df_2004_ec$exc13 == 'No', 0, NA))

df_2004_ec$corr_court<-ifelse(df_2004_ec$exc14 == 'Yes', 1,
                           ifelse(df_2004_ec$exc14== 'Not Applicable'| df_2004_ec$exc14 == 'No', 0, NA))

df_2004_ec$corr_health<-ifelse(df_2004_ec$exc15 == 'Yes', 1,
                            ifelse(df_2004_ec$exc15== 'Not Applicable'| df_2004_ec$exc15 == 'No', 0, NA))

df_2004_ec$corr_school<-ifelse(df_2004_ec$exc16 == 'Yes', 1,
                            ifelse(df_2004_ec$exc16== 'Not Applicable'| df_2004_ec$exc16 == 'No', 0, NA))
# Comparison table  ----------------------------------------------------------------------------------------------------------

# Create a comparison table between the two df's to see what columns will generate NA values for each year

# compare_df2004<-comparedf(df,df_2004) %>% summary()

# Export to XL

# write.xlsx(compare_df2004['vars.ns.table'],'comparisons/compare with 2004.xlsx')

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2004 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2004_ec)

# ================================================ 2014 and 2016 base ========================================================

# Create a base with only 2014 and 2016

df46<-subset(df, df$year == 2014 | df$year == 2016)

#================================================== Closing remarks ===================================================================

# Verifiy the dataframe is compiled correctly ----------------------------------------------------------------------------

df$year %>% summary()
nrow(df)
ncol(df)

# Stop logging -----------------------------------------------------------------------------------------------------------

sink()

