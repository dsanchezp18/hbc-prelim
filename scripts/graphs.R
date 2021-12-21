# ==================================== LAPOP ECUADOR DATA MANIPULATION ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# May 2021
# Script for graphs

# ================================================= Preliminaries =============================================================

# No working directory needed as an Rproject is being used
# No need for importing a dataframe as it uses the one from the join database, and joint calc script, 
# Must run those scripts before to be able to run these

# Load libraries

library(tidyverse) # Ggplot, deployer, tibble
library(broom) # Tidy

# ================================================= Corruption Tolerance ======================================================

# Graphs corresponding to the answers made in the exc18 question

# Scatter plots ------------------------------------------------------------------------------------------------------------

# Replicate the line graph in the 2019 report

# First transform the info to a dataframe to be able to apply it using ggplot

cor_tol_year_df<-as.data.frame(cor_tol_year)

# Select only "yes" responses (==1) and change names

cor_tol_year_df<-subset(cor_tol_year_df, cor_tol_year_df$Var2==1) %>% select('Var1', 'Freq') %>% rename('Year'='Var1')

# Change to 100%

cor_tol_year_df$Freq<-cor_tol_year_df$Freq*100

# Now graph the line

png(filename='figures/Corruption Tolerance Scatter Graph.png', width=1000, height=680)

time_corrtol<-ggplot(cor_tol_year_df, aes(x=Year, y=Freq, group=1))+
              geom_ribbon(aes(ymin=Freq-me_year$error_marg,
                              ymax=Freq+me_year$error_marg),
                          alpha=0.2)+
              geom_point(size=2)+
              geom_text(aes(label=round(Freq, 2)),vjust=3, hjust=-0.01, position=position_dodge(0.9))+
              theme_bw()+
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks = element_blank())+
              labs(y='', x='Year of Survey', title='Percent who Justify Bribery in Ecuador, by Year')
time_corrtol

dev.off()

# Make the same graph as before but now separating by economic situation

# 1. Worse Economic Situation than a year ago 

# First transform the info from calculations to a dataframe to be able to apply it using ggplot

cor_tol_year_dfworse<-as.data.frame(cor_tol_year_worse)

# Select only "yes" responses (==1) and change names

cor_tol_year_dfworse<-subset(cor_tol_year_dfworse, cor_tol_year_dfworse$Var2==1) %>% 
                      select('Var1', 'Freq') %>% rename('Year'='Var1')

# Change to 100%

cor_tol_year_dfworse$Freq<-cor_tol_year_dfworse$Freq*100

# Now graph the line

png(filename='figures/Corruption Tolerance Scatter Graph, Worse Ec Sit.png', width=800, height=550)

time_corrtol_Worse<-ggplot(cor_tol_year_dfworse, aes(x=Year, y=Freq, group=1))+
                    geom_point(size=2)+
                    geom_text(aes(label=round(Freq, 2)),vjust=1, hjust=-0.01, position=position_dodge(0.9))+
                    theme_bw()+
                    theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks = element_blank())+
                    labs(y='', x='Year of Survey', title='Percent who Justify Bribery in Ecuador, by Year', 
                         subtitle='Respondents with worse economic situations in a given year')
time_corrtol_Worse

dev.off()

# Better or Same

# First transform the info to a dataframe to be able to apply it using ggplot

cor_tol_year_dfbetters<-as.data.frame(cor_tol_year_betters)

# Select only "yes" responses (==1) and change names

cor_tol_year_dfbetters<-subset(cor_tol_year_dfbetters, cor_tol_year_dfbetters$Var2==1) %>% 
  select('Var1', 'Freq') %>% rename('Year'='Var1')

# Change to 100%

cor_tol_year_dfbetters$Freq<-cor_tol_year_dfbetters$Freq*100

# Now graph the line

png(filename='figures/Corruption Tolerance Scatter Graph, Better or Same Ec Sit.png', width=800, height=550)

time_corrtol_betters<-ggplot(cor_tol_year_dfbetters, aes(x=Year, y=Freq, group=1))+
  geom_point(size=2)+
  geom_text(aes(label=round(Freq, 2)),vjust=1, hjust=-0.01, position=position_dodge(0.9))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(y='', x='Year of Survey', title='Percent who Justify Bribery in Ecuador, by Year', 
       subtitle='Respondents with better or the same economic situations in a given year')
time_corrtol_betters

dev.off()

# Now the same two graphs, but for the income situation variable

# Worse

# First transform the info to a dataframe to be able to apply it using ggplot

cor_tol_year_dfiworse<-as.data.frame(cor_tol_year_iworse)

# Select only "yes" responses (==1) and change names

cor_tol_year_dfiworse<-subset(cor_tol_year_dfiworse, cor_tol_year_dfiworse$Var2==1) %>% 
  select('Var1', 'Freq') %>% rename('Year'='Var1')

# Change to 100%

cor_tol_year_dfiworse$Freq<-cor_tol_year_dfiworse$Freq*100

# Now graph the line

png(filename='figures/Corruption Tolerance Scatter Graph, Less Income Sit.png', width=800, height=550)

time_corrtol_incworse<-ggplot(cor_tol_year_dfiworse, aes(x=Year, y=Freq, group=1))+
  geom_point(size=2)+
  geom_text(aes(label=round(Freq, 2)),vjust=1, hjust=-0.01, position=position_dodge(0.9))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(y='', x='Year of Survey', title='Percent who Justify Bribery in Ecuador, by Year', 
       subtitle='Respondents with lesser income compared to last year')
time_corrtol_incworse

dev.off()

# Increased or stayed the same

# First transform the info to a dataframe to be able to apply it using ggplot

cor_tol_year_dfisameb<-as.data.frame(cor_tol_year_ibetters)

# Select only "yes" responses (==1) and change names

cor_tol_year_dfisameb<-subset(cor_tol_year_dfisameb, cor_tol_year_dfisameb$Var2==1) %>% 
  select('Var1', 'Freq') %>% rename('Year'='Var1')

# Change to 100%

cor_tol_year_dfisameb$Freq<-cor_tol_year_dfisameb$Freq*100

# Now graph the line

png(filename='figures/Corruption Tolerance Scatter Graph, Same or More Income Sit.png', width=800, height=550)

time_corrtol_incsameb<-ggplot(cor_tol_year_dfisameb, aes(x=Year, y=Freq, group=1))+
  geom_point(size=2)+
  geom_text(aes(label=round(Freq, 2)),vjust=1, hjust=-0.01, position=position_dodge(0.9))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(y='', x='Year of Survey', title='Percent who Justify Bribery in Ecuador, by Year', 
       subtitle='Respondents with more or the same income compared to last year')
time_corrtol_incsameb

dev.off()

# ============================================ Economic Variable Graphs =====================================================

# Scatter Plots ----------------------------------------------------------------------------------------------------------

# Graph the change in economic situations, year by year

# First turn into a dataframe for ggplot

ec_sit_year_df<-as.data.frame(ec_sit_year)

# Select only "Worse" responses (==1) and change names

ec_sit_year_df<-subset(ec_sit_year_df, ec_sit_year_df$Var2=='Worse') %>% 
  select('Var1', 'Freq') %>% rename('Year'='Var1')

# Multiply to get percentage

ec_sit_year_df$Freq<- ec_sit_year_df$Freq*100

# Now graph the line (time series)

png(filename='figures/Percent People who report worse econ sit.png', width=800, height=550)

ec_sit_year_sp<-ggplot(ec_sit_year_df, aes(x=Year, y=Freq, group=1))+
  geom_point(size=2)+
  geom_text(aes(label=round(Freq, 2)),vjust=1, hjust=-0.01, position=position_dodge(0.9))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(y='', x='Year of Survey', title='Percent who report a worse economic situation relative to last year')
ec_sit_year_sp

dev.off()

# =============================================== Labor Market Graphs ========================================================

# Scatter Plots ----------------------------------------------------------------------------------------------------------

# Graph the % of unemployed people over the years

# First turn the tabulation to a dataframe for ggplot

une

# Select only unemployed responses (==1) and change names

unem_year_df<-subset(unem_year_df, unem_year_df$Var2=='Unemployed') %>% 
  select('Var1', 'Freq') %>% rename('Year'='Var1')

# Show in percentage

unem_year_df$Freq<- unem_year_df$Freq*100

# Same with the other base

unem2_year_df<-as.data.frame(unem2_year)

# Select only unemployed responses (==1) and change names

unem2_year_df<-subset(unem2_year_df, unem2_year_df$Var2=='Unemployed') %>% 
  select('Var1', 'Freq') %>% rename('Year'='Var1')

# Show in percentage

unem2_year_df$Freq<-unem2_year_df$Freq*100

# Now the same with employment

em_year_df<-as.data.frame(em_year)

# Select only unemployed responses (==1) and change names

em_year_df<-subset(em_year_df, em_year_df$Var2=='Employed') %>% 
  select('Var1', 'Freq') %>% rename('Year'='Var1')

# Show in percentage

em_year_df$Freq<- em_year_df$Freq*100

# Graph complete unemployment 

png(filename='figures/Percent People who report unemployment complete sit.png', width=800, height=550)

unem2_year_sp<-ggplot(unem2_year_df, aes(x=Year, y=Freq, group=1))+
  geom_point(size=2)+
  geom_text(aes(label=round(Freq, 2)),vjust=1, hjust=-0.01, position=position_dodge(0.9))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())+
  labs(y='', x='Year of Survey', title='Percent who report being unemployed', 
       subtitle='Includes those looking and not looking for jobs anymore')
unem2_year_sp

dev.off()
