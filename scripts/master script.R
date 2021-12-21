# ==================================== LAPOP ECUADOR Corruption Tolerance Study ===============================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# May 2021
# Master script 

# Preliminaries ----------------------------------------------------------------------------------------------------------------

rm(list = ls()) # Delete everything that has been loaded before

# Call scripts -----------------------------------------------------------------------------------------------------------

# Call all the scripts which do the data wrangling. These are the necessary ones to produce the datasets needed for the
# replication of the paper in the Rnw files. 

source('scripts/join bases.R') # The join bases for the creation of the preliminary base

source('scripts/data manipulation.R') # Data manipulation for new variables and others

# End --------------------------------------------------------------------------------------------------------------------

