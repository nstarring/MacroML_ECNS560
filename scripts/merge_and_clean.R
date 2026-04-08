################################################################################
# This script serves multiple, related purposes for our analysis
# It will first merge the disparate datasets we've so far collected depending on
# their unit of observation (monthly, daily, etc.) this also mostly coincides with the
# sources of the data
# Following this, the data for the daily observed variables will then be inspec
# ted for missingness, which will guide our following effort to convert the daily
# measures into monthly ones
# 
# A large monthly dataset will then be created, which will then be ran through 
# the "Data Cleaning Checklist"(Hagerty, 2024) 

# Required libraries
################################################################################
library(naniar)
library(tidyverse)
################################################################################

################################################################################
# Importing data and full merging
################################################################################

#################
# Monthyl
##################

# To save time typing (and to make this scalable with other monthyl datasets)
# Note that this just grabs the paths
monthly_files = list.files(path = "data/raw/",
  # Finds all with monthyl and that end with csv
  pattern = "monthly.*\\.csv",
  full.names = TRUE)

# Now we'll loop through each, examine if the date column is in our desired
# format, and save each of the files. This will save us time

for(fpath in monthly_files){
  # Grabbing the name of the variable
  name = str_remove(basename(fpath), "\\.csv$")
  
  # Now actually reading the dataframe
  
}