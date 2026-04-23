##############################################################################
#
# This script contains helper functions for, and two main sets of functions
# that compute persistence barcodes for windows of the data, and the
# corresponding wassterstein metric between periods
#
# The workflow consists of two stages:
# 1. Compute persistence diagrams over time windows (overlapping or not)
# 2. Compute Wasserstein distances between consecutive windows' diagrams
#
# This script is only meant to be ran after alter_data.R so that the 
# "transformed_cleaned_data.csv / .RData" can be used
#
#
##############################################################################

#################################
# Libraries required
##################################
library(tidyverse)
# For ripsDiag and wasserstein
# ripsDiag actually
library(TDA)

# For parallel computing 
library(furrr)
library(future)

# Progress bars that work with furrr
library(progressr)
#################################

#################################
#
# Helper functions
#
#################################

##############################################################################
# Helper: subset data to shared coverage period, capped at 2024-12-01
##############################################################################

# This helper is used by both window-building functions. It takes the master 
# dataframe and the vector of variables the user wants to analyze, then finds
# the range of dates where ALL of those variables have non-missing values.

# It also caps it at the end of 2024 to avoid weird data gaps due to the govern
# ment shutdown

# . format is typical for helper functions

.get_shared_coverage_data = function(data, variables) {
  # Ensuring date is a Date object
  data = data %>% mutate(date = as.Date(date))
  
  # Subsetting to just the variables of interest plus date
  subset_data = data %>% select(date, all_of(variables))
  
  # Finding the rows where all variables of interest are non-missing
  complete_rows = subset_data %>%
    
    # if_all only evaluates to be true if each row of the subset contains
    # non-NA values
    # Filters by the rows that have data filled in.
    filter(if_all(all_of(variables), ~ !is.na(.x)))
  
  # Finding the first and last dates of complete coverage
  first_date = min(complete_rows$date)
  last_date  = min(max(complete_rows$date), as.Date("2024-12-01"))
  
  # Subsetting to this range (this keeps only the contiguous shared range)
  shared = subset_data %>% 
    filter(date >= first_date & date <= last_date) %>%
    arrange(date)
  
  # A final check for any remaining missingness, returning an error if so
  if(any(is.na(shared %>% select(all_of(variables))))){
    stop("There are still missing values in the shared coverage range.")
  }
  
  return(shared)
}
