####################################################
#
# The purpose of this file is to conduct exploratory data analysis (EDA),
# on not the raw variables themselves, but to analyze the results of
# running the PH-wasserstein distance pipeline to analyze the viability
# of our machine learning predictions later on
#
####################################################

###################
# Loading required libraries
# For our custom PH functions and visualization function
# Setting the wd for the script so it can run the PH functions script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("PH_functions.R")
# For interactive, 3d plots (sorry ggplot2)
library(tidyverse)
library(plotly)



# We start by creating a couple of different specifications of variables,
# Since we are preparing these vars for PH, they are already normalized
# and expressed as rates of change rather than in levels


# This set of variables has been tracked even before the recession
# they are also a collectoin of asset markets, market violatility,
# faith in markets (gold as an alternate asset) and a signal of the
# "real" economy.

# We can finally know that only if the bankers of the day had access
# to our function, then the depression could have been predicted,
# and perhaps avoided (only joking)
pre_depression_vars = c("gold_price_logret_scaled", "industrial_production_logret_scaled",
  "snp500_avg_close_logret_scaled", "snp500_volatility_scaled")


# These are the variables that the federal reserve cares about the most
# (at least per the dual mandate and their policy levers)
# This is what we were testing in the other EDA script
monetary_movers = c("u3_unemployment_diff_scaled", "fedfunds_diff_scaled", 
  "cpi_logret_scaled")

# A collection of variables intended to be measures of what is going on
# in Main Street, not wall street
real_economy = c("consumer_sentiment_imputed_diff_scaled", "industrial_production_logret_scaled",
  "u3_unemployment_diff_scaled", "case_shiller_logret_scaled", "housing_starts_diff_scaled")

# Since the inverted yield curve is thought to be a fairly good predictor
# of recessions, we'll have that here
yield_curve = c("tbill_10yr_diff_scaled", "tbill_1yr_diff_scaled", "fedfunds_diff_scaled")


# Attempting to get an idea of how all of these variables relate to one another
mano_del_dios = c(
  "snp500_avg_close_logret_scaled", "nasdaq_avg_close_logret_scaled", "russell2000_avg_close_logret_scaled",
  "snp500_volatility_scaled",
  "fedfunds_diff_scaled", "tbill_1yr_diff_scaled", "tbill_10yr_diff_scaled",
  "u3_unemployment_diff_scaled", "u6_unemployment_diff_scaled",
  "industrial_production_logret_scaled", "cpi_logret_scaled",
  "consumer_sentiment_imputed_diff_scaled"
)

# Utlizing new measures of consumer fear, (their interest in recessions),
# we create a set of variables that seeks to gauge panic both in the market
# and among consumers. u6 unemplotyment also gauges the discouraged workers
# a signal of pessimism

widespread_panic = c("google_recession_trends_diff_scaled",  "snp500_volatility_scaled",
  "u6_unemployment_diff_scaled")

# To get a better idea of what these points actually look like in space, we'll use
# plotly to create an interactive plot where we can see point clouds move over time
# while we will be using both overlapping and non-overlapping windows for points,
# we'll use seperate windows
