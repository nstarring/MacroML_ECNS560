################################################################################
# Due to the nature of this project, all data needed is available via R packages
# The purpose of this script is to import data, and send it to the "raw" subfolder
#
# Files are intentionally labelled with their unit of observation
#
# Each series will be fully utilized, though thieir coverage isn't universal,
# their range in the time they cover will be addressed later in the analysis.

# Due to the API call and key required later in this script, one should source 
# a serpai key and save it as an environmental variable.
# https://serpapi.com
# After obtaining your key, input this into the console,
# usethis::edit_r_environ()
# and then create a new sysytem, variable SERPAPI_KEY

# All variables aside from the price of gold over time come from this script
################################################################################


# Required libraries
################################################################################

# For fetching the data
library(tidyquant)
# For saving the data
library(readr)

# For API call
library(httr)
library(jsonlite)
library(tidyverse)
################################################################################

# We first check and ensure that the user has the directory structure, and create
# one if they lack one

file_location = "data/raw/"

# Checking if it exists and making a new one if not

if(!dir.exists(file_location)){dir.create(file_location, recursive = T)}




################################################################################
# First importing market and commodity data
# These will be observed on a daily basis, and give the following:
# open, high, low and closing prices, volume, and adjusted closing price,
# which accounts for things like dividend payments at the time of observation
################################################################################

# SP 500
snp_daily = tq_get("^GSPC", get = "stock.prices", from = "1900-01-01", to = Sys.Date())
write_csv(snp_daily, paste0(file_location, "snp500_daily.csv"))

# Dow
dow_daily = tq_get("^DJI", get = "stock.prices", from = "1900-01-01", to = Sys.Date())
write_csv(dow_daily, paste0(file_location, "dow_daily.csv"))

# NASDAQ
nasdaq_daily = tq_get("^IXIC", get = "stock.prices", from = "1900-01-01", to = Sys.Date())
write_csv(nasdaq_daily, paste0(file_location, "nasdaq_daily.csv"))

# Russell 2000
russell_daily = tq_get("^RUT", get = "stock.prices", from = "1900-01-01", to = Sys.Date())
write_csv(russell_daily, paste0(file_location, "russell2000_daily.csv"))

################################################################################
# Now for the macroeconomic data from FRED
################################################################################
# 1-Year Tbill
tbill_1yr_monthly = tq_get("GS1", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(tbill_1yr_monthly, paste0(file_location, "tbill_1yr_monthly.csv"))

# 10-Year Tbill
tbill_10yr_monthly = tq_get("GS10", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(tbill_10yr_monthly, paste0(file_location, "tbill_10yr_monthly.csv"))

# CPI
cpi_monthly = tq_get("CPIAUCSL", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(cpi_monthly, paste0(file_location, "cpi_monthly.csv"))

# Unemployment Rate (Monthly) U3
unrate_monthly = tq_get("UNRATE", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(unrate_monthly, paste0(file_location, "unemployment_monthly.csv"))

# Other unemployment measure
u6_unemployment_monthly = tq_get("U6RATE", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(u6_unemployment_monthly, paste0(file_location, "u6_unemployment_monthly.csv"))

# Fed funds rate
fedfunds_monthly = tq_get("FEDFUNDS", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(fedfunds_monthly, paste0(file_location, "fedfunds_monthly.csv"))

# Industrial Production on a monthy basis, useful as gdp proxy 
indprod_monthly = tq_get("INDPRO", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(indprod_monthly, paste0(file_location, "indpro_monthly.csv"))

# Housing Starts
housing_starts_monthly = tq_get("HOUST", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(housing_starts_monthly, paste0(file_location, "housing_starts_monthly.csv"))

# Case-Shiller index for national housing value (uses sold housing data)
cshousing_monthly = tq_get("CSUSHPISA", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(cshousing_monthly, paste0(file_location, "case_shiller_monthly.csv"))

# Consumer Sentiment
sentiment_monthly = tq_get("UMCSENT", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(sentiment_monthly, paste0(file_location, "sentiment_monthly.csv"))

# RGDP (observed quarterly)
rgdp_quarterly = tq_get("GDPC1", get = "economic.data", from = "1900-01-01", to = Sys.Date())
write_csv(rgdp_quarterly, paste0(file_location, "rgdp_quarterly.csv"))

################################################################################
# For another measure of consumer sentiment, and "worriness" around recession,
# wikipedia and google trends are also used
# USING API CALLS (We're challenging ourselves)
################################################################################

# Wikipedia
# Doesn't require a key, API is public and open
wiki_call = GET(
  paste0(
    "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
    "en.wikipedia/all-access/user/Recession/monthly/2015070100/",
    # This will grab data from most recent release, needed to format the date to work
    format(Sys.Date(), "%Y%m%d00")),
  # This argument indicates to the API who is accessing
  user_agent("ECNS560_student")
)

# Parsing and extracting

recession_views_monthly = fromJSON(rawToChar(wiki_call$content))$items %>% 
  select(timestamp, views) %>%
  # Doing quick formatting since the timestamp column is messy
  mutate(date = as.Date(paste0(substr(timestamp, 1, 4), "-", substr(timestamp, 5, 6), "-01"))) %>%
  select(date, views)

write_csv(recession_views_monthly, paste0(file_location, "recession_views_wiki_monthly.csv"))

# Google trends data
# Values represent a relative index that goes from 0-100, with higher values
# indicating a level of searches closer to the highest observed, lower values
# representing the opposite

google_call = GET(modify_url(
  "https://serpapi.com/search.json", 
  query = list(
    engine = "google_trends",
    q = "Recession",
    geo = "US",
    data_type = "TIMESERIES",
    date = "all",
    api_key = Sys.getenv("SERPAPI_KEY")
)))

# Saving the actual content of that call
google_content = fromJSON(rawToChar(google_call$content))

# This API is very tricky, and has periods where it simply doesnt work
# with that said, we'll provide some protection against the most common
# error that is ran into when running this script
# THIS MAY BE CHEATING, but it will just rely on the already present csv pulled
# the last succesful call
if (!is.null(google_content$error)) {
  
  message("SerpApi Error Encountered: ", google_content$error)
  message("Will use existing local CSV.")
  
} else {
  
  # We want to grab the nested data of the interest over time
  google_trends_monthly = google_content$interest_over_time$timeline_data %>% 
    # The date values are easy enough to grab, while the values are each given
    # their own dataframes
    select(date, values) %>% 
    mutate(
      # This uses an inline funciton to grab the first value of the dataframe,
      # our value of interest
      trend = sapply(values, function(value) value$extracted_value[1])
    ) %>% 
    select(date, trend)
  
  write_csv(google_trends_monthly, paste0(file_location, "google_trends_monthly.csv"))
  message("Google Trends data obtained succesfully")
}