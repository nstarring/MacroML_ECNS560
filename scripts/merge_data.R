################################################################################
# This script serves multiple, related purposes for our analysis
# It will merge the disparate datasets we've so far collected depending on
# their unit of observation (monthly, daily, etc.) this also mostly coincides with the
# sources of the data

# A large monthly dataset will then be created, which will then be ran through 
# the "Data Cleaning Checklist"(Hagerty, 2024), in the "cleaning" script

# Required libraries
################################################################################
library(naniar)
library(tidyverse)
library(lubridate)
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
  temp = read_csv(fpath, show_col_types = FALSE)
  # Creating a different one for each
  assign(name, temp)
}

# Now we'll check each date column to see if the dfs are joinable
# Since they mostly come from the same source/have had some preformating, we
# onyl have to change the name of the date column for the monthyl gold date
# and use lubridate to fix the date for 

# Renaming, I recognize that this isn't ideal practice, but since the chagnes
# are so minor, and are mostly cosmetic in nature, it isn't that problematic
gold_price_monthly = gold_price_monthly %>% 
  rename(date = Date) %>% 
  mutate(date = ym(date))
  
  
# Using lubridate to edit date
google_trends_monthly = google_trends_monthly %>%
  mutate(date = my(date))



##############################
# Merging monthly data
# We'll use purr here, to make it so that more
# monthyl datasets can be added later as long as they follow the
# format of 2 columns, one date, and the other as the measure
##############################

# Snagging names of all dataframes from the monthly_files
df_names = str_remove(basename(monthly_files), "\\.csv$")

# Using mget() to grab the actual dfs
all_monthly_dfs = mget(df_names)

for(name in names(all_monthly_dfs)){
  
  clean_prefix = str_remove(name, "_monthly$")
  
  temp_df = as.data.frame(all_monthly_dfs[[name]])
  
  # If it has 3 columns, delete the first one
  # This is for FRED data that has the symbol
  # column
  if(ncol(temp_df) == 3){
    temp_df = temp_df[, -1] 
  }
  
  # Gets the name of the dataframe as then name
  # of that variable in the dataframe
  data_col = setdiff(names(temp_df), "date")
  names(temp_df)[names(temp_df) == data_col] = clean_prefix
  
  # Save it back to the list
  all_monthly_dfs[[name]] = temp_df
}

# Now using purr to join them by date
monthly_merged = all_monthly_dfs %>%
  purrr::reduce(full_join, by = "date") %>%
  mutate(
    year = year(date),
    month = month(date)
  ) %>% 
  arrange(date, year, month) %>% 
  filter(year > 1920)
  

#################
# Daily to monthly
# We won't do the "add ability" component
# here since our analysis is focussing on
# monthyl observations
##################

daily_files = list.files(path = "data/raw/",
  pattern = "daily.*\\.csv",
  full.names = TRUE)

# Now we'll loop through each, examine if the date column is in our desired
# format, and save each of the files. This will save us time

for(fpath in daily_files){
  # Grabbing the name of the variable
  name = str_remove(basename(fpath), "\\.csv$")
  # Now actually reading the dataframe
  temp = read_csv(fpath, show_col_types = FALSE)
  # Creating a different one for each
  assign(name, temp)
}


# We'll first bind them all together since they have the same variables
# and use the symbol to id the different markets
daily_markets_stack = bind_rows(dow_daily, nasdaq_daily, russell2000_daily,
  snp500_daily)

# Now we'll convert to monthly
monthly_market_data = daily_markets_stack %>%
  mutate(
    # Force the date to the 1st of the month so it matches other merged set
    date = floor_date(date, "month") 
  ) %>%
  # Group by each unique date in each market
  group_by(symbol, date) %>%
  summarize(
    avg_close = mean(close, na.rm = TRUE), 
    volatility = var(close, na.rm = TRUE),
    .groups = "drop"
  )

# Now pivoting wider to be ready to merge
monthly_market_wide = monthly_market_data %>%
  # Renaming the symbols to be better
  mutate(
    symbol = case_when(
      symbol == "^GSPC" ~ "snp500",
      symbol == "^DJI"  ~ "dow",
      symbol == "^IXIC" ~ "nasdaq",
      symbol == "^RUT"  ~ "russell2000"
    )
  ) %>%
  # Pivotting wider
  pivot_wider(
    names_from = symbol,
    values_from = c(avg_close, volatility),
    # This function helps in labelling the columns properly
    names_glue = "{symbol}_{.value}"
  ) %>%
  arrange(date)

#########################
# Handling quarterly GDP
#########################
rgdp_quarterly = read_csv("data/raw/rgdp_quarterly.csv", show_col_types = FALSE) %>% 
  rename(rgdp = price) %>% 
  select(date, rgdp)

########################
# THE FULL JOIN
########################
full_merged = monthly_market_wide %>% 
  full_join(monthly_merged, by = "date") %>% 
  full_join(rgdp_quarterly, by = "date") %>% 
  relocate(year, .after = date) %>% 
  relocate(month, .after = year)

# Sending the data to the data folder
write_csv(full_merged, paste0("data/dirty/full_monthly_merged_uncleaned.csv"))
