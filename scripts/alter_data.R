#############################################################################
# This script alters the cleaned data so that it does the following:
#
#   - Uses percent change/log returns for certain variables, namely market based ones
#   - Uses first differences to do the same for percentage variables
#   - Normalize the data so that it can be used in Persistent Homology
#############################################################################

# Libraries
library(tidyverse)

# REading data

ua_data = read.csv("data/clean/cleaned_untransformed_data.csv")

altered_data = ua_data %>% 
  # Ensuring that datat is sorted by time for the lag() calls
  arrange(date) %>% 
  # Creating log_return versions of applicable variables
  mutate(
    across(
      # Grabbing the variables to be log returned
      .cols = c(
        snp500_avg_close, dow_avg_close, nasdaq_avg_close, russell2000_avg_close,
        gold_price, case_shiller, cpi, industrial_production, rgdp
      ),
      # Formula to be applied
      .fns = ~log(.x / lag(.x)),
      .names = "{.col}_logret"
    
    # Now doing the same for percentage variables, with differencing
    ),
    across(
      .cols = c(
        u3_unemployment, u6_unemployment, fedfunds, tbill_1yr, tbill_10yr,
        
        # These were difficult not to alter to be percentage changes
        # UM themselves report changes in index points, not pct
        # For the other sentiment indicators, massive spikes could lead
        # to drastically large percentage changes
        
        
        consumer_sentiment, google_recession_trends, wiki_recession_views,
        dow_volatility, snp500_volatility, nasdaq_volatility, russell2000_volatility,
        # Since this variable is mean reverting, and not growing over time
        # (deceptively so)
        housing_starts
      ),
      .fns = ~ .x - lag(.x),
      .names = "{.col}_diff"
    )) %>% 
    
  # Now standardizing across the new columns
  mutate(
    across(
      .cols = ends_with(c("_logret", "_diff")),
      .fns = ~ as.numeric(scale(.x)),
      .names = "{.col}_scaled"
    ),
    # To analyze levels (higher violatility periods, etc.)
    dow_volatility_scaled = scale(dow_volatility),
    snp500_volatility_scaled = scale(snp500_volatility),
    nasdaq_volatility_scaled = scale(nasdaq_volatility),
    russell2000_volatility_scaled = scale(russell2000_volatility)
  ) %>% 
  # No idea how this 'X' var got added
  select(-X)


# Saving the new data
write.csv(altered_data, file = "data/clean/transformed_cleaned_data.csv")
# Saving as an .rdata
save(altered_data, file = "data/clean/transformed_cleaned_data.RData")
