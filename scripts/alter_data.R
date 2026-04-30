#############################################################################
# This script alters the cleaned data so that it does the following:
#
#   - Uses percent change/log returns for certain variables, namely market based ones
#   - Uses first differences to do the same for percentage variables
#   - Normalize the data so that it can be used in Persistent Homology
#   - Imputes values for 
# This script also appends recession dates, pulled from the NBER 
# (this data is simply copy and pasted due to the relative simplicity)
#############################################################################

# Libraries
library(tidyverse)
# For imputing consumer sentiment data
library(zoo)

# REading data
nber_recessions = data.frame(
  start = as.Date(c("1929-08-01", "1937-05-01", "1945-02-01", "1948-11-01", 
    "1953-07-01", "1957-08-01", "1960-04-01", "1969-12-01", 
    "1973-11-01", "1980-01-01", "1981-07-01", "1990-07-01", 
    "2001-03-01", "2007-12-01", "2020-02-01")),
  end   = as.Date(c("1933-03-01", "1938-06-01", "1945-10-01", "1949-10-01", 
    "1954-05-01", "1958-04-01", "1961-02-01", "1970-11-01", 
    "1975-03-01", "1980-07-01", "1982-11-01", "1991-03-01", 
    "2001-11-01", "2009-06-01", "2020-04-01"))
)

ua_data = read.csv("data/clean/cleaned_untransformed_data.csv")

altered_data = ua_data %>% 
  # Ensuring that datat is sorted by time for the lag() calls
  arrange(date) %>% 
  
  # Using na.approx for linear interpolation between quarterly gaps. Since before
  # 1978, consumer sentiment was reported quarterly
  mutate(
    consumer_sentiment_imputed = na.approx(consumer_sentiment, na.rm = FALSE)
  ) %>%
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
    
    # Now doing the same for percentage and other variables, with differencing
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
  # Adding the recessions
  rowwise() %>%
  mutate(
    recession = ifelse(any(date >= nber_recessions$start & date <= nber_recessions$end), 1, 0),
  ) %>%
  ungroup() %>%
  
  # Adding the 6-month and 12-month forward-looking indicator variables
  # While also ensuring that 
  mutate(
    # We also add a variable indicating if a recession happens in the next 6 months
    recession_win_6months = ifelse(
      # These form a stacked "or" condition where if a recession is within the next 6 obs,
      # we note it
      lead(recession, 1) == 1 | 
        lead(recession, 2) == 1 | 
        lead(recession, 3) == 1 | 
        lead(recession, 4) == 1 | 
        lead(recession, 5) == 1 | 
        lead(recession, 6) == 1, 
      1, 0
    )
  ) %>%
  # No idea how this 'X' var got added
  select(-X)


# Saving the new data
write.csv(altered_data, file = "data/clean/transformed_cleaned_data.csv")
# Saving as an .rdata
save(altered_data, file = "data/clean/transformed_cleaned_data.RData")
