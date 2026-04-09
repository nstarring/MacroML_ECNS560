##############################################################################

# This script will clean the merged dataset, and is thusly meant to be ran once
# raw_data_procurement.R and merge_data.R have been ran, respectively.

# This script will clean according to Dr. Nick Hagerty's "Data Cleaning Checklist",
# located here: https://github.com/msu-econ-data-analytics/course-materials

# This script will mostly handle missing values and formatting, transformation will 
# occur in a seperate file
##############################################################################

##############################################################################
# Loading libraries
library(tidyverse)
library(ggplot2)
library(naniar)

#######################################
# Importing data
#######################################

merged_dirt = read.csv("data/dirty/full_monthly_merged_uncleaned.csv")


#######################################
# Structuring into tidy format
#######################################
#view(merged_dirt)

# Since each column is a variable, and each row is an observation (a monthly one)
# this data is "tidy"

#######################################
# Removing irrelevant, garbage, or empty columns and rows
# ALSO CHECKING FOR DUPLICATES
#######################################

# From viewing the data, there are no "weird" columns, with the 
# columns all having legitimate reason to remain in the analysis

# We'll also check for any repeat rows, since this returns true,
# all of our rows are distinct
print("Were all rows distinct (no repeats)?: ")
print(nrow(merged_dirt) == nrow(distinct(merged_dirt, .keep_all = T)))

#######################################
# Finding a primary key
#######################################

# A logical place to start for a primary key is the date,
# we'll evaluate its validity here

# Checking for missingness (can't be a primary key if some obs
# lack data)
print(sum(is.na(merged_dirt$date)))

# Checking for uniquenss
merged_dirt %>% 
  group_by(date) %>% 
  summarize(count = n()) %>% 
  filter(count > 1) %>% 
  sum()

# Since this returns 0, meaning none occur more than once, we can treat
# it as our pimrary key. WE'll also create a id variable that also serves
# as a primary key.

merged_dirt_id = merged_dirt %>% 
  arrange(date) %>% 
  mutate(id = row_number(), .before = date)

########################################
# Understanding definition, origin and units of each variable
########################################

# Markets
########################################

# Variable: SP500
# Definition: Index tracking the stock performance of 500 of the largest public companies in the US
# Source: Yahoo Finance
# Units: Index Value (points)

# Variable: Dow
# Definition: price-weighted index tracking 30 prominent, large-cap U.S. companies
# Source: Yahoo Finance
# Units: Index Value (points)

# Variable: NASDAQ
# Definition: A weighted index of over 2,500 stocks listed on the Nasdaq, weighted toward tech.
# Source: Yahoo Finance
# Units: Index Value (points)

# Variable: Russell2000
# Definition: An index representing the bottom 2,000 stocks in the Russell 3000
# Source: Yahoo Finance
# Units: Index Value (points)

# Variable: Gold Price
# Definition: Monthly gold price in USD per troy ounce from 1833 to present
# Source: DATAHUB, pulled from World Gold Council, compiled from Timothy Green
# Units: Price (USD per troy oz)

# Macroeconomic indicators
########################################

# Variable: 1-Year T-Bill
# Definition: Maturity interest rate on 1-year U.S. Treasury securities.
# Source: Federal Reserve
# Units: Raw Percent (not adjusted)

# Variable: 10-Year T-Bill
# Definition: Maturity interest rate on 10-year U.S. Treasury securities.
# Source: Federal Reserve
# Units: Raw Percent (not adjusted)

# Variable: CPI
# Definition: Measures average price changes for a basket of goods and services.
# Source: Federal Reserve
# Units: Index base year is 1982-1984, where it equals 100

# Variable: U3 Unemployment
# Definition: The estimated percentage of the labor force that is jobless and actively looking for work.
# Source: BLS
# Units: Seasonally adjusted percent

# Variable: Fed Funds Rate
# Definition: Interest rate at which commercial banks borrow and lend their excess r-
#              eserves to each other overnight, guides other rates.
# Source: FEderal Reserve
# Units: Raw Percent (not adjusted)

# Variable: Industrial Production
# Definition: Measures real output for all manufacturing, mining, and electric/gas industries
# Source: FEderal Reserve
# Units: Index where 2017=100

# Variable: Housing starts
# Definition: Number of new privately-owned residences on which construction has begun in that period
# Source: Census Bureau / HUD
# Units: Seasonally-adjusted thousands of units

# Variable: Case-Shiller
# Definition: Measures changes in the selling prices of single-family homes by 
#             looking at houses that have sold.
# Source: S&P Dow Jones Indices
# Units: Seasonally-adjusted index where January, 2000=100

# Variable: Real GDP
# Definition: Quarterly inflation-adjusted value of all goods and services produced in the US
# Source: BEA
# Units: Billions of Chained 2017 Dollars

# Sentiment indicators
########################################

# Variable: Consumer Sentiment
# Definition: Survey-based index measuring how optimistic consumers feel about their finances and the state of the economy.
# Source: University of Michigan
# Units: Indexed where Q1 of 1966 =100. Data turns to monthly around the late 1970s

# Variable: Wikipedia Views
# Definition: The total number of monthly page views for the English Wikipedia article titled "Recession".
# Source: Wikimedia REST API
# Units: Raw count

# Variable: Google Trends
# Definition: Google interest for the term "Recession" in the U.S. 
#             It is a relative metric scaled against the peak popularity of the term.
# Source: SerpApi and Google
# Units: Relative index (100 = maximum observed in period, 0 = lowest)


########################################
# Converting data types/"cleaning strings"
########################################
merged_dirt_typed = merged_dirt_id %>%
  mutate(
    # Forcing the date column to be a Date object
    date = as.Date(date),
    
    # Year and month to strings
    year = as.numeric(year),
    month = as.numeric(month)
  ) %>%
  # Every quantitative variable
  mutate(across(-c(date, year, month), as.numeric))

########################################
# Rename variables
########################################
merged_dirt_named = merged_dirt_typed %>%
  rename(
    # Renaming some of the more problematic names
    u3_unemployment         = unemployment,
    consumer_sentiment      = sentiment,
    industrial_production   = indpro,
    wiki_recession_views    = recession_views_wiki,
    google_recession_trends = google_trends
  )

########################################
# Missing values
########################################

# Due to the robustness of the data collection
# process for most of our data, NA values in this analysis are primarily
# from data series not being observed until certain dates

# Additionally, the variable representing UM's consumer sentiment value becomes
# monthly, not quarterly in Jan, 1979

# Given all of this, we'll begin by attempting to spot any "weird" NA values, IE
# ones that show up in the middle of a bunch of filled values.

# We'll create a new dataframe object that will tell us some dynamics of the missingness

data_coverage_summary = merged_dirt_named %>%
  # We pivot here to make it so that grouping is easier
  pivot_longer(
    cols = -c(date, year, month), 
    names_to = "raw_variable",
    values_to = "value"
  ) %>%
  group_by(raw_variable) %>%
  summarize(
    # Find the date of the first and last nonNA value
    first_obs = min(date[!is.na(value)], na.rm = TRUE),
    last_obs  = max(date[!is.na(value)], na.rm = TRUE),
    duration = as.numeric(last_obs - first_obs),
    
    # Total missing months overall
    total_nas = sum(is.na(value)),
    
    # How many missing values fall between these gaps
    internal_nas = sum(is.na(value) & date > first_obs & date < last_obs),
    
    .groups = "drop"
  ) %>%
  # Creating the nice names for the coverage plot
  mutate(
    nice_name = case_when(
      raw_variable == "dow_avg_close"           ~ "Dow Jones Avg. Close",
      raw_variable == "snp500_avg_close"        ~ "S&P 500 Avg. Close",
      raw_variable == "nasdaq_avg_close"        ~ "NASDAQ Avg. Close",
      raw_variable == "russell2000_avg_close"   ~ "Russell 2000 Avg Close",
      raw_variable == "dow_volatility"          ~ "Dow Jones Volatility",
      raw_variable == "snp500_volatility"       ~ "S&P 500 Volatility",
      raw_variable == "nasdaq_volatility"       ~ "NASDAQ Volatility",
      raw_variable == "russell2000_volatility"  ~ "Russell 2000 Volatility",
      raw_variable == "case_shiller"            ~ "Case-Shiller Index",
      raw_variable == "cpi"                     ~ "CPI",
      raw_variable == "fedfunds"                ~ "Fed Funds Rate",
      raw_variable == "gold_price"              ~ "Gold Price",
      raw_variable == "google_recession_trends" ~ "Google Trends (Recession)",
      raw_variable == "housing_starts"          ~ "Housing Starts",
      raw_variable == "industrial_production"   ~ "Industrial Production",
      raw_variable == "wiki_recession_views"    ~ "Wikipedia Views (Recession)",
      raw_variable == "consumer_sentiment"      ~ "Consumer Sentiment",
      raw_variable == "tbill_10yr"              ~ "10-Year T-Bill",
      raw_variable == "tbill_1yr"               ~ "1-Year T-Bill",
      raw_variable == "u3_unemployment"         ~ "U3 Unemployment",
      raw_variable == "u6_unemployment"         ~ "U6 Unemployment",
      raw_variable == "rgdp"                    ~ "Real GDP"
    )
  ) %>%
  arrange(duration) %>%
  # Lock the order so that ggplot graphs nicely
  mutate(nice_name = factor(nice_name, levels = unique(nice_name)))

# Examining this dataframe, all variables have no missing values between their
# first and last observation dates aside from the unemployment rates and CPI 
# (Each missing one, attributable to government shutdown in late 2025)

# The quarterly variables are missing data, which is expected. Analysis of
# those missing data suggest that this can entirely be attributable to 
# the data being quarterly

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

coverage_plot = data_coverage_summary %>%
  
  # Kill the revil NA category that caused me so much diress
  filter(!is.na(nice_name) & nice_name != "NA") %>%
  ggplot(aes(y = nice_name)) +
  
  geom_rect(
    data = nber_recessions, 
    # Overides the aes of nice_name
    inherit.aes = FALSE,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "gray", alpha = 0.6
  ) +

  # Set the font and other stylistic elements
  theme_minimal(base_size = 14, base_family = "sans") + 
  theme(
    panel.grid.major.x = element_line(color = "white", linetype = "dashed"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(), 
    
    # solid frame around the actual graph area
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
    
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(color = "#22333b", margin = margin(b = 20)),
    axis.text.y = element_text(face = "bold", color = "black")
  ) +
  
  # Timeline bars
  geom_segment(
    aes(x = as.Date(first_obs), xend = as.Date(last_obs), y = nice_name, yend = nice_name),
    color = "#162960", linewidth = 5) +
  
  # This makes custom ticks at the end of each bar
  geom_segment(
    aes(x = as.Date(first_obs), xend = as.Date(first_obs), 
        y = as.numeric(nice_name) - 0.45, yend = as.numeric(nice_name) + 0.45),
    color = "#f4b425", linewidth = 2
  ) +
  
  # Forcing exact boundaries
  scale_x_date(
    breaks = seq(as.Date("1926-01-01"), as.Date("2026-01-01"), by = "10 years"),
    date_labels = "%Y",
    limits = c(as.Date("1920-01-01"), as.Date("2026-12-31")), 
    expand = expansion(mult = c(0.01, 0.01)) 
  ) +
  
  labs(
    title = "Data Coverage",
    subtitle = "Temporal Range of Available Variables, Recessions Shaded",
    x = "Year",
    y = NULL 
  )

# Display the plot
print(coverage_plot)

# Now saving the plot in visualization folder
ggsave(
  filename = "output/visualizations/data_coverage_timeline.png", 
  plot = coverage_plot, 
  width = 14,
  height = 8,
  dpi = 300,
  # So that our tricks with the transparent axis work
  bg = "white"
)

# Given this discussion around NA values, due to the disparate nature
# of data availability, and how multiple analyses can be accomplished using
# different sets of our variables, we won't lop off any time periods for now
# and can sleep soundly knowing there is no missing NOT at random data.




########################################
# Units and Scales consistent
########################################

# All of our variables are in the same form (percentages, indicies, etc.)
# As for variables that are in dollars, converting GDP to actual dollar amounts
# would be impractical, as would listing the price of gold in billions of dollars
# since our data will be normalized regardless, we lose little by making this choice
# Something to keep in mind.

########################################
# Creating clean csv and .rdata file
########################################
write.csv(merged_dirt_named, file = "data/clean/cleaned_untransformed_data.csv")

# To create an R data file, we'll first add some attributes to each variable,
# so that the soruce and units can easily be found when looking at it

variable_attributes = list(
  snp500_avg_close        = "Source: Yahoo Finance | Units: Index Value (points) | Def: Index tracking 500 largest public companies in the US",
  dow_avg_close           = "Source: Yahoo Finance | Units: Index Value (points) | Def: Price-weighted index tracking 30 prominent large-cap US companies",
  nasdaq_avg_close        = "Source: Yahoo Finance | Units: Index Value (points) | Def: Weighted index of over 2,500 stocks listed on the Nasdaq, tech-weighted",
  russell2000_avg_close   = "Source: Yahoo Finance | Units: Index Value (points) | Def: Index representing the bottom 2,000 stocks in the Russell 3000",
  gold_price              = "Source: World Gold Council | Units: Price (USD per troy oz) | Def: Monthly gold price",
  tbill_1yr               = "Source: Federal Reserve | Units: Raw Percent (not adjusted) | Def: Maturity interest rate on 1-year US Treasury securities",
  tbill_10yr              = "Source: Federal Reserve | Units: Raw Percent (not adjusted) | Def: Maturity interest rate on 10-year US Treasury securities",
  cpi                     = "Source: Federal Reserve | Units: Index (1982-1984=100) | Def: Measures average price changes for a basket of goods and services",
  u3_unemployment         = "Source: BLS | Units: Seasonally adjusted percent | Def: Percentage of the labor force jobless and actively looking for work",
  fedfunds                = "Source: Federal Reserve | Units: Raw Percent (not adjusted) | Def: Interest rate at which commercial banks borrow/lend overnight",
  industrial_production   = "Source: Federal Reserve | Units: Index (2017=100) | Def: Real output for manufacturing, mining, and electric/gas industries",
  housing_starts          = "Source: Census Bureau/HUD | Units: Seasonally-adjusted thousands | Def: New privately-owned residences on which construction has begun",
  case_shiller            = "Source: S&P Dow Jones | Units: Seasonally-adjusted index (Jan 2000=100) | Def: Changes in selling prices of single-family homes",
  rgdp                    = "Source: BEA | Units: Billions of Chained 2017 Dollars | Def: Quarterly inflation-adjusted value of all goods and services produced in the US",
  consumer_sentiment      = "Source: Univ. of Michigan | Units: Indexed (Q1 1966=100) | Def: Survey-based index measuring consumer optimism",
  wiki_recession_views    = "Source: Wikimedia API | Units: Raw count | Def: Monthly page views for the English Wikipedia article titled 'Recession'",
  google_recession_trends = "Source: Google Trends | Units: Relative index (0-100) | Def: Google search interest for the term 'Recession' in the US"
)

# Chaning the name of the dataframe we're packaging
cleaned_untransformed_data = merged_dirt_named

# Now embedding these attributes for each column

#loopingn through the names
for (name in names(variable_attributes)) {
  # Checking to see if it is caught in our list
  # (Just in case)
  if(name %in% names(cleaned_untransformed_data)){
    # Altering description
    attr(cleaned_untransformed_data[[name]], "label") = variable_attributes[[name]]
  }
}

# Saving as an .rdata
save(cleaned_untransformed_data, file = "data/clean/cleaned_untransformed_data.RData")
