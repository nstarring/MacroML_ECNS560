##############################################################################
#
# This script is to conduct exploratory data analysis (EDA) on the dataset
# that has been cleaned and modified in other scripts
#
# This script DOES NOT save ALL visualizations. Look at the "visualization.R' file
# to find visualizations that are saved in the output directory.
#
##############################################################################

#################################
# Libraries required
library(tidyverse)
library(ggplot2)
library(dlookr)
library(skimr)
library(dygraphs)
library(xts)
#################################

# To gain a foundational understanding of our data, we'll first skim it
full_data = read.csv("data/clean/transformed_cleaned_data.csv")
skim(full_data)


########################################################################
# Introductory exploration of U3 and Federal Funds Rate
########################################################################
#
# Our exploratory analyis will focus on the "haevy hitters" of macroeconomic indicators
# Namely, we'll focus on the federal funds rate and the U3 unemployment rate

data = read.csv("data/clean/transformed_cleaned_data.csv") %>% 
  select(date, year, month, rgdp, fedfunds, u3_unemployment,
    fedfunds_diff, u3_unemployment_diff, recession, recession_win_6months
    ) %>% 
  filter(year > 1950)

# We'll first get an idea of the statistics behind each variable
summary(data %>% select(-date))

# From this, in adherence with the prompt, we'll look at unconditional
# distributions for our variables of interest
#
# Since raw time series data would be sort of confusing to visualize using
# one, we'll look at the differences

# Plotting unconditional distributions of the differences
# This graph isn't the prettiest, but serves a purpose to
# breifely visualize the data
u3_dist = ggplot(data, aes(x = u3_unemployment_diff)) +
  geom_histogram(binwidth = 0.1, fill = "#2F3061", color = "white") +
  theme_minimal() +
  labs(title = "Unconditional Distribution: Monthly change in U3 ", 
    subtitle = "Notice the massive right-tail outliers (Extreme layoffs)",
    x = "Month-over-Month Change (In Percentage Points)", y = "Count")

print(u3_dist)
# The distribution appears semi-normal without taking into account the outliers
# WE can see very apparent outliers here, we'll use a box and whisker
# to see the outliers

boxplot(data$u3_unemployment_diff)

# From this, we have quite a few notable outliers, we'll keep that in mind


# Unconditional dist for fed funds rate
ff_dist = ggplot(data, aes(x = fedfunds_diff)) +
  geom_histogram(binwidth = 0.1, fill = "#2F3061", color = "white") +
  theme_minimal() +
  labs(title = "Unconditional Distribution: Monthly change in Federal Funds Rate", 
    subtitle = "Notice the outliers in both directions",
    x = "Month-over-Month Change (In Percentage Points)", y = "Count")

print(ff_dist)

# This is distributed similarly, though there are notable outliers, now in both
# directions
boxplot(data$fedfunds_diff)

# The -~6 outlier seems off. We'll take a closer look in the outliers section
range(data$fedfunds_diff, na.rm=T)



#######################################################################
# Conditional distributions for fed funds rate
########################################################################

# Now that we have a fundamental understnding of what these rate variables look
# like, we'll now focus on the levels of variables given different circumstances
# (conditional distributions)

dist_fed_regime = ggplot(data %>% drop_na(recession), 
  aes(x = fedfunds, 
  fill = factor(recession, levels = c(0, 1), labels = c("Expansion", "Recession")))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("No recession" = "#7D4600", "Recession" = "#A4B0F5")) +
  theme_minimal() +
  labs(title = "Distribution: Fed Funds Rate by Regime",
    x = "Federal Funds Rate", y = "Density", fill = "Economic State") +
  theme(legend.position = "bottom")

print(dist_fed_regime)

# From this visualization, it seems like the federal funds rate is more
# consistent in periods without a recession, and has more variability 
# during a recession, we'll do the same for the rate version of this var
dist_fed_rate_regime = ggplot(data %>% drop_na(recession), 
  aes(x = fedfunds_diff, 
    fill = factor(recession, levels = c(0, 1), labels = c("Expansion", "Recession")))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("No recession" = "#7D4600", "Recession" = "#A4B0F5")) +
  theme_minimal() +
  labs(title = "Distribution: Fed Funds Rate Monthly Change by Regime",
    x = "Federal Funds Rate", y = "Density", fill = "Economic State") +
  theme(legend.position = "bottom")

print(dist_fed_regime)

# Now we'll look a box and whiskers to examine outliers
box_fed_diff = ggplot(data %>% drop_na(recession, fedfunds_diff), 
  aes(x = factor(recession, levels = c(0, 1), labels = c("Expansion", "Recession")), 
    y = fedfunds_diff, 
    fill = factor(recession, levels = c(0, 1), labels = c("Expansion", "Recession")))) +
  
  # The boxplot geom
  geom_boxplot(alpha = 0.8) +
  
  # Colors 
  scale_fill_manual(values = c("Expansion" = "#7D4600", "Recession" = "#A4B0F5")) +
  
  
  theme_minimal() +
  labs(title = "Distribution: Fed Funds Rate Shifts by Regime",
    x = "Economic State", 
    y = "Month-over-Month Change")+
  # The legend looks awful, we'll lose it for now.
  theme(legend.position = "none",
    plot.title = element_text(face = "bold"))

print(box_fed_diff)

# From these visuals, we now know that the changes center negatively for recession,
# compared to a regular period
# The changes during a recession indicatee greater variability in that variable
# under that context.
# Additionally, our large outlier occurs during a recession





#######################################################################
# Conditional distributions for U3 rate
########################################################################
dist_u3_regime = ggplot(data %>% drop_na(recession, u3_unemployment), 
  aes(x = u3_unemployment, 
    fill = factor(recession, levels = c(0, 1), labels = c("Expansion", "Recession")))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Expansion" = "#7D4600", "Recession" = "#A4B0F5")) +
  theme_minimal() +
  labs(title = "Distribution: U3 Unemployment by Regime",
    x = "U3 Unemployment Rate (%)", y = "Density", fill = "Economic State") +
  theme(legend.position = "bottom")

print(dist_u3_regime)
# It seems as though there is strong overlap between the two distributions, with
# recessionary periods tending to have more months with higher unemployment,
# indicating little predictive power of the level of the variable alone

# We'll now look at the changes in unemployment
dist_u3_rate_regime = ggplot(data %>% drop_na(recession, u3_unemployment_diff), 
                             aes(x = u3_unemployment_diff, 
                                 fill = factor(recession, levels = c(0, 1), labels = c("Expansion", "Recession")))) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Expansion" = "#7D4600", "Recession" = "#A4B0F5")) +
  theme_minimal() +
  labs(title = "Distribution: U3 Unemployment Monthly Change by Regime",
       x = "Monthly Change (Percentage Points)", y = "Density", fill = "Economic State") +
  theme(legend.position = "bottom")

print(dist_u3_rate_regime)

# This graph is telling; the distributions overlap heavily, but there is a notable 
# difference in where they are centered, with the recessionary points having positive
# changes in the unemployment rate, with notable outliers. Similarly to our examination
# of the federal funds rate, we'll also look at a box and whiskers plot
box_u3_diff = ggplot(data %>% drop_na(recession, u3_unemployment_diff), 
                     aes(x = factor(recession, levels = c(0, 1), labels = c("Expansion", "Recession")), 
                         y = u3_unemployment_diff, 
                         fill = factor(recession, levels = c(0, 1), labels = c("Expansion", "Recession")))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("Expansion" = "#7D4600", "Recession" = "#A4B0F5")) +
  theme_minimal() +
  labs(title = "Distribution: U3 Unemployment Shifts by Regime",
       x = "Economic State", 
       y = "Month-over-Month Change (Percentage Points)") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(box_u3_diff)

# We get a similar finding here. The distributions are centered differently, with
# an extreme outlier during a recession.





##########################################################################
# How U3 and FFR vary together
##########################################################################

# Since persistent homology relies on relationships via geometric distance,
# it is wise to look at each of the variables together

# looking at levels first
scatter_levels = ggplot(data %>% drop_na(u3_unemployment, fedfunds, recession), 
  aes(y = fedfunds, x = u3_unemployment, color = factor(recession))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("0" = "#f18701", "1" = "#7678ed"), labels = c("Expansion", "Recession")) +
  theme_minimal() +
  labs(title = "Fed Funds vs. U3 Unemployment",
    y = "Federal Funds Rate",
    x = "U3 Unemployment Rate",
    color = "Economic State")

print(scatter_levels)

# This graph is actually quite interesting, we see clustering of points when we
# aren't in a recession, but see more volatility/ pattern breaking when in recession
# this indicates that PH may yield interesting results

# We also do the same with a lagged variable
scatter_levels_lag = ggplot(data %>% drop_na(u3_unemployment, fedfunds, recession_win_6months), 
  aes(y = fedfunds, x = u3_unemployment, color = factor(recession_win_6months))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("0" = "#f18701", "1" = "#7678ed"), labels = c("Not", "Upcoming Recession")) +
  theme_minimal() +
  labs(title = "Fed Funds vs. U3 Unemployment",
    y = "Federal Funds Rate",
    x = "U3 Unemployment Rate",
    color = "Upcoming Recession within 6 months")

print(scatter_levels_lag)

# This gives us more confidence to think recessions, or their precession
# can be spotted by structural breaks from the existing point cloud


# now doing the same but for the differenced shocks
scatter_diffs = ggplot(data %>% 
    drop_na(u3_unemployment_diff, fedfunds_diff, recession) %>% 
    # Filtering out extreme value to better see center of distribution
    filter(fedfunds_diff >-3.7 & u3_unemployment_diff < 5)
  , 
  aes(x = fedfunds_diff, y = u3_unemployment_diff, color = factor(recession))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("0" = "#f18701", "1" = "#7678ed"), labels = c("Expansion", "Recession")) +
  theme_minimal() +
  labs(title = "Fed Funds Shifts vs. Unemployment Shifts, 2 major outliers excluded",
    x = "Monthly Change in Fed Funds Rate",
    y = "Monthly Change in U3 Unemployment",
    color = "Economic State")

print(scatter_diffs)

# We have similar findings here



##########################################################################
# How U3 and FFR vary intertemporaly
##########################################################################

# We should recognize that our data is a time series, and looking at distri
# butions can be misleading due to intertemporal correlations, and the 
# idea that we aren't merely taking random draws from some population.

# We'll visualize how they vary with time
# plotting both raw variables over time

# Havng trouble with time series with my date object
data$date = as.Date(data$date)

# We're going to use dygraph and XTS to better visualize this long time series
data = data %>% arrange(date)
data$date = as.Date(data$date)

# extract the start and end dates of recessions from recession
# This will be used for shading later
rec_starts = data$date[which(diff(c(0, data$recession)) == 1)]
rec_ends   = data$date[which(diff(c(data$recession, 0)) == -1)]

# create the time series objects required for dygraphs
xts_levels = xts(data[, c("fedfunds", "u3_unemployment")], order.by = data$date)
xts_diffs  = xts(data[, c("fedfunds_diff", "u3_unemployment_diff")], order.by = data$date)

# build the interactive levels plot
dy_levels = dygraph(xts_levels, main = "Historical Levels: Fed Funds and Unemployment") %>%
  dySeries("fedfunds", label = "Fed Funds Rate", color = "#ff7d00", strokeWidth = 2) %>%
  dySeries("u3_unemployment", label = "U3 Unemployment", color = "#15616d", strokeWidth = 2) %>%
  dyAxis("y", label = "Rate (%)") %>%
  dyRangeSelector()

# apply the gray recession shading using looping
for(i in seq_along(rec_starts)) {
  dy_levels = dy_levels %>% dyShading(from = rec_starts[i], to = rec_ends[i], color = "lightgray")
}

# build the interactive diffs plot
dy_diffs = dygraph(xts_diffs, main = "Historical Shocks: Rate Changes vs Employment Shifts") %>%
  dySeries("fedfunds_diff", label = "Fed Funds Shift", color = "#ff7d00", strokeWidth = 2) %>%
  dySeries("u3_unemployment_diff", label = "U3 Shift", color = "#15616d", strokeWidth = 2) %>%
  dyAxis("y", label = "Month-over-Month Change") %>%
  dyRangeSelector()

for(i in seq_along(rec_starts)) {
  dy_diffs = dy_diffs %>% dyShading(from = rec_starts[i], to = rec_ends[i], color = "lightgray")
}

# view the plots
dy_levels
dy_diffs
# These graphs tell an interesting story. From the levels, it seems like each
# variable dances after eachother, which is to be expected as
# the federal funds rate is a monetary policy tool to control employment (and inflation) 

# We have similar findings with the shifts of the two variables, with shifts seemingly
# related to one another from being staggered or something else

#########################################################################
# Extreme value analysis for u3 and ffr
#########################################################################

# Earlier, we discovered values of the U3 unemployment change, and the fed funds rate
# change had extreme values, here we shall investigate them

data %>% 
  arrange(fedfunds_diff) %>% 
  head()

# The large negative federal funds rate decrease occured in May, 1980, which
# coincides with violatile behavior by the Federal Reserve, namely, in response
# to a sudden recession, interest rates were greatly cut. Seeing as it is a feature
# of our data that can help inform our 

# For our unemployment outliers
data %>% 
  arrange(desc(u3_unemployment_diff)) %>% 
  head()

# The massive drop in employment can be explained by COVID. 

# Because these outliers 
# represent genuine, systemic macroeconomic events rather than some sort of data collection errors, 
# capping them would artificially bias the exact structural signals that Persistent
# homology is intended to detect.





#########################################################################
# Other extreme values / outlier in the dataset
#########################################################################

# As was argued earlier, outliers, as long as they are not artifacts of poor
# data collection, should remain in our analysis as our use of persistent homology
# relies on larger structural shifts.

# with that said, we will now conduct an analysis to see if there are data errors
# that are producing outliers

# We'll tkake advantage of the dlookr package, which allows for an analysis
# of a large collection of features

# Importing our large dataset

outlier_report = diagnose_outlier(full_data)
print(outlier_report)
# Examining our outliers, we don't find anything that is extremely alarming in 
# terms of data entry/ data errors, seeing as we don't see any physically impossible 
# values. These extremes align with known historical market crashes.

# Some variables are extremely volatile, with log returns of the price of gold
# and snp500 violatility each having around ~35% of their data points made up
# of outliers

##########################################################################
# Missing values are discussed and handled in the "clean_data" script
# We found no meaningful missingness within the ranges that each series
# covers. Missingness is only concerning in that it restricts our period
# of analysis to be that where we have our variables of interest.
##########################################################################

###########################################################################
# Transformations are handled and defended in the "alter data" script
# transformations for this analysis consist of converting variables into
# their "monthly change" computed via simple first differencing and log 
# returns where applicable. The data was then normalized, since PH relies
# upon normalized features.
#
###########################################################################