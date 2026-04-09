##############################################################################
#
# This script is to conduct exploratory data analysis (EDA) on the dataset
# that has been cleaned and modified in other scripts
#
##############################################################################

#################################
# Libraries required
library(tidyverse)
library(ggplot2)
#################################

# Our exploratory analyis will focus on the "haevy hitters" of macroeconomic indicators
# Namely, we'll focus on the federal funds rate and the U3 unemployment rate


data = read.csv("data/clean/transformed_cleaned_data.csv") %>% 
  select(date, year, month, rgdp, fedfunds, u3_unemployment,
    fedfunds_diff, u3_unemployment_diff, recession
    ) %>% 
  filter(year > 1950)

# We'll first get an idea of the statistics behind each variable
summary(data %>% select(-date))

# From this, in adherence with the prompt, we'll look at unconditional
# distributions for our variables of interest
#
# Since raw time series datat would be sort of confusing to visualize using
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
