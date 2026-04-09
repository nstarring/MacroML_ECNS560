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
library(dlookr)
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
# can rely on larger structural shifts.

# with that said, we will now conduct an analysis to see if there are data errors
# that are producing outliers

# We'll tkake advantage of the dlookr package, which allows for an analysis
# of a large collection of features

