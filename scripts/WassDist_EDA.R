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
library(htmlwidgets)

# Loading master data
master_data = read_csv("../data/clean/transformed_cleaned_data.csv")

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
  "u3_unemployment_diff_scaled", "housing_starts_diff_scaled")

# Since the inverted yield curve is thought to be a fairly good predictor
# of recessions, we'll have that here
yield_curve = c("tbill_10yr_diff_scaled", "tbill_1yr_diff_scaled", "fedfunds_diff_scaled")


# Attempting to get an idea of how a large collection of all of these variables relate to one another
mano_del_dios = c(
  "snp500_avg_close_logret_scaled", "nasdaq_avg_close_logret_scaled",
  "snp500_volatility_scaled",
  "fedfunds_diff_scaled", "tbill_1yr_diff_scaled", "tbill_10yr_diff_scaled",
  "u3_unemployment_diff_scaled",
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
# These point clouds use overlapping windows

# Where the saved plots live
plot_dir = "../output/interactive_plotly_plots"


########################################################################
# pre_depression_vars
########################################################################
# Gold + industrial production. Flight-to-safety vs real economy
rds_path  = file.path(plot_dir, "pre_depression_2d.rds")
html_path = file.path(plot_dir, "pre_depression_2d.html")
if(!file.exists(rds_path)){
  pre_depression_2d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("gold_price_logret_scaled", "industrial_production_logret_scaled"),
    window_size = 12,
    start_date  = "1928-01-01",
    end_date    = "2024-12-01",
    x_label     = "Gold Price (log return, scaled)",
    y_label     = "Industrial Production (log return, scaled)"
  )
  saveRDS(pre_depression_2d, rds_path)
  saveWidget(pre_depression_2d, html_path, selfcontained = TRUE)
} else {
  pre_depression_2d = readRDS(rds_path)
}

# Adds equity market dimension to the gold/IP pair
rds_path  = file.path(plot_dir, "pre_depression_3d.rds")
html_path = file.path(plot_dir, "pre_depression_3d.html")
if(!file.exists(rds_path)){
  pre_depression_3d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("gold_price_logret_scaled", "industrial_production_logret_scaled",
                    "snp500_avg_close_logret_scaled"),
    window_size = 12,
    start_date  = "1928-01-01",
    end_date    = "2024-12-01",
    x_label     = "Gold Price (log return, scaled)",
    y_label     = "Industrial Production (log return, scaled)",
    z_label     = "S&P 500 (log return, scaled)"
  )
  saveRDS(pre_depression_3d, rds_path)
  saveWidget(pre_depression_3d, html_path, selfcontained = TRUE)
} else {
  pre_depression_3d = readRDS(rds_path)
}

pre_depression_2d
pre_depression_3d


########################################################################
# monetary_movers
########################################################################
# Dual mandate in two axes
rds_path  = file.path(plot_dir, "monetary_2d.rds")
html_path = file.path(plot_dir, "monetary_2d.html")
if(!file.exists(rds_path)){
  monetary_2d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("u3_unemployment_diff_scaled", "fedfunds_diff_scaled"),
    window_size = 12,
    start_date  = "1954-08-01",
    end_date    = "2024-12-01",
    x_label     = "U3 Unemployment (monthly change, scaled)",
    y_label     = "Fed Funds Rate (monthly change, scaled)"
  )
  saveRDS(monetary_2d, rds_path)
  saveWidget(monetary_2d, html_path, selfcontained = TRUE)
} else {
  monetary_2d = readRDS(rds_path)
}

# Adds inflation to the dual mandate picture
rds_path  = file.path(plot_dir, "monetary_3d.rds")
html_path = file.path(plot_dir, "monetary_3d.html")
if(!file.exists(rds_path)){
  monetary_3d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("u3_unemployment_diff_scaled", "fedfunds_diff_scaled",
                    "cpi_logret_scaled"),
    window_size = 12,
    start_date  = "1954-08-01",
    end_date    = "2024-12-01",
    x_label     = "U3 Unemployment (monthly change, scaled)",
    y_label     = "Fed Funds Rate (monthly change, scaled)",
    z_label     = "CPI (log return, scaled)"
  )
  saveRDS(monetary_3d, rds_path)
  saveWidget(monetary_3d, html_path, selfcontained = TRUE)
} else {
  monetary_3d = readRDS(rds_path)
}

monetary_2d
monetary_3d


########################################################################
# real_economy
########################################################################
# Output and labor utilization
rds_path  = file.path(plot_dir, "real_economy_2d.rds")
html_path = file.path(plot_dir, "real_economy_2d.html")
if(!file.exists(rds_path)){
  real_economy_2d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("industrial_production_logret_scaled", "u3_unemployment_diff_scaled"),
    window_size = 12,
    start_date  = "1959-01-01",
    end_date    = "2024-12-01",
    x_label     = "Industrial Production (log return, scaled)",
    y_label     = "U3 Unemployment (monthly change, scaled)"
  )
  saveRDS(real_economy_2d, rds_path)
  saveWidget(real_economy_2d, html_path, selfcontained = TRUE)
} else {
  real_economy_2d = readRDS(rds_path)
}

# Adds consumer sentiment / expectations dimension
rds_path  = file.path(plot_dir, "real_economy_3d.rds")
html_path = file.path(plot_dir, "real_economy_3d.html")
if(!file.exists(rds_path)){
  real_economy_3d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("industrial_production_logret_scaled", "u3_unemployment_diff_scaled",
                    "consumer_sentiment_imputed_diff_scaled"),
    window_size = 12,
    start_date  = "1959-01-01",
    end_date    = "2024-12-01",
    x_label     = "Industrial Production (log return, scaled)",
    y_label     = "U3 Unemployment (monthly change, scaled)",
    z_label     = "Consumer Sentiment (monthly change, scaled)"
  )
  saveRDS(real_economy_3d, rds_path)
  saveWidget(real_economy_3d, html_path, selfcontained = TRUE)
} else {
  real_economy_3d = readRDS(rds_path)
}


real_economy_2d
real_economy_3d


########################################################################
# yield_curve
########################################################################
# Yield curve in its purest form
rds_path  = file.path(plot_dir, "yield_curve_2d.rds")
html_path = file.path(plot_dir, "yield_curve_2d.html")
if(!file.exists(rds_path)){
  yield_curve_2d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("tbill_10yr_diff_scaled", "tbill_1yr_diff_scaled"),
    window_size = 12,
    start_date  = "1953-04-01",
    end_date    = "2024-12-01",
    x_label     = "10-Year T-Bill (monthly change, scaled)",
    y_label     = "1-Year T-Bill (monthly change, scaled)"
  )
  saveRDS(yield_curve_2d, rds_path)
  saveWidget(yield_curve_2d, html_path, selfcontained = TRUE)
} else {
  yield_curve_2d = readRDS(rds_path)
}

# Adds the Fed's policy lever
rds_path  = file.path(plot_dir, "yield_curve_3d.rds")
html_path = file.path(plot_dir, "yield_curve_3d.html")
if(!file.exists(rds_path)){
  yield_curve_3d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("tbill_10yr_diff_scaled", "tbill_1yr_diff_scaled",
                    "fedfunds_diff_scaled"),
    window_size = 12,
    start_date  = "1954-08-01",
    end_date    = "2024-12-01",
    x_label     = "10-Year T-Bill (monthly change, scaled)",
    y_label     = "1-Year T-Bill (monthly change, scaled)",
    z_label     = "Fed Funds Rate (monthly change, scaled)"
  )
  saveRDS(yield_curve_3d, rds_path)
  saveWidget(yield_curve_3d, html_path, selfcontained = TRUE)
} else {
  yield_curve_3d = readRDS(rds_path)
}

# No super clear evidence
yield_curve_2d
yield_curve_3d


########################################################################
# mano_del_dios
########################################################################
# Picking variables not already shown by other specs -- equity / sentiment
rds_path  = file.path(plot_dir, "mano_2d.rds")
html_path = file.path(plot_dir, "mano_2d.html")
if(!file.exists(rds_path)){
  mano_2d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("nasdaq_avg_close_logret_scaled", "snp500_volatility_scaled"),
    window_size = 12,
    start_date  = "1971-02-01",
    end_date    = "2024-12-01",
    x_label     = "NASDAQ (log return, scaled)",
    y_label     = "S&P 500 Volatility (scaled)"
  )
  saveRDS(mano_2d, rds_path)
  saveWidget(mano_2d, html_path, selfcontained = TRUE)
} else {
  mano_2d = readRDS(rds_path)
}

# Adds russell 2000 (small cap)
rds_path  = file.path(plot_dir, "mano_3d.rds")
html_path = file.path(plot_dir, "mano_3d.html")
if(!file.exists(rds_path)){
  mano_3d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("nasdaq_avg_close_logret_scaled", "snp500_volatility_scaled",
                    "russell2000_avg_close_logret_scaled"),
    window_size = 12,
    start_date  = "1987-09-01",
    end_date    = "2024-12-01",
    x_label     = "NASDAQ (log return, scaled)",
    y_label     = "S&P 500 Volatility (scaled)",
    z_label     = "Russell 2000 (log return, scaled)"
  )
  saveRDS(mano_3d, rds_path)
  saveWidget(mano_3d, html_path, selfcontained = TRUE)
} else {
  mano_3d = readRDS(rds_path)
}

# No super strong trends
mano_2d
mano_3d


########################################################################
# widespread_panic
########################################################################
# Consumer fear vs market fear
rds_path  = file.path(plot_dir, "panic_2d.rds")
html_path = file.path(plot_dir, "panic_2d.html")
if(!file.exists(rds_path)){
  panic_2d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("google_recession_trends_diff_scaled", "snp500_volatility_scaled"),
    window_size = 12,
    start_date  = "2004-01-01",
    end_date    = "2024-12-01",
    x_label     = "Google Recession Trends (monthly change, scaled)",
    y_label     = "S&P 500 Volatility (scaled)"
  )
  saveRDS(panic_2d, rds_path)
  saveWidget(panic_2d, html_path, selfcontained = TRUE)
} else {
  panic_2d = readRDS(rds_path)
}

# Adds u6 unemployment for full panic picture
rds_path  = file.path(plot_dir, "panic_3d.rds")
html_path = file.path(plot_dir, "panic_3d.html")
if(!file.exists(rds_path)){
  panic_3d = plot_point_cloud_animation(
    data        = master_data,
    variables   = c("google_recession_trends_diff_scaled", "snp500_volatility_scaled",
                    "u6_unemployment_diff_scaled"),
    window_size = 12,
    start_date  = "2004-01-01",
    end_date    = "2024-12-01",
    x_label     = "Google Recession Trends (monthly change, scaled)",
    y_label     = "S&P 500 Volatility (scaled)",
    z_label     = "U6 Unemployment (monthly change, scaled)"
  )
  saveRDS(panic_3d, rds_path)
  saveWidget(panic_3d, html_path, selfcontained = TRUE)
} else {
  panic_3d = readRDS(rds_path)
}

# Both of these plots are quite interesting, both seem to show
# topological difference between periods preceeeding the 08 and 20 recessions
panic_2d
panic_3d





# These dynamic plots may indicate some sort of structural break being
# predictive of a recession, but its tough to know for sure, especially
# considering the heterogeneity across the selections of variables chosen.
# Lets compute the waserstein distances for each specfication using a 12 month window
# and see if we can see any interesting appterns across the specficiations

########################################################################
# pre_depression_vars
########################################################################
pre_depression_barcodes = barcodes_overlapping(
  data = master_data,
  variables = pre_depression_vars,
  window_size  = 24,
  max_dimension = 2
)
pre_depression_wass = compute_wasserstein(
  barcode_df = pre_depression_barcodes,
  max_d = 2
)
pre_depression_dygraph = plot_distances(
  dist_data = pre_depression_wass,
  full_data = master_data,
  dimensions = c("dim0", "dim1", "dim2"),
  subtitle = "Pre-depression vars (gold, industrial production, S&P 500, ...)"
)
pre_depression_dygraph
########################################################################
# monetary_movers
########################################################################
monetary_barcodes = barcodes_overlapping(
  data = master_data,
  variables = monetary_movers,
  window_size = 24,
  max_dimension = 2
)
monetary_wass = compute_wasserstein(
  barcode_df = monetary_barcodes,
  max_d = 2
)
monetary_dygraph = plot_distances(
  dist_data = monetary_wass,
  full_data = master_data,
  dimensions = c("dim0", "dim1", "dim2"),
  subtitle = "Monetary movers (U3 unemployment, fed funds, CPI)"
)
monetary_dygraph
########################################################################
# real_economy
########################################################################
real_economy_barcodes = barcodes_overlapping(
  data = master_data,
  variables = real_economy,
  window_size = 24,
  max_dimension = 2
)
real_economy_wass = compute_wasserstein(
  barcode_df = real_economy_barcodes,
  max_d = 2
)
real_economy_dygraph = plot_distances(
  dist_data = real_economy_wass,
  full_data = master_data,
  dimensions = c("dim0", "dim1", "dim2"),
  subtitle = "Real economy (consumer sentiment, industrial production, U3 unemployment, ...)"
)
real_economy_dygraph
########################################################################
# yield_curve
########################################################################
yield_curve_barcodes = barcodes_overlapping(
  data = master_data,
  variables = yield_curve,
  window_size = 24,
  max_dimension = 2
)
yield_curve_wass = compute_wasserstein(
  barcode_df = yield_curve_barcodes,
  max_d  = 2
)
yield_curve_dygraph = plot_distances(
  dist_data = yield_curve_wass,
  full_data = master_data,
  dimensions = c("dim0", "dim1", "dim2"),
  subtitle = "Yield curve (10-year T-bill, 1-year T-bill, fed funds)"
)
yield_curve_dygraph
########################################################################
# mano_del_dios
########################################################################
mano_barcodes = barcodes_overlapping(
  data = master_data,
  variables = mano_del_dios,
  window_size = 24,
  max_dimension = 2
)
mano_wass = compute_wasserstein(
  barcode_df = mano_barcodes,
  max_d = 2
)
mano_dygraph = plot_distances(
  dist_data = mano_wass,
  full_data = master_data,
  dimensions = c("dim0", "dim1", "dim2"),
  subtitle = "Mano del dios (S&P 500, NASDAQ, fed funds, U3 unemployment, ...)"
)
mano_dygraph
########################################################################
# widespread_panic
########################################################################
panic_barcodes = barcodes_overlapping(
  data = master_data,
  variables = widespread_panic,
  window_size = 24,
  max_dimension = 2
)
panic_wass = compute_wasserstein(
  barcode_df = panic_barcodes,
  max_d = 2
)
panic_dygraph = plot_distances(
  dist_data = panic_wass,
  full_data = master_data,
  dimensions = c("dim0", "dim1", "dim2"),
  subtitle = "Widespread panic (Google recession trends, S&P 500 volatility, U6 unemployment)s"
)
panic_dygraph

# From each of these graphs, and considering that data availability is of concern
# when doing a ML technique, the "best performing" specifications from this 
# very cursory look at Wasserstein distances are the pre-depression vars, the
# "real economy" and "monetary movers" specifications are chosen to go foward



