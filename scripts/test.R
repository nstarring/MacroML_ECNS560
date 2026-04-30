
# This script is sort of a playground for the functions that I've created
# pay it no attention, tjhough some of the interactive plots are actually pretty
# fun to play with
transformed_cleaned_data = read_csv("data/clean/transformed_cleaned_data.csv")

logical_vars = c(
  "snp500_avg_close_logret_scaled",
  "snp500_volatility_diff_scaled",
  "industrial_production_logret_scaled",
  "u3_unemployment_diff_scaled",
  "fedfunds_diff_scaled",
  "tbill_10yr_diff_scaled",
  "cpi_logret_scaled"
)



diag_obj_test = barcodes_overlapping(transformed_cleaned_data, logical_vars, 12,1)

distances_test = compute_wasserstein(diag_obj_test, 2)

graph = plot_distances(distances_test, transformed_cleaned_data, dimensions = c("dim0", "dim1", "dim2", "dim3"))


graph

fig_2d = plot_point_cloud_animation(
  data = transformed_cleaned_data, 
  variables = c("snp500_avg_close_logret_scaled", "tbill_1yr_diff_scaled"), 
  window_size = 12, 
  start_date = "2006-01-01", 
  end_date = "2010-12-01"
)

fig_2d

fig_2d = plot_point_cloud_animation(
  data = transformed_cleaned_data, 
  variables = c("snp500_avg_close_logret_scaled", "tbill_1yr_diff_scaled"), 
  window_size = 12, 
  start_date = "1990-01-01", 
  end_date = "2010-12-01",
  x_label = "snp500 Average Closing Price, Log Returns, Normalized",
  y_label = "1 Year T-Bill Yield, Month over Month difference, Normalized",
  frames = 275,
  transi = 250
)


  