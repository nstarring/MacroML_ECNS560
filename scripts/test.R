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



diag_obj_test = barcodes_overlapping(transformed_cleaned_data, logical_vars, 12,2)

distances_test = compute_wasserstein(diag_obj_test, 2)

graph = plot_distances(distances_test, transformed_cleaned_data, dimensions = c("dim0", "dim1", "dim2", "dim3"))

graph %>% htmlwidgets::saveWidget("wasserstein_plot.html")

graph

results = run_ml_grid(logical_vars, transformed_cleaned_data)
  

  