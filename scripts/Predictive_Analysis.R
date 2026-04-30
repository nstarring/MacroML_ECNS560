# This script will carry off of the findings of the "WassDist_EDA" script and attempt
# to test the predictive power of the wasserstein metrics


# Importing libraries and loading scripts
source("scripts/PH_functions.R")
source("scripts/prediction_functions.R")
library(knitr)
library(kableExtra)


transformed_cleaned_data = read_csv("data/clean/transformed_cleaned_data.csv")
set.seed(123)

# Turn this to TRUE to run the whole pipeline (will take quite long), but it works
# goshdangit as of 4/30 with this seed and this code
# Could probably be sped up if parallelization was applied somehwere, but could run
# into dangerous conflicting worker conditions
if(F){
  # Runing the ml for each horizon, for each desired specification
  message("Running Specification 1")
  pre_depression_vars = c("gold_price_logret_scaled", "industrial_production_logret_scaled",
                          "snp500_avg_close_logret_scaled", "snp500_volatility_scaled")
  results_spec_pre_dep = evaluate_horizons_for_spec(variables = pre_depression_vars, master_df = transformed_cleaned_data)
  
  message("Running Specification 2")
  real_economy = c("consumer_sentiment_imputed_diff_scaled", "industrial_production_logret_scaled",
                   "u3_unemployment_diff_scaled", "housing_starts_diff_scaled")
  results_spec_real_econ = evaluate_horizons_for_spec(variables = real_economy, master_df = transformed_cleaned_data)
  
  message("Running Specification 3")
  monetary_movers = c("u3_unemployment_diff_scaled", "fedfunds_diff_scaled", 
                      "cpi_logret_scaled")
  results_spec_monetary_movers = evaluate_horizons_for_spec(variables = monetary_movers, master_df = transformed_cleaned_data)
  
  # Now we'll save the results so the user doesnt have to go through that hastle of their computer not working
  # for a tad
  write.csv(results_spec_pre_dep, "data/clean/spec_pre_dep_results_raw.csv", row.names = FALSE)
  write.csv(results_spec_real_econ, "data/clean/spec_real_econ_results_raw.csv", row.names = FALSE)
  write.csv(results_spec_monetary_movers, "data/clean/spec_monetary_movers_results_raw.csv", row.names = FALSE)
}

results_spec_pre_dep = read.csv("data/clean/spec_pre_dep_results_raw.csv")
results_spec_real_econ = read.csv("data/clean/spec_real_econ_results_raw.csv")
results_spec_monetary_movers = read.csv("data/clean/spec_monetary_movers_results_raw.csv")

# These results are then piped into dynamic
format_results_table(results_spec_pre_dep, "Pre-Depression Specification Performance")
format_results_table(results_spec_real_econ, "Real Economy Specification Performance")
format_results_table(results_spec_monetary_movers, "Monetary Movers Specification Performance")
