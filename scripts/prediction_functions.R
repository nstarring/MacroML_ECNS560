##############################################################################
#
# The purpose of this script is to contain functions that use machine learning
# techniques to attempt to predict if a given month is within 6 months of a \
# recession, given the wasserstein distances computed at that period's end
#
# We have to create custom folds for training since if we relied on fractional
# stacked window CV, as is typical in time series data, we could easily end 
# up with validation sets that have no recessions within them.
#
# With that said, we also set aside a set testing set for the years 2000+
# 
# We use a lasso, and a normal logistic regression as a baseline
#
##############################################################################

##############################################################################
# Libraries
##############################################################################
library(tidyverse)
library(tidymodels)
library(glmnet)
library(lubridate)


##############################################################################
# Helper function to build a tidymodels rset object from pre-defineed folds
# to build pre-defined folds that it can use
##############################################################################

# function takes two dataframes, one for the training data and another df, 
# fold_defs that is a list of lists where each nested list contains a train_end
# , a val_start and val_end value.

build_manual_rset = function(train_data, fold_defs){
  
  # Iterating over every pre defined fold, calling each f
  splits = lapply(fold_defs, function(f){
    
    # Gives the row numbers of train_data where the date falls
    # on or before the training cutoff specified in the fold_defs
    # Using which() skips boolean vector subsetting
    # back fills so we get the "stacked window" CV that is standard in TS
    
    # This is actually very useful, as it will allow for standardized folds,
    # where it will start the first fold where the training data first starts
    
    # We should still pay attention when creating the folds for each spec, but
    # regardless, its sort of nice
    train_index = which(train_data$window2_end <= f$train_end)
    # Same thing but with validation period
    val_index = which(train_data$window2_end >  f$val_start & train_data$window2_end <= f$val_end)
    
    # Becuase we may get specified folds that result in attempting to create
    # training data without observations for that subset of variables
    # This should be avoided via the construction of the folds
    if(length(train_index) == 0 | length(val_index) == 0){
      return(NULL)
    }
    
    # Creating a custom fold object for tinymodels
    make_splits(
      # analysis is for training, assessment for validation, we
      # essentially assign to x a list of lists of indicies
      x = list(analysis = train_index, assessment = val_index),
      # Telling it where to actually get the values from
      data = train_data
    )
  })
  
  # Drop any NULL folds that got skipped
  # negates the boolean vector that returns true if elements are null
  splits = splits[!sapply(splits, is.null)]
  
  if(length(splits) == 0){
    stop("All folds were empty -- check fold_defs against actual data coverage")
  }
  
  # returns the manual_rset object that tidymodels needs
  manual_rset(splits, ids = paste0("fold_", 1:length(splits)))
}


##############################################################################
# Helper functino to prepare the modeling dataset
##############################################################################

# Takes the Wasserstein distances dataframe (already with dim0/dim1, etc. columns),
# joins on the recession target and builds interactions

# The 
prepare_model_data = function(wass_df, master_df){
  
  # The Wasserstein output already comes wide with dim0, dim1, dim2 cols
  model_df = wass_df %>%
    # Making sure the window2 end is a date object
    mutate(window2_end = as.Date(window2_end)) %>%
    # Joining with the master dataframe to get the recession within 6 months
    # indicator (is already contained)
    left_join(
      master_df %>%
        mutate(date = as.Date(date)) %>%
        select(date, recession_win_6months),
      by = c("window2_end" = "date")
    ) %>%
    # Build interactions between the dimensions
    mutate(
      dim0_x_dim1 = dim0 * dim1,
      dim0_x_dim2 = dim0 * dim2,
      dim1_x_dim2 = dim1 * dim2
    ) %>%
    # Make target a factor
    mutate(recession_win_6months = factor(recession_win_6months, 
                                          levels = c(0, 1))) %>%
    # Drop any rows missing asny data (just in case)
    drop_na()
  
  return(model_df)
}


##############################################################################
# Helper function to run the lasso method for a single Wasserstein dataset
##############################################################################

# Function takes the custom model_df and fold_defs, and 
run_lasso = function(model_df, fold_defs){
  
  # Hard split at 2000, ensuring a consistent testing set
  # the motivation for this is discussed in the write-up, but its
  # really the most effective way to avoid the "looking foward" bias
  # present if we tested on prior recessions
  #
  # With that said, due to the massively heterogenous causative factors
  # that lead to recessions, and perhaps the unique circumstances around these
  # ones, we may get some pretty abismal predictive power
  train_df = model_df %>% filter(window2_end <  as.Date("2000-01-01"))
  test_df  = model_df %>% filter(window2_end >= as.Date("2000-01-01"))
  
  # The features that lasso will use
  feature_cols = c("dim0", "dim1", "dim2",
                   "dim0_x_dim1", "dim0_x_dim2", "dim1_x_dim2")
  
  # Build the recipe
  rec = recipe(recession_win_6months ~ ., # Setting the predicted var
               # just grabbing the feature cols and the indicator var
               data = train_df %>% select(feature_cols, recession_win_6months)) %>%
    # Getting rid of dimensional distances that had no values
    step_zv(all_numeric_predictors()) %>% 
    # Normalizing each factor for lasso
    step_normalize(all_numeric_predictors())
  
  # Logistic LASSO model with lambda to be tuned
  # mixture = 1 means pure LASSO (no ridge component)
  lasso_spec = logistic_reg(penalty = tune(), mixture = 1) %>%
    # Using computation method used in class
    set_engine("glmnet") %>%
    set_mode("classification")
  
  # Creating a workflow object from the recipe and specification
  wflow = workflow() %>%
    add_recipe(rec) %>%
    add_model(lasso_spec)
  
  # Build custom folds for this specific dataset from our helper function
  custom_folds = build_manual_rset(train_df, fold_defs)
  
  # Creating our grid of values to be tuned for
  # We go very low to allow LASSO to find optimal lambda even when 
  # Wasserstein values are small in magnitude, and since compute isn't
  # a big factor when doing just one var in a lasso
  lambda_grid = tibble(penalty = 10^seq(-8, 1, length.out = 80))
  
  # Tune lambda using custom folds, optimizing ROC-AUC
  tune_results = tune_grid(
    wflow,
    resamples = custom_folds,
    grid      = lambda_grid,
    # Settting metric to be roc_auc
    metrics   = metric_set(roc_auc),
    # To have the function output
    control   = control_grid(verbose = F)
  )
  
  # Pick the lambda with highest mean AUC across folds
  best_lambda = select_best(tune_results, metric = "roc_auc")
  
  # Refit on full training set with chosen lambda
  final_wflow = finalize_workflow(wflow, best_lambda)
  final_fit   = fit(final_wflow, data = train_df)
  
  # Predict on the test set and compute AUC
  test_preds = predict(final_fit, test_df, type = "prob") %>%
    bind_cols(test_df %>% select(recession_win_6months))
  
  # Now we put it to thed test data
  test_auc = roc_auc(test_preds,
                     truth = recession_win_6months,
                     .pred_1,
                     # Necessary for the ROC-AUC calc
                     # predicting recessions, not non-recessions
                     event_level = "second") %>%
    # Just getting the estimate
    pull(.estimate)
  
  # Extract which features survived LASSO at the chosen lambda
  glmnet_fit = extract_fit_engine(final_fit)
  # Grabbing the coefficients that corersponse to best lambda
  coefs = coef(glmnet_fit, s = best_lambda$penalty)
  # Converting to numeric
  coefs_vec = as.numeric(coefs)
  # Sets the names of this new dataframe 
  names(coefs_vec) = rownames(coefs)
  
  
  surv = list(
    # Checking if each coefficient is zero, or at least negligebly close 
    # to zero or not
    dim0_kept = abs(coefs_vec["dim0"]) > 1e-10,
    dim1_kept = abs(coefs_vec["dim1"]) > 1e-10,
    dim2_kept = abs(coefs_vec["dim2"]) > 1e-10,
    dim0_x_dim1_kept = abs(coefs_vec["dim0_x_dim1"]) > 1e-10,
    dim0_x_dim2_kept = abs(coefs_vec["dim0_x_dim2"]) > 1e-10,
    dim1_x_dim2_kept = abs(coefs_vec["dim1_x_dim2"]) > 1e-10
  )
  
  # Building the output frame that will be appended in the alter function
  tibble(
    test_auc = test_auc,
    best_lambda = best_lambda$penalty,
    n_train = nrow(train_df),
    n_test = nrow(test_df),
    # This fancy operator takse the elements inside of surv and makes them into
    # columns in the tibble
    !!!surv
  )
}

##############################################################################
# Helper function to a run plain logistic regression as baseline
##############################################################################

# Runs a plain logistic regression on the same train/test split with
# all six variables, returning the test AUC for comparison against the lasso
# and other methods
run_plain_logistic = function(model_df){
  
  # Subsetting the model df into the same train and test split as the ML methods
  train_df = model_df %>% filter(window2_end <  as.Date("2000-01-01"))
  test_df  = model_df %>% filter(window2_end >= as.Date("2000-01-01"))
  
  
  # Plain logistic with all features and interactions, no penalty
  baseline_fit = glm(
    recession_win_6months ~ dim0 + dim1 + dim2 + dim0_x_dim1 + dim0_x_dim2 + dim1_x_dim2,
    data   = train_df,
    # For binary variable
    family = binomial
  )
  
  # Predict probabilities on test set
  baseline_preds = predict(baseline_fit, test_df, type = "response")
  
  # Compute AUC using the same convention as LASSO (positive class = 1)
  pred_df = tibble(
    # Pulls the recession labels
    truth = test_df$recession_win_6months,
    # Attaches the predicted probabilities from the 
    pred  = baseline_preds
  )
  
  # Calculating the auc for the roc (function does most of the lifting for us)
  roc_auc(pred_df, truth = truth, pred, event_level = "second") %>% pull(.estimate)
}


full_coverage_folds = list(
  # Tests the post-WWII cluster (1953, 1957, and 1960 recessions)
  # Gives the model a good 30 years of initial training data (1921 - 1950) if
  # the speficiation goes that far back
  list(train_end = as.Date("1950-01-01"),
       val_start = as.Date("1950-01-01"),
       val_end   = as.Date("1965-01-01")),
  
  # Tests the 1969-1970 recession
  list(train_end = as.Date("1965-01-01"),
       val_start = as.Date("1965-01-01"),
       val_end   = as.Date("1972-01-01")),
  
  # Tests the 1973-1975 Oil recession
  list(train_end = as.Date("1972-01-01"),
       val_start = as.Date("1972-01-01"),
       val_end   = as.Date("1978-01-01")),
  
  # Tests the 1980 and 1981-1982 recessions
  list(train_end = as.Date("1978-01-01"),
       val_start = as.Date("1978-01-01"),
       val_end   = as.Date("1984-01-01")),
  
  # Fold 5: Tests the 1990-1991 recession
  # Ends perfectly before the 2000 boundary split
  list(train_end = as.Date("1984-01-01"),
       val_start = as.Date("1984-01-01"),
       val_end   = as.Date("1995-01-01"))
)


##############################################################################
# Fucntion to Run the full specification grid
##############################################################################
# 
# This function takes a set of variables, a custom fold plan, and a customizabl
# vector of window sizes, and for each specification, computes the roc-auc 
# for each specification, grabs the pertinent lasso variables, and then outputs
# it into a nice format.


run_ml_grid = function(variables, master_df, 
                          fold_defs = full_coverage_folds,
                          overlapping_sizes = c(6, 12, 18, 24, 36),
                          nonoverlapping_sizes = c(8, 12),
                          max_dim = 2){

  
  # Sets then total amount of specifications tested so that
  # we can track the iteration it is on/how much it has left
  total_specifications = length(overlapping_sizes) + length(nonoverlapping_sizes)
  # To store the results
  all_results = list()
  # To keep track of what iteration we're on
  spec_counter = 0
  
  # Outer loop: overlap type
  for(overlap_type in c("overlapping", "nonoverlapping")){
    
    # Pick window sizes and barcode function for this overlap type
    # We are essentially reassigning the function depending on the 
    # value of the overlapping or non-overlappping, saving if statements
    # below
    sizes = if(overlap_type == "overlapping") overlapping_sizes else nonoverlapping_sizes
    barcode_fn = if(overlap_type == "overlapping") barcodes_overlapping else barcodes_non_overlapping
    
    # Inner loop: window size
    for(ws in sizes){
      
      spec_counter = spec_counter + 1
      message(paste0("\nSpec : ",spec_counter, "/", total_specifications, "\nType: ",
                     overlap_type, "   Window size: ", ws)) 
      
      # Compute barcodes
      message("Computing barcodes")
      diag_obj = barcode_fn(master_df, variables, ws, max_dim)
      
      # Compute distances between consecutive windows
      message("Computing Wasserstein distances")
      wass = compute_wasserstein(diag_obj, max_dim)
      
      # Build modeling dataset with target and interactions
      model_df = prepare_model_data(wass, master_df)
      
      # Run Lasso
      message("Running Lasso")
      lasso_result = run_lasso(model_df, fold_defs)
      
      # Run plain logistic as baseline for comparison
      message("Running plain logistic baseline")
      baseline_auc = run_plain_logistic(model_df)
      
      # Takes lasso result df and appends more variables
      result_row = lasso_result %>%
        mutate(
          overlap = overlap_type,
          window_size = ws,
          plain_logit_auc = baseline_auc
        ) %>%
        # Everything() grabs all the other variables and places tham at the end
        select(overlap, window_size, test_auc, plain_logit_auc, everything())
      
      # Assigning the new row to the big list we started at the beginning
      all_results[[spec_counter]] = result_row
      
      
      message(paste0("Lasso Test AUC: ", round(lasso_result$test_auc,4), " | Plain Logit AUC: ", 
                     round(baseline_auc, 4)))
    }
  }
  # Binds the list of rows into a dataframe
  bind_rows(all_results)
}
