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
# To create a nice table on the other side
library(knitr)
library(kableExtra)
library(dplyr)

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

prepare_model_data = function(wass_df, master_df, target_col){
  
  # The Wasserstein output already comes wide with dim0, dim1, dim2 cols
  model_df = wass_df %>%
    # Making sure the window2 end is a date object
    mutate(window2_end = as.Date(window2_end)) %>%
    # Joining with the master dataframe to get the specific indicator
    left_join(
      master_df %>%
        mutate(date = as.Date(date)) %>%
        # Dynamically selects the requested target column and renames it to target_var
        select(date, target_var = all_of(target_col)),
      by = c("window2_end" = "date")
    ) %>%
    # Build interactions between the dimensions
    mutate(
      dim0_x_dim1 = dim0 * dim1,
      dim0_x_dim2 = dim0 * dim2,
      dim1_x_dim2 = dim1 * dim2
    ) %>%
    # Make target a factor
    mutate(target_var = factor(target_var, levels = c(0, 1))) %>%
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
  rec = recipe(target_var ~ ., # Setting the predicted var
               # just grabbing the feature cols and the indicator var
               data = train_df %>% select(all_of(feature_cols), target_var)) %>%
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
  # Due to extreme recession sparsity in non-overlapping windows, dynamic LASSO penalty 
  # tuning was bypassed in favor of a static penalty parameter where necessary to
  # prevent cross-validation failure, this allows for this function to continue running,
  # while also acknowledging what happened (we'll put an astericks in the table when it is computed)
  tuning_attempt = tryCatch({
    tune_res = tune_grid(
      wflow,
      resamples = custom_folds,
      grid      = lambda_grid,
      # Settting metric to be roc_auc
      metrics   = metric_set(roc_auc),
      # To have the function output
      control   = control_grid(verbose = F)
    )
    # Pick the lambda with highest mean AUC across folds
    select_best(tune_res, metric = "roc_auc")
  }, error = function(e){return(NULL)})
  
  if(!is.null(tuning_attempt)){
    best_lambda = tuning_attempt
    tuning_failed = F
  } else {
    best_lambda = tibble(penalty = 1e-4)
    tuning_failed = T
  }
  
  # Refit on full training set with chosen lambda
  final_wflow = finalize_workflow(wflow, best_lambda)
  final_fit   = fit(final_wflow, data = train_df)
  
  # Predict on the test set and compute AUC
  test_preds = predict(final_fit, test_df, type = "prob") %>%
    bind_cols(test_df %>% select(target_var))
  
  # Now we put it to thed test data
  test_auc = roc_auc(test_preds,
                     truth = target_var,
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
    # isTRUE() forces any NAs from missing coefficients to become FALSE
    dim0_kept = isTRUE(abs(coefs_vec["dim0"]) > 1e-10),
    dim1_kept = isTRUE(abs(coefs_vec["dim1"]) > 1e-10),
    dim2_kept = isTRUE(abs(coefs_vec["dim2"]) > 1e-10),
    dim0_x_dim1_kept = isTRUE(abs(coefs_vec["dim0_x_dim1"]) > 1e-10),
    dim0_x_dim2_kept = isTRUE(abs(coefs_vec["dim0_x_dim2"]) > 1e-10),
    dim1_x_dim2_kept = isTRUE(abs(coefs_vec["dim1_x_dim2"]) > 1e-10)
  )
  
  # Building the output frame that will be appended in the alter function
  tibble(
    test_auc = test_auc,
    best_lambda = best_lambda$penalty,
    # Adding a flag if the tuning failed due to not enough folds
    tuning_failed = tuning_failed,
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
    target_var ~ dim0 + dim1 + dim2 + dim0_x_dim1 + dim0_x_dim2 + dim1_x_dim2,
    data   = train_df,
    # For binary variable
    family = binomial
  )
  
  # Predict probabilities on test set
  baseline_preds = predict(baseline_fit, test_df, type = "response")
  
  # Compute AUC using the same convention as LASSO (positive class = 1)
  pred_df = tibble(
    # Pulls the recession labels
    truth = test_df$target_var,
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
                       
                       target_col = "recession_within_6mo",
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
      model_df = prepare_model_data(wass, master_df, target_col)
      
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
          plain_logit_auc = baseline_auc,
          target_predicted = target_col # Helps track which target was used in final table
        ) %>%
        # Everything() grabs all the other variables and places tham at the end
        select(target_predicted, overlap, window_size, test_auc, plain_logit_auc, everything())
      
      # Assigning the new row to the big list we started at the beginning
      all_results[[spec_counter]] = result_row
      
      
      message(paste0("Lasso Test AUC: ", round(lasso_result$test_auc,4), " | Plain Logit AUC: ", 
                     round(baseline_auc, 4)))
    }
  }
  # Binds the list of rows into a dataframe
  bind_rows(all_results)
}

##############################################################################
# Function to evaluate a specification across all time horizons
##############################################################################
#
# This wrapper runs run_ml_grid for 6, 12, and 18 month targets.
# It then cleans the output, rounds the metrics, collapses the LASSO features
# into a readable string, and formats it into a nice lookin table

# the ... allows us to pass a buynch of arguments into the function signature
# while not robust, since it is a function that will be used for a carefully 
# designed workflow, its not the biggest deal.
evaluate_horizons_for_spec = function(variables, master_df, spec_name = "Spec 1", ...){
  
  # The targets we want to evaluate (all the horzizons we have in our dataframe)
  targets = c("recession_within_6mo", "recession_within_12mo", "recession_within_18mo")
  
  # Initializes an empty vector to store the resulst from each iteration
  all_horizons = list()
  counter = 0
  for(t in targets){
    counter = counter + 1
    message(paste0("------------------------BEGINNING TARGET ", counter ,"/", length(targets),": ", t))
    # Run the grid we built earlier
    res = run_ml_grid(variables = variables, master_df = master_df, target_col = t, ...)
    
    # Assinging the actual value of the t'th element
    # if we used single bracketrs it would try to convert it to a list
    all_horizons[[t]] = res
  }
  
  # Bind all the results together
  final_df = bind_rows(all_horizons)
  
  # Clean and format for the final report
  report_table = final_df %>%
    mutate(
      # Make the target column nicer to look at
      Target = case_when(
        target_predicted == "recession_within_6mo"  ~ "6 Months",
        target_predicted == "recession_within_12mo" ~ "12 Months",
        target_predicted == "recession_within_18mo" ~ "18 Months",
        TRUE ~ target_predicted
      ),
      # Format metrics
      `Lasso AUC` = paste0(round(test_auc, 3)),
      `Logit AUC` = paste0(round(plain_logit_auc, 3)),
      # Format Lambda to scientific notation so it doesn't take up massive space
      # Flags the lambdas that failed from not tuning
      Lambda = ifelse(tuning_failed, 
                      paste0(formatC(best_lambda, format = "e", digits = 2), "*"), 
                      formatC(best_lambda, format = "e", digits = 2))
    ) %>%
    # Collapse the surviving features into a single string, so the reader can just look at
    # a compiled list instead of across a bunch of cells
    # Rowise forces for the following operations to only appy to one row at a time
    rowwise() %>%
    # Checks each of the dimension columns, and decides to append the keepers to a list
    # 
    mutate(
      `Features Kept` = paste(c(
        if(dim0_kept) "D0" else NULL,
        if(dim1_kept) "D1" else NULL,
        if(dim2_kept) "D2" else NULL,
        if(dim0_x_dim1_kept) "D0xD1" else NULL,
        if(dim0_x_dim2_kept) "D0xD2" else NULL,
        if(dim1_x_dim2_kept) "D1xD2" else NULL
        # collapse turns a vector of strings and combines them together, spereated
        # by a comma and a space
      ), collapse = ", ")
    ) %>%
    # Getting out of rowise
    ungroup() %>%
    # Clean up empty feature strings (if Lasso killed everything)
    mutate(`Features Kept` = ifelse(`Features Kept` == "", "None", `Features Kept`)) %>%
    # Select only the columns needed for the report and clean it up a littl moer  
    select(Target, Overlap = overlap, Window = window_size, 
           `Lasso AUC`, `Logit AUC`, Lambda, `Features Kept`) %>%
    # Sort nicely according to target, overlap and window
    arrange(Target, desc(Overlap), Window)
  
  return(report_table)
}

##############################################################################
# Function to tailor output of above function into nice table
#############################################################################
format_results_table = function(df, caption_text) {
  df %>%
    kable(
      # Set to be an html element
      format = "html",
      caption = caption_text,
      # center text
      align = "c",
      # Allow for our astericks to be there
      escape = FALSE
    ) %>%
    kable_styling(
      # styling options, kable makes it nice and we can just give it a list
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "center"
    ) %>%
    # Styles the header row
    # Uses a "professional color pallete"
    row_spec(0, bold = TRUE, color = "white", background = "#70002e") %>%
    # Bolds the Target column
    column_spec(1, bold = TRUE) %>%
    # Merges repeating values in the target column to void clutter
    # an option seen often in high tier charts
    collapse_rows(columns = 1:2, valign = "top") %>%
    footnote(
      general = "* Indicates optimal parameter tuning failed due to few recessions in cross-validation folds; a lamabda value of 1e-4 was applied.",
      general_title = "Note: ",
      footnote_as_chunk = TRUE
    )
}