##############################################################################
#
# This script contains helper functions for, and two main sets of functions
# that compute persistence barcodes for windows of the data, and the
# corresponding wassterstein metric between periods
#
# The workflow consists of two stages:
# 1. Compute persistence diagrams over time windows (overlapping or not)
# 2. Compute Wasserstein distances between consecutive windows' diagrams
#
# This script is only meant to be ran after alter_data.R so that the 
# "transformed_cleaned_data.csv / .RData" can be used
#
# future and furr are used as the main parallel libraries due to some difficulties
# when using just parallel
#
##############################################################################

#################################
# Libraries required
##################################
library(tidyverse)
# For ripsDiag and wasserstein
library(TDA)

# For parallel processing
library(furrr)
library(future)
library(parallel)

# Progress bars that work with parallel libraries
library(progressr)

# For plotting time series
library(dygraphs)
library(xts)


#################################

#################################
#
# Helper functions
#
#################################

##############################################################################
# Shared Coverage helper function
##############################################################################

# This helper is used by both window-building functions. It takes the master 
# dataframe and the vector of variables the user wants to analyze, then fin
# ds the range of dates where ALL of those variables have non-missing values.
# It also caps it at the end of 2024 to avoid weird data gaps due to the govern
# ment shutdown.


get_shared_coverage_data = function(data, variables) {
  
  # Ensuring date is a Date object (when loading in data)
  data = data %>% mutate(date = as.Date(date))
  
  # Subsetting to just the variables of interest plus date
  subset_data = data %>% select(date, all_of(variables))
  
  # Finding the rows where all variables of interest are non-missing
  complete_rows = subset_data %>%
    
    # if_all only evaluates to be true if each row of the subset contains
    # non-NA values
    # Filters by the rows that have data filled in.
    filter(if_all(all_of(variables), ~ !is.na(.x)))
  
  # Finding the first and last dates of complete coverage
  first_date = min(complete_rows$date)
  last_date  = min(max(complete_rows$date), as.Date("2024-12-01"))
  
  # Subsetting the data to this range
  shared = subset_data %>% 
    filter(date >= first_date & date <= last_date) %>%
    arrange(date)
  
  return(shared)
}





##############################################################################
# Persistence diagram for window helper function
##############################################################################

# This function takes a set of data points (the ones in the window), and computes
# a persistence diagram for each dimension specified, with a given max scale

# max_scale is a paremeter that alters "how big" the n-dimensional sphere around
# a point can grow. If the parameter is set too small, we risk missing some 
# topological features that may appear at relatively larger distances apart,
# (we want to capture outlier points like covid, for example)
# if we set it too large, it is both computationally intensive, and leads to
# already connected features (their spheres are touching) creating non-valuable
# higher-dimensional objects, which we are not interested in for the purposes
# of this analysis. A more thorough justification for the parameter set will
# be given later on

compute_diag_obj = function(datum, max_dim, maxscale) {
  # ripsDiag function computes the Vietoris-Rips filtration for our data
  # essentially grows an n-dimensional sphere around each point
  # When two spheres touch, an edge is created between the points, forming stru
  # ctures. This process keeps track of each structure at different levels
  # of radius of the sphere.
  diag_obj = ripsDiag(
    X = datum,
    # Setting the max dimension of strucutre it will track
    maxdimension = max_dim,
    maxscale = maxscale,
    # Alters how algorithm is implemented. This way is faster and more memory
    # efficient
    library = "GUDHI",
    # False since we'll be using a different progress bar library
    printProgress = FALSE
  )
  # Returns the object, containing barcode
  return(diag_obj$diagram)
}

##############################################################################
# Inter-window Wasserstein distance helper function
##############################################################################

# Takes two persistence diagrams, and returns a dataframe noting the distance
# for each dimension analyzed

wasserstein_by_dimension = function(diag1, diag2, max_dimension){
  results = data.frame()
  for(d in 0:max_dimension){
    results = rbind(results, data.frame(
      dimension = d,
      # p alters the type of Wasserstein distance being calculated
      # the 2-Wasserstein or Earth Mover's distance is standard
      # penalizes large distances more than smaller ones
      wasserstein_dist = wasserstein(diag1, diag2, p = 2, dimension = d)
    ))
  }
  results
}



#################################
#
# Main Functions
#
#################################

##############################################################################
# Computing persistence diagrams for each window, non-overlapping
##############################################################################

# Computes persistence diagrams for non-overlapping, consecutive windows
# Takes the following arguments: data - the cleaned dataframe with desired vars
#                                variables - a string vector of the variables to be analyzed
#                                window_size - sets the number of months within the window
#                                max_dim - sets the dimensionality to be analyzed
# This function will default use all but 1 cores for working
#
# It will return a datafram/tibble containing an identifier variable, the start date
# of the window, and the end date of the interval, and the diagram object 

barcodes_non_overlapping = function(data, variables, window_size, max_dimension) {
  
  # We first subset the data to only include months where all variables have data
  shared = get_shared_coverage_data(data, variables)
  
  # To create our empty tibble, we need to calculate how many windows there will be
  n_windows  = floor(nrow(shared) / window_size)
  
  # output statement just to make sure user isnt getting a very small data set
  # We'll use message() since it looks nicer
  message(paste0("Number of months in subsetted data: ", nrow(shared), "\n", 
              "Number of windows to be computed: ", n_windows))
  
  # Creating skeleton tibble
  windows = tibble(
    id = 1:n_windows,
    # We need a way to easily get the starting index for a window and the end index
    # from the shared dataframe so that we can easily transfer them
    # The start of the window is 1 window length less than the end of the window 
    # (obviously) the +1 adjustment is to account for R indexing
    start_id = (1:n_windows - 1) * window_size + 1,
    end_id = 1:n_windows * window_size
  ) %>% 
    # Now we can easily grab the start and end dates from the shared data
    mutate(
      start_date = shared$date[start_id],
      end_date = shared$date[end_id]
    )
  
  # Now we build a collection of point clouds for each window
  window_datum = lapply(1:n_windows, function(i){
    # A matter of slicing the shared data into chunks, and returning
    # those chunks
    shared %>% 
      # Slice the data according to the identifiers we made in the last part
      slice(windows$start_id[i]:windows$end_id[i]) %>% 
      select(variables) %>% 
      # Since the ripsDiag function requires a matrix object (not a tibble/df)
      as.matrix()
  })
  
  # Now we'll compute the max_scale to set for ripsDiag function
  # this must be standardized across the computation of every window in order
  # for a consistently measured Wasserstein distance. To ensure that every
  # structure is found for every window (we don't miss anything), we'll use the
  # maximum distance found in any such window between points.
  max_scale = max(
    sapply(window_datum, function(window){
      max(dist(window))
    })
  )
  
  # Prior to computing each barcode, we'll start up our workers
  # plan() is a future function which defines the parallel strategy
  plan(multisession, workers = detectCores() - 2)
  # Use on.exit() so that the workers are reset once function finishes
  on.exit(plan(sequential))
  
  
  # with_progress allows for progress tracking
  with_progress({
    # Creates progress tracking object, defining how many steps to expect
    p = progressor(along = window_datum)
    # Use future_map for parallel processing
    diagrams = future_map(window_datum, function(window){
      # Each time a worker finishes, p() is called, advancing progress bar
      p()
      compute_diag_obj(window, max_dim = max_dimension, maxscale = max_scale)
    })
  })
  
  # Now assembling the returnable object
  return(
    windows %>% 
      select(id, start_date, end_date) %>% 
      mutate(
        diagram_obj = diagrams
      )
  )
  
}




##############################################################################
# Computing persistence diagrams for each window, overlapping
##############################################################################
#
# This function is identical to the one above aside from how the windows are computed
#
# Computes persistence diagrams for non-overlapping, consecutive windows
# Takes the following arguments: data - the cleaned dataframe with desired vars
#                                variables - a string vector of the variables to be analyzed
#                                window_size - sets the number of months within the window
#                                max_dim - sets the dimensionality to be analyzed
# This function will default use all but 1 cores for working
#
# It will return a datafram/tibble containing an identifier variable, the start date
# of the window, and the end date of the interval, and the diagram object 

barcodes_overlapping = function(data, variables, window_size, max_dimension) {
  
  # We first subset the data to only include months where all variables have data
  shared = get_shared_coverage_data(data, variables)
  
  
  # Start indices just increment by 1, adjusted so that there is no windows
  # with small amounts of data
  start_indicies = 1:(nrow(shared) - window_size + 1)
  n_windows = length(start_indicies)
  
  
  windows = tibble(
    id = 1:n_windows,
    start_id = start_indicies,
    # Incrementing the end index
    end_id = start_indicies + window_size - 1
  ) %>%
    mutate(
      start_date = shared$date[start_id],
      end_date = shared$date[end_id]
    )
  
  # Now we build a collection of point clouds for each window
  window_datum = lapply(1:n_windows, function(i){
    # A matter of slicing the shared data into chunks, and returning
    # those chunks
    shared %>% 
      # Slice the data according to the identifiers we made in the last part
      slice(windows$start_id[i]:windows$end_id[i]) %>% 
      select(variables) %>% 
      # Since the ripsDiag function requires a matrix object (not a tibble/df)
      as.matrix()
  })
  
  # Now we'll compute the max_scale to set for ripsDiag function
  # this must be standardized across the computation of every window in order
  # for a consistently measured Wasserstein distance. To ensure that every
  # structure is found for every window (we don't miss anything), we'll use the
  # maximum distance found in any such window between points.
  max_scale = max(
    sapply(window_datum, function(window){
      max(dist(window))
    })
  )
  
  message(paste0("Number of months in subsetted data: ", nrow(shared), "\n",
                 "Number of windows to be computed: ", length(start_indicies)))
  
  # Prior to computing each barcode, we'll start up our workers
  # plan() is a future function which defines the parallel strategy
  plan(multisession, workers = detectCores() - 2)
  # Use on.exit() so that the workers are reset once function finishes
  on.exit(plan(sequential))
  
  
  # with_progress allows for progress tracking
  with_progress({
    # Creates progress tracking object, defining how many steps to expect
    p = progressor(along = window_datum)
    # Use future_map for parallel processing
    diagrams = future_map(window_datum, function(window){
      # Each time a worker finishes, p() is called, advancing progress bar
      p()
      compute_diag_obj(window, max_dim = max_dimension, maxscale = max_scale)
    })
  })
  
  # Now assembling the returnable object
  return(
    windows %>% 
      select(id, start_date, end_date) %>% 
      mutate(
        diagram_obj = diagrams
      )
  )
}


##############################################################################
# Computing Wasserstein distance for each two windows
##############################################################################

# Computes Wasserstein distance for consecutive windows
# Takes the following arguments: diag_df - ouptut dataframe from other function
#                                max_dim - sets the dimensionality to be analyzed
# This function will default use all but 1 cores for working
#
# It will return a long-style dataframe containg the start and end dates for the
# two windows being analyzed, and the distance for each dimension

compute_wasserstein = function(barcode_df, max_d) {
  n_windows = nrow(barcode_df)
  
  # Setting up parallel environment
  plan(multisession, workers = detectCores() - 2)
  on.exit(plan(sequential))
  
  # Setting up progress tracking
  with_progress({
    # Telling it what it is keeping track of
    p = progressor(steps = n_windows - 1)
    # Filling the results dataframe
    # We use future_map_dfr, as it works well with the parallelism
    # and also automatically stacks our results into a dataframe
    # skips an rbind() call
    results = future_map_dfr(1:(n_windows-1), function(a){
      # Keeping track of progress
      p()
      # Create a dataframe for the distances
      # This will output adf with a column for dim
      # and another col for the distance
      wasserstein_by_dimension(barcode_df$diagram_obj[[a]],
                                           barcode_df$diagram_obj[[a+1]],
                                           max_dimension = max_d) %>% 
        # Now we'll append the date data
        mutate(
          window1_start = barcode_df$start_date[a],
          window1_end = barcode_df$end_date[a],
          window2_start = barcode_df$start_date[a+1],
          window2_end = barcode_df$end_date[a+1]
        )
    })
  })
  # We do some quick reformatting before returning the object
  return(
    results %>% 
      select(window1_start, window1_end, window2_start, window2_end,
             dimension, wasserstein_dist) %>%
      pivot_wider(
        names_from   = dimension,
        values_from  = wasserstein_dist,
        names_prefix = "dim"
      )
  )
}

##############################################################################
# Visualizing wasserstein distances versus recessions using dygraph
##############################################################################
#
# This function takes a dataframe of distances and plots them on a time-series
# with recessions shaded, so a cursory analysis of the predictive power can
# be assessed. Function also takes the full data so that recession dates
# can be obtained.
#
# The actual date that will be plotted is the end date of the second window
# being analyzed. This is done since one wouldn't be able to compute a new dist-
# ance if it were located anywhere else in the window (since they havent obser
# ed those times yet)
# The user can also edit the dimensions they want plotted

plot_distances = function(dist_data, full_data,dimensions = c("dim0", "dim1", "dim2", "dim3")){
  
  
  # Making sure that dist is sorted
  dist_data = dist_data %>% arrange(window2_end)
  
  # Subset to only the requested dimensions
  cols_to_plot = intersect(dimensions, names(dist_data))
  
  # We first convert the distances to an XTS object
  dists_xts = xts(
    # All of ensures that each
    select(dist_data, all_of(cols_to_plot)),
    # Order by determines how the dates wil be handled
    order.by = dist_data$window2_end
  )
  
  # Extracting the recession periods from the big data
  # rle() will detect sequences of 0s and 1s in the rec
  # ession data, making it easier to get the start and end
  # dates of the recession.
  
  full_data = full_data %>% arrange(date)
  runs = rle(full_data$recession)
  
  # Here we get the start and end dates for the recessions
  #
  # The rle object contains a list of 0 as and 1s, with another list
  # of lengths, or the length of the period of that number where it was
  # continuous
  # Taking the cumsum will add the lengths ontop of eachother so they
  # will track the ending index
  end_index   = cumsum(runs$lengths)
  
  # end_idx[-length(end_idx)] drops the last element of the end_index
  # and drops the last index so that it doesn't look for one after that
  # 
  # It then increases the list along by 1 (+1), and has 1 for the first 
  # distance measured
  start_index = c(1, end_index[-length(end_index)] + 1)
  
  
  recessions = tibble(
    # Creating a new tibble object with the value (0 or 1) and the start
    # and end date of that sequence of 0s and 1s
    is_recession = runs$values,
    start        = as.Date(full_data$date[start_index]),
    end          = as.Date(full_data$date[end_index])
  ) %>%
    # Now just grabbing the dates for the recessions
    filter(is_recession == 1)
  
  # Now we build the dygraph
  graph = dygraph(dists_xts, main = "Wasserstein Distance Over Time") %>%
    dyAxis("y", label = "Wasserstein Distance") %>%
    dyAxis("x", label = "Date") %>%
    dyOptions(colors = c("#002052", "#9fa7e8", "#b28c00", "#5e4300")[seq_along(cols_to_plot)], 
              strokeWidth = 1.5) %>%
    dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
    dyRangeSelector() %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2))
  # Add recession shading
  for(i in 1:nrow(recessions)){
    graph = graph %>%
      dyShading(
        from  = as.character(recessions$start[i]),
        to    = as.character(recessions$end[i]),
        color = "#DCDCDC"
      )
  }
  return(graph)
}





