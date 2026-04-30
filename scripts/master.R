################################################################################
# INSTRUCTIONS FOR USE:
#  If you can, Please open the R Project file (.Rproj) to automatically set the working directory.
#  If you are not using the .Rproj file, uncomment the line below and set your 
#    working directory to the root folder of this project (where the .proj is ).
#
# setwd("PATH/TO/PROJECT/ROOT/FOLDER")
################################################################################

################################################################################
# PHASE 1: DATA PROCUREMENT & ASSEMBLY
# YOU NEED TO GET AN API KEY FOR GOOGLE TRENDS, INSTRUCTIONS ARE CONTAINED IN THE
# raw_data_procurement FILE. WITH THAT SAID, YOU CAN ALSO RUN IT WITHOUT, AND THE
# GOOGLE TRENDS OBTAINING PROCEDURE WILL SIMPLY PULL THE LAST FOUND DATA FROM THE
# DATA FOLDER, AND LET YOU KNOW WHAT HAPPENED.
################################################################################

message("Step 1/5: Procuring raw data from APIs and libraries")
source("scripts/raw_data_procurement.R")

message("Step 2/5: Merging raw datasets")
source("scripts/merge_data.R")

message("Step 3/5: Cleaning merged data (handling NAs, formatting and what not)")
source("scripts/clean_data.R")

message("Step 4/5: Altering, scaling, and engineering target variables")
source("scripts/alter_data.R")


################################################################################
# PHASE 2: TOPOLOGICAL MACHINE LEARNING PIPELINE
################################################################################
# Note that PH_functions.R and prediction_functions.R are sourced automatically 
# inside Predictive_Analysis.R, so they do not need to be called here
# IF YOU DELETE THE SAVED OUTPUT FROM THIS DATA, THIS WILL TAKE A LONG TIME
# ADDITIONALLY, YOUR COMPUTER MAY CRASH DUE TO THE PARALLELIZATION NEEDS
# ENSURE THAT YOU HAVE ENOUGH COMPUTATION AVAILABLE (DONT HAVE A MILLION TABS OPEN)
message("Step 5/5: Running TDA and LASSO Predictive Models...")

source("scripts/Predictive_Analysis.R")

# If you want to see the success of the ML process, look at the "results_spec_...."
# dataframes in the output currently, they will 
################################################################################
# EXPLORATORY DATA ANALYSISes
################################################################################
# The scripts below generate the static and interactive visualizations used in 
# the final R Markdown report. They are not strictly required to reproduce the 
# datasets or the machine learning output. 
#
# To regenerate all report visuals, uncomment and run the lines below:
#
# source("scripts/MacroVars_EDA.R")
# source("scripts/visualizations.R")