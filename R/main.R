# load packages -----------------------------------------------------------
list.of.packages <- c("tidyverse", "runjags", "rjags", "rootSolve")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(runjags)
library(rjags)
library(rootSolve)
testjags()


# run scripts -------------------------------------------------------------
source("R/functions.R") # functions for data simulations
source("R/models.R")    # Bayesian models
source("R/simulations_1_test.R")    # simulations
source("R/simulations_2_tests.R")  
source("R/simulations_summaries_plots.R")  # save simulations summaries and plots
