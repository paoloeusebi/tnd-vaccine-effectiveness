# load packages -----------------------------------------------------------
list.of.packages <- c("tidyverse", "runjags", "rjags", "rootSolve", "parallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(runjags)
library(rjags)
library(rootSolve)
library(parallel)
testjags()


# run scripts -------------------------------------------------------------
source("R/functions.R") # functions for data simulations
source("R/models.R")    # Bayesian models
source("R/simulations_1t.R")    # simulations
source("R/simulations_summaries_plots.R")  # save simulations summaries and plots

# check R/examples.Rmd for interactive examples using functions and models