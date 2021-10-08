# load packages -----------------------------------------------------------
library(tidyverse)
library(runjags)
library(rjags)
library(rootSolve)
testjags()


# run scripts -------------------------------------------------------------
source("R/functions.R") # functions for data simulations
source("R/models.R")    # Bayesian models
source("R/simulations_1_test.R")    # simulations

