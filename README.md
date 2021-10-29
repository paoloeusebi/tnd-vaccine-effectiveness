# tnd-vaccine-effectiveness

Users can run all the analyses within the main.R script in the R folder.

Users can run portions of the main.R files to see BLCM for evaluating Vaccine Effectiveness in Test-Negative Design (TND). 

In the [R](R) folder there are the following scripts (tasks):
- [functions.R](R/functions.R) (for simulating data under different scenarios of test(s) accuracy, vaccine effectiveness, sample size.
- [models.R](R/models.R) (Bayesian models for 1 imperfect test and Bayesian Latent Class Models for two imperfect tests).
- [simulations_1_test.R](R/simulations_1_test.R) (for running simulations with 1 imperfect test)
- [simulations_2_tests.R](R/simulations_2_tests.R) (for running simulations with 2 imperfect tests)
- [simulations_summaries_plots.R](R/simulations_summaries_plots.R) (for reading simulations results / create summaries and plots)
- [examples.Rmd](R/examples.Rmd) (using functions.R and models.R for testing the approach with interactive examples)

In the [tfls](tfls) folder users can find summaries/plots of simulations

