# tnd-vaccine-effectiveness

For the moment everything is in the R folder.
Users can run portions of the main.R files to see BLCM for evaluating Vaccine Effectiveness in Test-Negative Design (TND). 

In R/functions.R we have two functions:
- sim_ve_imperfect_tests() for generating data of vaccine effectiveness and non-differential missclassification (max two tests).
- sim_ve_imperfect_tests_ diff() for generating data of vaccine effectiveness and differential missclassification (max two tests).

In R/blcm_models.R we stored BLCMs.
