# Load in Libraries.

library(haven)
library(survey)
library(tidyverse)
library(margins)
library(forcats)
library(stargazer)
library(aod)
library(xgboost)
library(gtsummary)
library(xtable)
library(data.table)
library(ggmosaic)

# Source Utility Functions
source("Code/UtilityFunctions.R")

# Run Analysis Scripts 
source("Code/00_data_cleaning.R")     # Cleans data and drops unused levels
source("Code/01_pooled_logit.R")      # Run full pooled regression
source("Code/02_logits_by_partyid.R") # Run party-by-party specification
source("Code/03_wald.R")              # Compute Wald statistics to show party-interacted coefs are not jointly zero, 
                                      # implying our disaggregated regression is informative  
source("Code/04_counterfactual.R")    # Compute counterfactuals against 2020 benchmark
source("Code/05_tables.R")            # Only Table.E13
source("Code/06_figure_creation.R")   # Create all paper figures
source("Code/07_secondary_counterfactual.R")   # Create all paper figures


## Recoded

source("Code/01_pooled_logit_recode.R")      # Run full pooled regression
source("Code/02_logits_by_partyid_recode.R") # Run party-by-party specification
source("Code/04_counterfactual_recode.R")    # Compute counterfactuals against 2020 benchmark
source("Code/06_figure_creation_recode.R")   # Create all paper figures
