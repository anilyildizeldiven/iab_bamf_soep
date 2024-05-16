############################## Read Data ######################################
# Input data set: 
# 1.) Individual Tracking File: ppathl
# 2.) Data individual questionnaire: pl
# Output data set:
# 1.) ppathl_ref_mig
# 2.) pl_ref_mig
# Aim: Read data set and keep samples of interest

# Load packages ----------------------------------------------------------------
library(dplyr)
library(haven)

# Define paths -----------------------------------------------------------------
path_data_soep <- "/Users/clarastrasser/soep_data/data/"
path_out <- "/Users/clarastrasser/soep_data/processed/"


# Load data --------------------------------------------------------------------

# Data Set: ppathl 
# Rows: 1.285.062
# Columns: 45
ppathl <- read_dta(paste0(path_data_soep, "ppathl.dta"))

## pl -------
# Rows: 763.223
# Columns: 5157
pl <- read_dta(paste0(path_data_soep, "pl.dta"))


# Subset data  -----------------------------------------------------------------

## ppathl data -----
# Aim: 
# Keep "pid" for refugee and migration sample
# Refugee Sample:
# M3 - 17: 2016 Refugee (2013-2015)
# M4 - 18: 2016 Refugee/family (2013-2015)
# M5 - 19: 2017 Refugee (2013-2016)
# M6 - 24: 2020 Refugee
# Migration Sample
# M1 - 15: 2013 Migration (1995-2011)
# M2 - 16: 2015 Migration (2009-2013)
# M7 - 25: 2020 Migration
# M8 - 26: 2020 Migration Professionals

# Rows: 164.138
# Unique Rows: 39.349
# Columns: 45
ppathl_ref_mig <- ppathl[ppathl$psample %in% c(17, 18, 19, 24, 15, 16, 25, 26), ]
unique_pid_ref_mig <- unique(ppathl_ref_mig$pid)

## pl data -----
# Aim: 
# Keep "pid" for refugee and migration sample

# Rows: 58.188
# Columns: 5157
pl_ref_mig <- pl[pl$pid %in% unique_pid_ref_mig, ]


# Save data --------------------------------------------------------------------

save(ppathl_ref_mig, file = paste0(path_out,"ppathl_ref_mig.RData"))
save(pl_ref_mig, file = paste0(path_out,"pl_ref_mig.RData"))


