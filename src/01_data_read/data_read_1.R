############################## Read Data ####################################
# Input data set: ppathl 
# Output data set: ppathl_mig, ppathl_ref
# Aim: Read data set and keep samples of interest

# Load packages ------
library(dplyr)
library(haven)

# Define paths -------
path_data_soep <- "/Users/clarastrasser/soep_data/"
path_out <- "/Users/clarastrasser/soep_data/processed/"


# Load data ------------
# Data Set: ppathl 
# Rows: 1.285.062
# Columns: 45
ppathl <- read_dta(paste0(path_data_soep, "ppathl.dta"))


# Subset data ------

## Refugee data -----
# Aim: 
# Keep "pid" for refugee sample
# Samples:
# M3 - 17: 2016 Refugee (2013-2015)
# M4 - 18: 2016 Refugee/family (2013-2015)
# M5 - 19: 2017 Refugee (2013-2016)
# M6 - 24: 2020 Refugee
# Rows: 88.074
# Columns: 45
ppathl_subset_ref <- ppathl[ppathl$psample %in% c(17, 18, 19, 24), ]

# Vector length: 22.280
unique_pid_ref <- unique(ppathl_subset_ref$pid)


## Migration data -----
# Aim: 
# Keep "pid" for migration sample
# Samples:
# M1 - 15: 2013 Migration (1995-2011)
# M2 - 16: 2015 Migration (2009-2013)
# M7 - 25: 2020 Migration
# M8 - 26: 2020 Migration Professionals
# Rows: 76.064
# Columns: 45

ppathl_subset_mig <- ppathl[ppathl$psample %in% c(15, 16, 25, 26), ]

# Vector length: 17.069
unique_pid_mig <- unique(ppathl_subset_mig$pid)


# Save data -------

## pppathl_subset_ref ------
save(ppathl_subset_ref, file = paste0(path_out,"ppathl_subset_ref.RData"))
save(unique_pid_ref, file = paste0(path_out,"unique_pid_ref.RData"))

## ppathl_subset_mig -------
save(ppathl_subset_mig, file = paste0(path_out,"ppathl_subset_mig.RData"))
save(unique_pid_mig, file = paste0(path_out,"unique_pid_mig.RData"))






