############################## Read Data ####################################
# Input data set: pl 
# Output data set: pl_subset_mig, pl_subset_ref
# Aim: Read data set and keep samples of interest

# Load packages ------
library(dplyr)
library(haven)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep_project/soepdata/processed/"
path_out <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep_project/soepdata/processed/"


# Load data ------------

## pl -------
# Rows: 763.223
# Columns: 5157
pl <- read_dta(paste0(path_data_soep, "pl.dta"))

## ppathl vector ----

# Refugees
load(paste0(path_data_processed, "unique_pid_ref.RData"))

# Migrants
load(paste0(path_data_processed, "unique_pid_mig.RData"))

# Subset data ------

## Refugee data -----
# Aim: 
# Keep "pid" for refugee sample
# Samples:
# M3 - 17: 2016 Refugee (2013-2015)
# M4 - 18: 2016 Refugee/family (2013-2015)
# M5 - 19: 2017 Refugee (2013-2016)
# M6 - 24: 2020 Refugee
# Rows: 25.451
# Columns: 5157
pl_subset_ref <- pl[pl$pid %in% unique_pid_ref, ]


## Migration data -----
# Aim: 
# Keep "pid" for migration sample
# Samples:
# M1 - 15: 2013 Migration (1995-2011)
# M2 - 16: 2015 Migration (2009-2013)
# M7 - 25: 2020 Migration
# M8 - 26: 2020 Migration Professionals
# Rows: 32.737
# Columns: 5157
pl_subset_mig <- pl[pl$pid %in% unique_pid_mig, ]


# Save data -------

## pl_subset_ref ------
save(pl_subset_ref, file = paste0(path_out,"pl_subset_ref.RData"))

## pl_subset_mig -------
save(pl_subset_mig, file = paste0(path_out,"pl_subset_mig.RData"))







