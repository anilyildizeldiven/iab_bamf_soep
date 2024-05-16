############################## 2 - Prepare Data ####################################
# Refugees and Integration Outcomes
# Input data set: data_prep_1, pl_subset_ref, biol
# Output data set: data_prep_2
# Aim: Merge integration outcomes of interest


# Load packages ----------------------------------------------------------------
library(dplyr)
library(haven)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/"
path_data_soep <- file.path(base_path, "soep_data", "data")
path_data_soep_raw <- file.path(path_data_soep, "raw")
path_data_processed <- file.path(base_path, "soep_data", "processed")
path_data_processed_ref <- file.path(path_data_processed, "refugee")
path_out <- path_data_processed_ref

# Load data --------------------------------------------------------------------

## data_prep_1 -----
# Rows: 88.074
load(file.path(path_data_processed_ref, "data_prep_1.RData"))

## pl_subset_ref ------
# Rows: 25.451
load(file.path(path_data_processed, "pl_subset_ref.RData"))

## biol -------
# Rows: 130.429
biol <- read_dta(file.path(path_data_soep, "biol.dta"))


# EMPLOYMENT ------------------------------------------------------------------
# Employment year 

## pl_subset_ref ------
# Rows: 25.451
# Relevant questions:
# plb0022_h: Are you currently employed? 

empl_pl_variables <- c("pid", "hid", "syear",
                  "plb0022_h")


empl_pl_subset <- pl_subset_ref[,empl_pl_variables]

## biol ------
# Rows: 130.429
# lm0607i01 - In which year were you employed in Germany at your first job?
# lr2098 - In which year were you in your first job in Germany?
# lr2110 - Have you ever been employed in Germany?


empl_biol_variables <- c("pid", "hid", "syear",
                    "lr2110", "lr2098")

empl_biol_subset <- biol[ , empl_biol_variables]


# INT - LANGUAGE ---------------------------------------------------------------
# Hours learning German



# INT - SOCIAL -----------------------------------------------------------------
# Spending time with Germans

## pl_subset_ref ------
# Rows: 25.451
# Relevant questions:
# plj0569: Spending time with Germans 
time_pl_variables <- c("plj0569")


time_pl_subset <- pl_subset_ref[,time_pl_variables]


# Merge data -------------------------------------------------------------------
# input: data_prep_1, biol_subset, pl_subset
# output: data_prep_2
# Rows: 88.074
# Columns: 33

data_prep_2 <- data_prep_1 %>%
  left_join(pl_subset, by = c("pid", "hid", "syear")) %>%
 left_join(biol_subset, by = c("pid", "hid", "syear"))



# Save data --------------------------------------------------------------------

# data_prep_2
save(data_prep_2, file = file.path(path_out, "data_prep_2.RData"))





















