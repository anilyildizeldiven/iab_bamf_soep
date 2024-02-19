############################## 2 - Prepare Data ####################################
# Migrants and Employment
# Input data set: data_prep_1, pl_subset_ref, biol
# Output data set: data_prep_2
# Aim: Merge employment outcome of interest


# Load packages ------
library(dplyr)
library(haven)

# Base path ------
base_path <- "C:/Users/ru23kek/Desktop/projects/"

# Define paths ------
path_data_soep <- file.path(base_path, "data", "soepdata")
path_data_soep_raw <- file.path(path_data_soep, "raw")
path_data_processed <- file.path(base_path, "iab_bamf_soep", "soepdata", "processed")
path_data_processed_mig <- file.path(path_data_processed, "migrants")
path_out <- path_data_processed_mig

# Load data ------

## data_prep_1 -----
# Rows: 76.064
load(file.path(path_data_processed_mig, "data_prep_1.RData"))

## pl_subset_mig ------
# Rows: 32.737
load(file.path(path_data_processed, "pl_subset_mig.RData"))

## biol -------
# Rows: 130.429
biol <- read_dta(file.path(path_data_soep, "biol.dta"))


# Keep relevant columns -------

## pl_subset_mig ------
# Rows: 25.451
# Relevant questions:
# plb0022_h: Are you currently employed? (multiple answers)

pl_variables <- c("pid", "hid", "syear",
                  "plb0022_h")

pl_subset <- pl_subset_mig[,pl_variables]


## biol ------
# Rows: 130.429
# Relevant questions:
# lb0265 - Are you currently employed? (yes/no answer)
# lm0607i01 - In which year were you employed in Germany at your first job? (2016-2018)
# lb1421 - Have you ever been employed in Germany? (2019-2021)
# lm0607i02 - Never employed (2016-2018)


biol_variables <- c("pid", "hid", "syear",
                    "lb0265", "lm0607i01", "lb1421", "lm0607i02")

biol_subset <- biol[ , biol_variables]



# Merge data ------
# input: data_prep_1, biol_subset, pl_subset
# output: data_prep_2
# Rows: 76.064

data_prep_2 <- data_prep_1 %>%
  left_join(pl_subset, by = c("pid", "hid", "syear")) %>%
  left_join(biol_subset, by = c("pid", "hid", "syear"))



# Save data ------

# data_prep_2
save(data_prep_2, file = file.path(path_out, "data_prep_2.RData"))





















