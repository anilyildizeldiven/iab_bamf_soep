############################## 2 - Prepare Data ####################################
# Refugees and Employment
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
path_data_processed_ref <- file.path(path_data_processed, "refugee")
path_out <- path_data_processed_ref

# Load data ------

## data_prep_1 -----
# Rows: 88.074
load(file.path(path_data_processed_ref, "data_prep_1.RData"))

## pl_subset_ref ------
# Rows: 25.451
load(file.path(path_data_processed, "pl_subset_ref.RData"))

## biol -------
# Rows: 130.429
biol <- read_dta(file.path(path_data_soep, "biol.dta"))


# Keep relevant columns -------

## pl_subset_ref ------
# Rows: 25.451
# Relevant questions:
# plb0022_h: Are you currently employed? 

pl_variables <- c("pid", "hid", "syear",
                  "plb0022_h")


pl_subset <- pl_subset_ref[,pl_variables]

## biol ------
# Rows: 130.429
# lm0607i01 - In which year were you employed in Germany at your first job?
# lr2098 - In which year were you in your first job in Germany?
# lr2110 - Have you ever been employed in Germany?



biol_variables <- c("pid", "hid", "syear",
                    "lr2110", "lr2098")

biol_subset <- biol[ , biol_variables]


# Merge data ------
# input: data_prep_1, biol_subset, pl_subset
# output: data_prep_2
# Rows: 88.074
# Columns: 33

data_prep_2 <- data_prep_1 %>%
  left_join(pl_subset, by = c("pid", "hid", "syear")) %>%
 left_join(biol_subset, by = c("pid", "hid", "syear"))



# Save data ------

# data_prep_2
save(data_prep_2, file = file.path(path_out, "data_prep_2.RData"))





















