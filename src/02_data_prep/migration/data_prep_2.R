############################## 2 - Prepare Data ####################################
# Migrants and Integration Outcomes
# Input data set: data_prep_1, pl_subset_ref, biol
# Output data set: data_prep_2
# Aim: Merge integration outcomes of interest


# Load packages ------
library(dplyr)
library(haven)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/"
path_data_soep <- file.path(base_path, "soep_data", "data")
path_data_soep_raw <- file.path(path_data_soep, "raw")
path_data_processed <- file.path(base_path, "soep_data", "processed")
path_data_processed_mig <- file.path(path_data_processed, "migrants")
path_out <- path_data_processed_mig

# Load data --------------------------------------------------------------------

## data_prep_1 -----
# Rows: 76.064
load(file.path(path_data_processed_mig, "data_prep_1.RData"))

## pl_subset_mig ------
# Rows: 32.737
load(file.path(path_data_processed, "pl_subset_mig.RData"))

## biol -------
# Rows: 130.429
biol <- read_dta(file.path(path_data_soep, "biol.dta"))

## biojob -----
# Rows: 96.177
biojob <- read_dta(file.path(path_data_soep, "biojob.dta"))


# EMPLOYMENT -------------------------------------------------------------------
# Employment year

## pl_subset_mig ------
# Rows: 32.737
# Relevant questions:
# plb0022_h: Are you currently employed? (multiple answers)

empl_pl_variables <- c("pid", "hid", "syear",
                  "plb0022_h")

empl_pl_subset <- pl_subset_mig[,empl_pl_variables]


## biol ------
# Rows: 130.429
# Relevant questions:
# lb0265 - Are you currently employed? (yes/no answer)
# lm0607i01 - In which year were you employed in Germany at your first job? (2016-2018)
# lb1421 - Have you ever been employed in Germany? (2019-2021)
# lm0607i02 - Never employed (2016-2018)


empl_biol_variables <- c("pid", "hid", "syear",
                    "lb0265", "lm0607i01", "lm0615i01", "lb1421", "lm0607i02", "lm0610_h",
                    "lr2098", "lb0266_h", "lb1362", "lb1360", "lb1359", "lb0165")

empl_biol_subset <- biol[ , empl_biol_variables]

## biojob -----
# Rows: 96.177
# einstieg_artk
empl_biojob_variables <- c("cid","pid", "einstieg_artk","einstieg_pbio", "stillfj", "yearlast", "occmove")

empl_biojob_subset <- biojob[ , empl_biojob_variables]

# LANGUAGE ---------------------------------------------------------------------

# Attended German language course
#lan_biol_variables <- c("lm0131_v1", "lm0131_v2")

#lan_biol_subset <- biol[ , lan_biol_variables]


# SOCIAL -----------------------------------------------------------------------
# Spending time with Germans

## pl_subset_mig ------
# Rows: 32.737
# Relevant questions:
# plj0569: Spending time with Germans 
#time_pl_variables <- c("plj0569")


#time_pl_subset <- pl_subset_mig[,time_pl_variables]

# Merge data -------------------------------------------------------------------
# input: data_prep_1, biol_subset, pl_subset
# output: data_prep_2
# Rows: 76.064

data_prep_2 <- data_prep_1 %>%
  left_join(empl_pl_subset, by = c("pid", "hid", "syear")) %>%
  left_join(empl_biol_subset, by = c("pid", "hid", "syear")) %>%
  left_join(empl_biojob_subset, by = c("pid"))
  



# Save data --------------------------------------------------------------------

# data_prep_2
save(data_prep_2, file = file.path(path_out, "data_prep_2.RData"))





















