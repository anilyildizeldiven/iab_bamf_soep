############################## 2 - Prepare Data ####################################
# Refugees and Employment
# Input data set: data_prep_1, pl_subset_ref, biol
# Output data set: data_prep_2
# Aim: Merge employment outcome of interest


# Load packages ------
library(dplyr)
library(haven)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_soep_raw <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/raw/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/"
path_data_processed_ref <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/refugee/"
path_out <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/refugee/"

# Load data ------------

## data_prep_1 -----
# Rows: 88.074
# Columns: 23
load(paste0(path_data_processed_ref, "data_prep_1.RData"))

## pl_subset_ref ------
# Rows: 25.451
# Columns: 5157
load(paste0(path_data_processed, "pl_subset_ref.RData"))

## biol -------
# Rows: 130.429
# Columns: 3579
biol <- read_dta(paste0(path_data_soep, "biol.dta"))


# Keep relevant columns -------

## biol ------
# Rows: 130.429
# Columns: 6
# Relevant questions:
# lr2110 - Have you ever been employed in Germany?
# Answer options:
# 1 - yes
# 2 - no
# -2 - does not apply
# -5 - not included in version
# -8 - question not included in the year
# lr2098 - In which year were you in your first job in Germany?
# Answer options:
# Year
# -5 / -8 - not included in the version


biol_variables <- c("pid", "hid", "syear",
                    "lr2110", "lr2098", "lr2099")

biol_subset <- biol[ , biol_variables]



## pl_subset_ref ------
# Rows: 25.451
# Columns: 
# Relevant questions:
# plb0022_h: Are you currently employed? 
# Answer options:
# 1 - full employment
# 2 - part-time
# 3 - ausbildung
# 4 - geringfügig
# 5 - altersteilzeit
# 6 - wehrdienst
# 7 - FSJ
# 8 - Werkstatt für Behinderte
# 9 - no
# -1 - not known

pl_variables <- c("pid", "hid", "syear",
                  "plb0022_h")


pl_subset <- pl_subset_ref[,pl_variables]


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
save(data_prep_2, file = paste0(path_out,"data_prep_2.RData"))





















