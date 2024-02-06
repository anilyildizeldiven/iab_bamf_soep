############################## 1 - Prepare Data ####################################
# Refugees and Location
# Input data set: ppathl_subset_ref, hbrutto
# Output data set: data_prep_1
# Aim: Merge location data from hbrutto to ppathl_subset_ref data 
# 

# Load packages ------
library(dplyr)
library(haven)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_soep_raw <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/raw/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/"
path_out <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep_project/soepdata/processed/refugee/"


# Load data ------------

## ppathl_subset_ref -----
# Rows: 88.074
# Columns: 45
load(paste0(path_data_processed, "ppathl_subset_ref.RData"))


## hbrutto -------
# Rows: 501.622
# Columns: 102
hbrutto <- read_dta(paste0(path_data_soep, "hbrutto.dta"))


## bioresidrefinG -----
# Remarks: starting out of 2019
# Rows:
# Columns:
bioresidrefinG <- read_dta(paste0(path_data_soep_raw, "bioresidrefinG.dta"))



# Keep relevant columns -------


## ppathl_subset_ref -----
# Keep only variables of interest
# Rows: 88.074
# Columns: 18
ppathl_variables <- c("pid", "hid", "parid",
                      "syear","eintritt", "austritt",
                      "erstbefr", "letztbef", "immiyear",
                      "sex", "gebjahr", "gebmonat",
                      "partner", "psample","migback",
                      "arefback","germborn", "corigin")

ppathl_subset_ref <- ppathl_subset_ref[,ppathl_variables]

## hbrutto ------
# Keep only variables of interest
# Rows: 501.622
# Columns:  11
hbrutto_variables <- c("hid", "syear", "bula_h", "bula_v1", "bula_v2", 
                       "wein_v1", "wein_v2", "wein_v3",
                       "hader_v1", "hader_v2", "hadq")


hbrutto_subset <- hbrutto[,hbrutto_variables]

## hl -----
#
#



# Merge data ------
# Input: ppath_subset_ref and hbrutto_subset
# Output: data_prep_1
# Rows: 88.074
# Columns: 27

data_prep_1 <- ppathl_subset_ref %>%
  left_join(hbrutto_subset, by = c("hid", "syear"))


# Save data ------

# data_prep_1
save(data_prep_1, file = paste0(path_out,"data_prep_1.RData"))








