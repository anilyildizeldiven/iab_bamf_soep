############################## 1 - Prepare Data ####################################
# Refugees and Location
# Input data set: ppathl_subset_ref, hbrutto, biol, regionl, bioregionl
# Output data set: data_prep_1
# Aim: Merge location data (federal state information to hid and pid)


# Load packages ------
library(dplyr)
library(haven)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_soep_raw <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/raw/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/"
path_out <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/refugee/"


# Load data ------------

## ppathl_subset_ref -----
# Rows: 88.074
# Columns: 45
load(paste0(path_data_processed, "ppathl_subset_ref.RData"))


## hbrutto -------
# Rows: 501.622
# Columns: 102
hbrutto <- read_dta(paste0(path_data_soep, "hbrutto.dta"))


## biol -------
# Rows: 130.429
# Columns: 3579
biol <- read_dta(paste0(path_data_soep, "biol.dta"))


## regionl -----
# Rows: 652.238
# Columns: 36
regionl <- read_dta(paste0(path_data_soep, "regionl.dta"))

## bioregion ----
# Rows: 68.489
# Columns: 94
bioregion <- read_dta(paste0(path_data_soep_raw, "bioregion.dta"))



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

## biol -----
# Keep only variables of interest
biol_variables <- c("pid","hid", "syear",
                    "lr3367_h", "lr3235")

biol_subset <- biol[, biol_variables]

## regionl ----
# Keep only variables of interest
regionl_variables <- c("hid", "syear", "bula")

regionl_subset <- regionl[, regionl_variables]


## bioregion -----
# Keep only variable of interest
bioregion_variables <- c("pid", "syear", "place_type")

bioregion_subset <- bioregion[, bioregion_variables]

# Check duplicates
# Duplicates: 23914
duplicates <- bioregion_subset %>%
  group_by(pid,syear) %>%
  filter(n()>1)

# Remove duplicates
# Rows: 44.675
bioregion_subset <- bioregion_subset %>%
  anti_join(duplicates, by = c("pid", "syear"))


# Merge data ------
# Input: ppath_subset_ref and hbrutto_subset
# Output: data_prep_1
# Rows: 88.074
# Columns: 27

data_prep_1 <- ppathl_subset_ref %>%
  left_join(biol_subset, by = c("pid", "hid", "syear")) %>%
  left_join(regionl_subset,  by = c("hid", "syear")) %>%
  left_join(bioregion_subset, by = c( "pid", "syear"))


# Save data ------

# data_prep_1
save(data_prep_1, file = paste0(path_out,"data_prep_1.RData"))





