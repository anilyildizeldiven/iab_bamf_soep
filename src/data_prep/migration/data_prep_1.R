############################## 1 - Prepare Data ####################################
# Migrants and Location
# Input data set: ppathl_subset_mig, hbrutto, biol, regionl, bioregionl
# Output data set: data_prep_1
# Aim: Merge location data (federal state information to hid and pid)


# Load packages ------
library(dplyr)
library(haven)

# Define base path ---------
base_path <- "C:/Users/ru23kek/Desktop/projects/"

# Define paths --------
path_data_soep <- file.path(base_path, "data", "soepdata")
path_data_soep_raw <- file.path(path_data_soep, "raw")
path_data_processed <- file.path(base_path, "iab_bamf_soep", "soepdata", "processed")
path_out <- file.path(path_data_processed, "migrants")

# Load data -------

## ppathl_subset_mig -----
# Rows: 76.064
load(file.path(path_data_processed, "ppathl_subset_mig.RData"))

## hbrutto -------
# Rows: 501.622
hbrutto <- read_dta(file.path(path_data_soep, "hbrutto.dta"))

## biol -------
# Rows: 130.429
biol <- read_dta(file.path(path_data_soep, "biol.dta"))

## regionl -----
# Rows: 652.238
regionl <- read_dta(file.path(path_data_soep, "regionl.dta"))

## bioregion ----
# Rows: 68.489
bioregion <- read_dta(file.path(path_data_soep_raw, "bioregion.dta"))



# Keep relevant columns -------


## ppathl_subset_ref -----
# Keep only variables of interest
# Rows: 76.064
ppathl_variables <- c("pid", "hid", "parid",
                      "syear","eintritt", "austritt",
                      "erstbefr", "letztbef", "immiyear",
                      "sex", "gebjahr", "gebmonat",
                      "partner", "psample","migback",
                      "arefback","germborn", "corigin")

ppathl_subset_mig <- ppathl_subset_mig[,ppathl_variables]


## biol -----
# Keep only variables of interest
# lb1436_h: First residence in Germany (2019-2021)
biol_variables <- c("pid","hid", "syear",
                     "lb1436_h")

biol_subset <- biol[, biol_variables]

## regionl ----
# Keep only variables of interest
# bula: Federal state of the household in the year of survey
regionl_variables <- c("hid", "syear", "bula")

regionl_subset <- regionl[, regionl_variables]


## bioregion -----
# Keep only variable of interest
# place_bula: Federal state location of the person
# place_type: Type of location
bioregion_variables <- c("pid", "syear", "place_bula",
                         "place_type")

bioregion_subset <- bioregion[, bioregion_variables]

# Check duplicates
# Duplicates: 23814
duplicates <- bioregion_subset %>%
  group_by(pid,syear) %>%
  filter(n()>1)

# Remove duplicates
# 56582
bioregion_subset <- bioregion_subset %>%
  distinct(pid, syear, .keep_all = TRUE)


# Merge data ------
# Input: ppath_subset_mig and hbrutto_subset
# Output: data_prep_1
# Rows: 76.064

data_prep_1 <- ppathl_subset_mig %>%
  left_join(biol_subset, by = c("pid", "hid", "syear")) %>%
  left_join(regionl_subset,  by = c("hid", "syear")) %>%
  left_join(bioregion_subset, by = c( "pid", "syear"))


# Save data ------

# data_prep_1
save(data_prep_1, file = file.path(path_out, "data_prep_1.RData"))



