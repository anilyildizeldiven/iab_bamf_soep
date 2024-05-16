############################ Clean Data ######################################
# Input data set: 
# data_prep
# Output data set:
# data_clean
# Aim: Create final variables 
# 01 - Federal State of first residence
# 02 - Employment 
# 03 - Individual-level characteristics

# Load packages ----------------------------------------------------------------
library(data.table)
library(dplyr)
library(tidyr)
library(haven)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/"
path_data_soep <- file.path(base_path, "soep_data", "data")
path_data_soep_raw <- file.path(base_path, "soep_data", "raw")
path_data_processed <- file.path(base_path, "soep_data", "processed")

# Load data --------------------------------------------------------------------

# data_prep
# Rows: 164.138
load(file.path(path_data_processed, "data_prep.RData"))

# Rename data ------------------------------------------------------------------

data <- data_prep

# Subset Data ------------------------------------------------------------------

# Keep only those that were not born in Germany
# germborn == 2
# Rows: 125.009
data <- data  %>%
  subset(germborn == 2)

# Keep those with direct migration background
# migback == 2
# Rows: 78.346
data <- data  %>%
  subset(migback == 2)

# Keep only those with direct refugee experience
# arefback == 2
# Rows: 
#data <- data  %>%
  #subset(arefback == 2)

# Keep only if first interview and last interview information not missing
# Rows: 81.230
data <- data  %>%
  subset(erstbefr != -2 & letztbef != -2)

# Keep only if corigin known
# corigin != -1
# Rows: 81.185
data <- data  %>%
  subset(corigin != -1)

# Keep only if year of immigration not missing
# !is.na(immiyear) & immiyear >-1
# Rows: 80.183
data <- data  %>%
  subset(!is.na(immiyear) & immiyear >-1)


# Rename columns ---------------------------------------------------------------

data <- data %>%
  rename(first_int = erstbefr,
         last_int = letztbef,
         birth_year = gebjahr,
         birth_month = gebmonat,
         first_residence_mig = lb1436_h,
         first_residence_ref = lr3367_h,
         longest_residence = lr2074_h,
         number_residences = lr3235,
         employment_now = plb0022_h,
         ever_employed_ger_v1 = lb1421,
         never_employed_ger = lm0607i02,
         ever_employed_ger_v2 = lr2110,
         first_employment_age = lr3031,
         never_employed_life = lr3032,
         never_employed_home_country_v1 = lr3033_h,
         never_employed_home_country_v2 = lr3033_v1,
         employed_home_country = lm0626,
         year_first_employment_v1 = lm0607i01,
         year_first_employment_v2 = lm0615i01,
         year_first_employment_v3 = lr2098,
         year_last_employment = lb0266_h,
         religious_affiliation = plh0258_h,
         marital_status = pld0131_h,
         german_speaking = lm0128i01,
         german_writing = lm0128i02,
         german_reading = lm0128i03,
         school_degree_v1 = lr3079,
         school_degree_v2 = lb0188,
         school_years = lb0187,
         vocational_training = lb0228)

# Create columns ---------------------------------------------------------------

## 01 - FEDERAL STATE OF FIRST RESIDENCE ---------------------------------------
# Aim: create bula_res
# How:
# bula_res == first_residence_ref if first_residence_ref >= 1 or
# bula_res == first_residence_mig if first_residence_mig >= 1 or
# bula_res == longest_residence if longest_residence >= 1 or
# bula_res == place_bula if place_bula >= 1 or
# bula_res == bula if is.na(bula_res) & number_residences %in% c(1)

# bula_res == first_residence_ref if first_residence_ref >= 1 or
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(first_residence_ref >= 1, first_residence_ref, NA_real_ )) %>%
  fill(bula_res, .direction = "downup") 

# bula_res == first_residence_mig if first_residence_mig >= 1 or
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(first_residence_mig >= 1, first_residence_mig, bula_res )) %>%
  fill(bula_res, .direction = "downup") 

# bula_res == longest_residence if longest_residence >= 1 or
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(is.na(bula_res) & longest_residence >= 1, longest_residence, bula_res)) %>%
  fill(bula_res, .direction = "downup") 

# bula_res == place_bula if place_bula >= 1 or
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(is.na(bula_res) & place_bula >= 1, place_bula, bula_res)) %>%
  fill(bula_res, .direction = "downup") 

# bula_res == bula if is.na(bula_res) & number_residences %in% c(1)
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(is.na(bula_res) & number_residences %in% c(1), bula, bula_res))  %>%
  fill(bula_res, .direction = "downup") 


## 02 - EMPLOYMENT -------------------------------------------------------------
# Aim: create bula_res
# How:

















