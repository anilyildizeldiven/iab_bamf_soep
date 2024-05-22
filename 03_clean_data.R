############################ Clean Data ######################################
# Input data set: 
# data_prep
# Output data set:
# data_clean
# Aim: Create variables 
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
# Rows: 125.009
data <- data  %>%
  subset(migback == 2)

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
         german_speaking_v1 = lm0128i01,
         german_writing_v1 = lm0128i02,
         german_reading_v1 = lm0128i03,
         school_country_v1 = lb0186_v1,
         school_country_v2 = lb0186_v2,
         school_country_v3 = lb0186_v3,
         school_degree_v1 = lr3079,
         school_degree_v2 = lb0188,
         school_degree_v3 = lb0191,
         school_years_v1 = lb0187,
         vocational_training_v1 = lb0228)

# Create columns ---------------------------------------------------------------

## 01 - FEDERAL STATE OF FIRST RESIDENCE ---------------------------------------
# Aim: create bula_res
# bula_res: Federal state of first residence

# bula_res == first_residence_ref if first_residence_ref >= 1 or
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(first_residence_ref >= 1, first_residence_ref, NA_real_ )) %>%
  fill(bula_res, .direction = "downup") 

# bula_res == first_residence_mig if first_residence_mig >= 1 or
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse((is.na(bula_res) & !is.na(first_residence_mig) & first_residence_mig >= 1), first_residence_mig, bula_res)) %>%
  fill(bula_res, .direction = "downup") 


# bula_res == longest_residence if longest_residence >= 1 or
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse((is.na(bula_res) & !is.na(longest_residence) & longest_residence >= 1), longest_residence, bula_res)) %>%
  fill(bula_res, .direction = "downup") 

# bula_res == place_bula if place_bula >= 1 or
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse((is.na(bula_res) & !is.na(place_bula) & place_bula >= 1), place_bula, bula_res)) %>%
  fill(bula_res, .direction = "downup") 

# bula_res == bula if is.na(bula_res) & number_residences %in% c(1)
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse((is.na(bula_res) & !is.na(number_residences) & number_residences %in% c(1)), bula, bula_res))  %>%
  fill(bula_res, .direction = "downup") 

# bula_res == bula if is.na(bula_res) & syear - immiyear <= 1
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse((is.na(bula_res) & syear - immiyear <= 1), bula, bula_res))  %>%
  fill(bula_res, .direction = "downup") 

#data_test <- data %>%
  #mutate(pid = as.integer(pid),
         #syear = as.integer(syear)) %>%
  #select(c("pid", "bula_res","first_residence_ref", "first_residence_mig", "longest_residence", "number_residences",
           #"bula","place_bula", "syear", "first_int", "last_int", "immiyear"))

## 02 - EMPLOYMENT -------------------------------------------------------------

### EMPLOYMENT YEAR ------------------------------------------------------------
# Aim: create employment_year
# employment_year: Year of first employment in Germany

# employment_year == year_first_employment_v1 if !is.na(year_first_employment_v1) & year_first_employment_v1 >= 1
data <- data %>%
  group_by(pid) %>%
  mutate(employment_year = ifelse((!is.na(year_first_employment_v1) & year_first_employment_v1 >= 1), year_first_employment_v1, NA_real_ )) %>%
  fill(employment_year, .direction = "downup") 

# employment_year == year_first_employment_v2 if is.na(employment_year) & !is.na(year_first_employment_v2) & year_first_employment_v2 >= 1
data <- data %>%
  group_by(pid) %>%
  mutate(employment_year = ifelse((is.na(employment_year) & !is.na(year_first_employment_v2) & year_first_employment_v2 >= 1), year_first_employment_v2, employment_year)) %>%
  fill(employment_year, .direction = "downup") 

# employment_year == year_first_employment_v3 if is.na(employment_year) & !is.na(year_first_employment_v3) & year_first_employment_v3 >= 1
data <- data %>%
  group_by(pid) %>%
  mutate(employment_year = ifelse((is.na(employment_year) & !is.na(year_first_employment_v3) & year_first_employment_v3 >= 1), year_first_employment_v3, employment_year)) %>%
  fill(employment_year, .direction = "downup") 

# employment_year == einstieg_pbio if is.na(employment_year) & !is.na(einstieg_pbio) & einstieg_pbio >= 1 &  !is.na(stillfj) & stillfj == 1
data <- data %>%
  group_by(pid) %>%
  mutate(employment_year = ifelse((is.na(employment_year) & !is.na(einstieg_pbio) & einstieg_pbio >= 1 &  !is.na(stillfj) & stillfj == 1), einstieg_pbio, employment_year)) %>%
  fill(employment_year, .direction = "downup") 

# employment_year == einstieg_pbio if is.na(employment_year) & !is.na(einstieg_pbio) & einstieg_pbio >= 1 &  !is.na(occmove) & occmove == 1
data <- data %>%
  group_by(pid) %>%
  mutate(employment_year = ifelse((is.na(employment_year) & !is.na(einstieg_pbio) & einstieg_pbio >= 1 &  !is.na(occmove) & occmove == 1), einstieg_pbio, employment_year)) %>%
  fill(employment_year, .direction = "downup") 

# employment_year == 0 if is.na(employment_year) & !is.na(ever_employed_ger_v2) & ever_employed_ger_v2 == 2
data <- data %>%
  group_by(pid) %>%
  mutate(employment_year = ifelse((is.na(employment_year) & !is.na(ever_employed_ger_v2) & ever_employed_ger_v2 == 2), 0, employment_year)) %>%
  fill(employment_year, .direction = "downup") 

# employment_year == 0 if is.na(employment_year) & !is.na(ever_employed_ger_v1) & ever_employed_ger_v1 == 2
data <- data %>%
  group_by(pid) %>%
  mutate(employment_year = ifelse((is.na(employment_year) & !is.na(ever_employed_ger_v1) & ever_employed_ger_v1 == 2), 0, employment_year)) %>%
  fill(employment_year, .direction = "downup") 

# employment_year == 0 if is.na(employment_year) & !is.na(never_employed_ger) & never_employed_ger == 1
data <- data %>%
  group_by(pid) %>%
  mutate(employment_year = ifelse((is.na(employment_year) & !is.na(never_employed_ger) & never_employed_ger == 1), 0, employment_year)) %>%
  fill(employment_year, .direction = "downup") 

# employment_year == 0 if is.na(employment_year) & !is.na(never_employed_life) &  never_employed_life == 1
data <- data %>%
  group_by(pid) %>%
  mutate(employment_year = ifelse((is.na(employment_year) & !is.na(never_employed_life) &  never_employed_life == 1), 0, employment_year)) %>%
  fill(employment_year, .direction = "downup") 


### EMPLOYMENT YEAR ARRIVAL ----------------------------------------------------
# Aim: create employment_year_arrival
# employment_year_arrival: employment in year of arrival yes/no

data <- data %>%
  group_by(pid) %>%
  mutate(employment_year_arrival = ifelse(employment_year==immiyear, 1, 0)) %>%
  mutate(employment_year_arrival = ifelse(is.na(employment_year), NA, employment_year_arrival)) %>%
  mutate(employment_year_arrival = ifelse(employment_year==0,0,employment_year_arrival))

data <- data %>%
  group_by(pid) %>%
  mutate(employment_year_arrival = ifelse((is.na(employment_year_arrival) & 
                                             syear == immiyear &
                                             employment_now %in% c(1:8, 10:12)),1, employment_year_arrival)) %>%
  mutate(employment_year_arrival = ifelse((is.na(employment_year_arrival) & 
                                             syear == immiyear &
                                             employment_now %in% c(9)),0, employment_year_arrival)) %>%
  fill(employment_year_arrival, .direction = "downup") 


### EMPLOYMENT 1 YEAR ARRIVAL --------------------------------------------------
# Aim: create employment_one_year_arrival
# employment_one_year_arrival: employment 1 year after arrival yes/no

data <- data %>%
  group_by(pid) %>%
  mutate(employment_one_year_arrival = ifelse(employment_year-immiyear==1, 1, 0)) %>%
  mutate(employment_one_year_arrival = ifelse(is.na(employment_year), NA, employment_one_year_arrival)) %>%
  mutate(employment_one_year_arrival = ifelse(employment_one_year_arrival== 0 & employment_year==0,0,employment_one_year_arrival)) %>%
  mutate(employment_one_year_arrival = ifelse((employment_one_year_arrival == 0 | is.na(employment_one_year_arrival)) & employment_year_arrival==1,1,employment_one_year_arrival)) 


data <- data %>%
  group_by(pid) %>%
  mutate(employment_one_year_arrival = ifelse((is.na(employment_one_year_arrival) & 
                                             syear - immiyear == 1 &
                                             employment_now %in% c(1:8, 10:12)),1, employment_one_year_arrival)) %>%
  mutate(employment_one_year_arrival = ifelse((is.na(employment_one_year_arrival) & 
                                                 syear - immiyear == 1 &
                                             employment_now %in% c(9)),0, employment_one_year_arrival)) %>%
  fill(employment_one_year_arrival, .direction = "downup") 


### EMPLOYMENT 2 YEAR ARRIVAL --------------------------------------------------
# Aim: create employment_two_year_arrival
# employment_two_year_arrival: employment 2 year after arrival yes/no


data <- data %>%
  group_by(pid) %>%
  mutate(employment_two_year_arrival = ifelse(employment_year-immiyear==2, 1, 0)) %>%
  mutate(employment_two_year_arrival = ifelse(is.na(employment_year), NA, employment_two_year_arrival)) %>%
  mutate(employment_two_year_arrival = ifelse(employment_two_year_arrival== 0 & employment_year==0,0,employment_two_year_arrival)) %>%
  mutate(employment_two_year_arrival = ifelse((employment_two_year_arrival == 0 | is.na(employment_two_year_arrival))  & employment_one_year_arrival==1,1,employment_two_year_arrival)) 


data <- data %>%
  group_by(pid) %>%
  mutate(employment_two_year_arrival = ifelse((is.na(employment_two_year_arrival) & 
                                                 syear - immiyear == 2 &
                                                 employment_now %in% c(1:8, 10:12)),1, employment_two_year_arrival)) %>%
  mutate(employment_two_year_arrival = ifelse((is.na(employment_two_year_arrival) & 
                                                 syear - immiyear == 2 &
                                                 employment_now %in% c(9)),0, employment_two_year_arrival)) %>%
  fill(employment_two_year_arrival, .direction = "downup") 

### EMPLOYMENT 3 YEAR ARRIVAL --------------------------------------------------
# Aim: create employment_three_year_arrival
# employment_three_year_arrival: employment 3 year after arrival yes/no

data <- data %>%
  mutate(employment_three_year_arrival = ifelse(employment_year-immiyear==3, 1, 0)) %>%
  mutate(employment_three_year_arrival = ifelse(is.na(employment_year), NA, employment_three_year_arrival)) %>%
  mutate(employment_three_year_arrival = ifelse(employment_three_year_arrival== 0 & employment_year==0,0,employment_three_year_arrival)) %>%
  mutate(employment_three_year_arrival = ifelse((employment_three_year_arrival == 0 | is.na(employment_three_year_arrival)) & employment_two_year_arrival==1,1,employment_three_year_arrival)) 


data <- data %>%
  group_by(pid) %>%
  mutate(employment_three_year_arrival = ifelse((is.na(employment_three_year_arrival) & 
                                                 syear - immiyear == 3 &
                                                 employment_now %in% c(1:8, 10:12)),1, employment_three_year_arrival)) %>%
  mutate(employment_three_year_arrival = ifelse((is.na(employment_three_year_arrival) & 
                                                 syear - immiyear == 3 &
                                                 employment_now %in% c(9)),0, employment_three_year_arrival)) %>%
  fill(employment_three_year_arrival, .direction = "downup") 



### EMPLOYMENT 4 YEAR ARRIVAL --------------------------------------------------
# Aim: create employment_four_year_arrival 
# employment_four_year_arrival: employment 4 year after arrival yes/no


data <- data %>%
  mutate(employment_four_year_arrival = ifelse(employment_year-immiyear==4, 1, 0)) %>%
  mutate(employment_four_year_arrival = ifelse(is.na(employment_year), NA, employment_four_year_arrival)) %>%
  mutate(employment_four_year_arrival = ifelse(employment_four_year_arrival== 0 & employment_year==0,0,employment_four_year_arrival)) %>%
  mutate(employment_four_year_arrival = ifelse((employment_four_year_arrival == 0 | is.na(employment_four_year_arrival)) & employment_three_year_arrival==1,1,employment_four_year_arrival)) 

data <- data %>%
  group_by(pid) %>%
  mutate(employment_four_year_arrival = ifelse((is.na(employment_four_year_arrival) & 
                                                   syear - immiyear == 4 &
                                                   employment_now %in% c(1:8, 10:12)),1, employment_four_year_arrival)) %>%
  mutate(employment_four_year_arrival = ifelse((is.na(employment_four_year_arrival) & 
                                                   syear - immiyear == 4 &
                                                   employment_now %in% c(9)),0, employment_four_year_arrival)) %>%
  fill(employment_four_year_arrival, .direction = "downup") 


#data_test <- data %>%
  #mutate(pid = as.integer(pid),
         #syear = as.integer(syear)) %>%
  #select(c("pid", "employment_year", "employment_year_arrival", "employment_one_year_arrival", 
          #"employment_two_year_arrival", "employment_three_year_arrival", "employment_four_year_arrival", "employment_now", "syear", "first_int", "last_int", "immiyear"))


## 03 - INDIVIDUAL-LEVEL CHARACTERISTICS ---------------------------------------
# Aim: create individual-level variables


# age
data <- data %>%
  mutate(age = syear - birth_year)

# age_immigration
data <- data %>%
  mutate(age_immigration = immiyear - birth_year)

# religious_affiliation
data <- data %>%
  group_by(pid) %>%
  mutate(religious_affiliation = ifelse(religious_affiliation >= 1, religious_affiliation, NA_real_ )) %>%
  fill(religious_affiliation, .direction = "downup") 

# german_speaking
data <- data %>%
  group_by(pid) %>%
  mutate(german_speaking = ifelse(german_speaking_v1 >= 1, german_speaking_v1, NA_real_ )) %>%
  fill(german_speaking, .direction = "downup") 

# german_writing
data <- data %>%
  group_by(pid) %>%
  mutate(german_writing = ifelse(german_writing_v1 >= 1, german_writing_v1, NA_real_ )) %>%
  fill(german_writing, .direction = "downup") 

# german_reading
data <- data %>%
  group_by(pid) %>%
  mutate(german_reading = ifelse(german_reading_v1 >= 1, german_reading_v1, NA_real_ )) %>%
  fill(german_reading, .direction = "downup") 

# school_degree_ref
data <- data %>%
  group_by(pid) %>%
  mutate(school_degree_ref = ifelse(school_degree_v1 >= 1, school_degree_v1, NA_real_ )) %>%
  fill(school_degree_ref, .direction = "downup") 

# school_degree_mig
data <- data %>%
  group_by(pid) %>%
  mutate(school_degree_mig = ifelse(school_degree_v2 >= 1, school_degree_v2, NA_real_ )) %>%
  fill(school_degree_mig, .direction = "downup") 

# school_degree_other
data <- data %>%
  group_by(pid) %>%
  mutate(school_degree_other = ifelse(school_degree_v3 >= 1, school_degree_v3, NA_real_ )) %>%
  fill(school_degree_other, .direction = "downup") 

# school country
data <- data %>%
  group_by(pid) %>%
  mutate(school_country = ifelse(school_country_v1 >= 1, school_country_v1, NA_real_ )) %>%
  fill(school_country, .direction = "downup") 

data <- data %>%
  group_by(pid) %>%
  mutate(school_country = ifelse(is.na(school_country) & school_country_v2 >= 1, school_country_v2, school_country )) %>%
  fill(school_country, .direction = "downup") 

data <- data %>%
  group_by(pid) %>%
  mutate(school_country = ifelse(is.na(school_country) & school_country_v3 >= 1, school_country_v3, school_country )) %>%
  fill(school_country, .direction = "downup") 


# school_years
data <- data %>%
  group_by(pid) %>%
  mutate(school_years = ifelse(school_years_v1 >= 1, school_years_v1, NA_real_ )) %>%
  fill(school_years, .direction = "downup") 


# vocational_training
data <- data %>%
  group_by(pid) %>%
  mutate(vocational_training = ifelse(vocational_training_v1 >= 1, vocational_training_v1, NA_real_ )) %>%
  fill(vocational_training, .direction = "downup") 


# Subset data ------------------------------------------------------------------

# Keep only the first survey year of each person ID
# syear == erstbefr
# Rows: 18.053
data <- data  %>%
  group_by(pid) %>%
  filter(syear == first_int)

# Keep only refugees with >=18 in immigration year
# age_immigration >= 18
# Rows: 15.352
data <- data %>%
  subset(age_immigration >= 18)

# Keep only if federal state residence not missing
# !is.na(bula_res)
# Rows: 13.517
data <- data  %>%
  subset(!is.na(bula_res))


# Save data --------------------------------------------------------------------

data_clean <- data
save(data_clean, file = paste0(path_data_processed,"/data_clean.RData"))









