################################## Clean Data ####################################
# Input: data_prep_3
# output: refugee_data


# Load packages ----------------------------------------------------------------
library(data.table)
library(dplyr)
library(tidyr)
library(haven)

# Base path --------------------------------------------------------------------
base_path <- "C:/Users/ru23kek/Desktop/projects/"

# Define paths ------
path_data_soep <- file.path(base_path, "data", "soepdata")
path_data_processed <- file.path(base_path, "iab_bamf_soep", "soepdata", "processed", "refugee")
path_out <- file.path(base_path, "iab_bamf_soep", "soepdata", "final")

# Load data --------------------------------------------------------------------

# data_prep_3 
# Rows: 88.074
load(file.path(path_data_processed, "data_prep_3.RData"))

# Rename data ------------------------------------------------------------------

data <- data_prep_3

# Subset Data ------------------------------------------------------------------

# Keep only those that were not born in Germany
# germborn == 2
# Rows: 78.346
data <- data  %>%
  subset(germborn == 2)

# Keep those with direct migration background
# migback == 2
# Rows: 78.346
data <- data  %>%
  subset(migback == 2)

# Keep only those with direct refugee experience
# arefback == 2
# Rows: 77.493
data <- data  %>%
  subset(arefback == 2)

# Keep only if first interview and last interview information not missing
# Rows: 43.024
data <- data  %>%
  subset(erstbefr != -2 & letztbef != -2)

# Keep only if corigin known
# corigin != -1
# Rows: 43.015
data <- data  %>%
  subset(corigin != -1)

# Keep only those that immigrated 2 years before first survey
# erstbefr - immiyear <= 2
# Rows: 31.225
#data <- data  %>%
  #subset(erstbefr - immiyear <= 2)

# Keep only if year of immigration not missing
# !is.na(immiyear) & immiyear >-1
# Rows: 42.801
data <- data  %>%
  subset(!is.na(immiyear) & immiyear >-1)



# Rename columns ---------------------------------------------------------------

data <- data %>%
  rename(first_int = erstbefr,
         last_int = letztbef,
         birth_year = gebjahr,
         birth_month = gebmonat,
         longest_residence = lr2074_h,
         first_residence = lr3367_h,
         number_residences = lr3235,
         past_employment = lr2110,
         employment_year = lr2098,
         employment_now = plb0022_h,
         religious_affiliation = plh0258_h,
         marital_status = pld0131_h,
         english_speaking = plj0700,
         english_writing = plj0699,
         english_reading = plj0698,
         german_speaking = lm0128i01,
         german_writing = lm0128i02,
         german_reading = lm0128i03,
         read_write_level = lr2087 ,
         writing_level = lr2088,
         reading_level = lr2109,
         school_degree = lr3079,
         school_years = lb0187,
         support_migration_family = lb1246,
         support_migration_friends_v1 = lb1247_v1,
         support_migration_friends_v2 = lb1247_v2,
         no_support_migration = lb1248,
         vocational_training = lb0228,
         choice_leave_family = lr3142,
         choice_germany_family = lr3168)


# Create columns ---------------------------------------------------------------

## Age ------------------------------------
data <- data %>%
  mutate(age = syear - birth_year)

## Age Immigration -----------------------
data <- data %>%
  mutate(age_immigration = immiyear - birth_year)


## State Residence -----------------------
# bula_res == first_residence if first_residence >= 1 or

data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(first_residence >= 1, first_residence, NA_real_ )) %>%
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

## Free-case ----------------------------------------
# If person has partner no free case
# Test that partners have the same first residence

data <- data %>%
  mutate(free_case = ifelse(parid == -2, 1, 0)) %>%
  group_by(pid,parid) %>%
  mutate(same_state = ifelse(parid != -2, all(bula_res == first(bula_res)), 0)) %>%
  ungroup()

## Employment --------------------------------------

# Employment 0 Year of Arrival
data <- data %>%
  mutate(employment_year_arrival = ifelse(employment_year==immiyear, 1, 0))

# Employment 1 Year after Arrival
data <- data %>%
  mutate(employment_one_year_arrival = ifelse(employment_year-immiyear==1, 1, 0))

# Employment 2 Year after Arrival
data <- data %>%
  mutate(employment_two_year_arrival = ifelse(employment_year-immiyear==2, 1, 0))


## Schooling degree ----------------------------------

# If country of school foreign
data <- data %>%
  mutate(school_degree = ifelse(school_degree<0,NA,school_degree),
         school_degree_low = ifelse(school_degree ==1 & school_degree >0 ,
                                    1,0),
         school_degree_med = ifelse(school_degree ==2 & school_degree >0 | school_degree ==5 & school_degree >0,
                                    1,0),
         school_degree_high = ifelse(school_degree ==3 & school_degree >0 | school_degree ==4 & school_degree >0,
                                     1,0)) %>%
  mutate(school_degree_abroad = ifelse(school_degree_low == 1, 1,
                                       ifelse(school_degree_med == 1, 2,
                                              ifelse(school_degree_high == 1, 3, NA)))) 

# Data Type --------------------------------------------------------------------

# Integers 


# Integers 
data <- data %>%
  mutate(pid = as.integer(pid),
         hid = as.integer(hid),
         syear = as.integer(syear),
         first_int = as.integer(first_int),
         last_int = as.integer(last_int),
         immiyear = as.integer(immiyear),
         psample = as.integer(psample),
         age = as.integer(age),
         age_immigration = as.integer(age_immigration),
         bula_res = as.integer(bula_res),
         birth_year = as.integer(birth_year),
         birth_month = as.integer(birth_month),
         parid = as.integer(parid),
         corigin = as.integer(corigin),
         school_years = as.integer(school_years)
  )


# Factors 
data <- data %>%
  mutate(employment_now = factor(employment_now, levels = c(9,1,2,3,4,5,6,7,8,10,11,12),
                                        labels = c("no", "yes","yes","yes","yes","yes","yes","yes","yes","yes","yes","yes")),
         employment_year_arrival = factor(employment_year_arrival, levels = c(0,1),
                                          labels = c("no", "yes")),
         employment_one_year_arrival = factor(employment_one_year_arrival, levels = c(0,1),
                                              labels = c("no", "yes")),
         employment_two_year_arrival = factor(employment_two_year_arrival, levels = c(0,1),
                                              labels = c("no", "yes")),
         sex = factor(sex, levels = c(1,2), labels = c("male", "female")),
         partner = factor(partner, levels = c(0, 1, 2, 3, 4, 5),
                          labels = c("no", "married", "cohabitation", "married", "cohabitation", "not clear")),
         religious_affiliation = factor(religious_affiliation, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 
                                        labels = c("catholic", "protestant", "other christian", "muslim", "other", "atheist", "christian orthodox", "shiite religious", "sunni religious", "alevi religious")),
         marital_status = factor(marital_status, levels = c( 1, 2, 3, 4, 5),
                                 labels = c("married", "cohabitation", "single", "divorced", "widowed")),
         english_speaking = factor(english_speaking, levels = c(5, 4, 3, 2, 1),
                                  labels = c("low", "low", "medium", "medium", "high")),
         english_reading = factor(english_reading, levels = c(5, 4, 3, 2, 1),
                                 labels = c("low", "low", "medium", "medium", "high")),
         english_writing = factor(english_writing, levels = c(5, 4, 3, 2, 1),
                                 labels = c("low", "low", "medium", "medium", "high")),
         german_speaking = factor(german_speaking, levels = c(5, 3, 4, 1, 2),
                                  labels = c("low", "medium", "medium", "high", "high")),
         german_reading = factor(german_reading, levels = c(5, 3, 4, 1, 2),
                                 labels = c("low", "medium", "medium", "high", "high")),
         german_writing = factor(german_writing, levels = c(5, 3, 4, 1, 2),
                                 labels = c("low", "medium", "medium", "high", "high")),
         read_write_level = factor(read_write_level, levels = c(2,1),
                                   labels = c("no", "yes")),
         reading_level = factor(reading_level, levels = c(5, 4, 3, 2, 1),
                                 labels = c("low", "medium", "medium", "high", "high")),
         writing_level = factor(writing_level, levels = c(5, 4, 3, 2, 1),
                                 labels = c("low", "medium", "medium", "high", "high")),
         school_degree_abroad = factor(school_degree_abroad, levels = c(1, 2, 3),
                                       labels = c("low", "medium", "high")),
         free_case = factor(free_case, levels = c(0, 1),
                            labels= c( "no", "yes")),
         vocational_training = factor(vocational_training, levels = c(2, 1),
                                      labels= c( "no", "yes"))
         
  )


# Select columns ---------------------------------------------------------------

data <- data %>%
  select(-c("eintritt", "austritt", "migback", 
            "arefback", "germborn", "longest_residence",
            "first_residence", "number_residences",
            "bula", "place_bula", "place_type", 
            "employment_year", "past_employment",
            "school_degree", "support_migration_friends_v1", "support_migration_friends_v2",
            "support_migration_family", "no_support_migration", "choice_germany_family", 
            "choice_leave_family", "biwfam", "bifamc",  "bifamcl"
            
  ))



# Order columns ----------------------------------------------------------------

data <- data %>%
  select(pid, hid, syear, 
         first_int, last_int, immiyear, psample, 
         age, sex, birth_year, birth_month, free_case,
         partner, parid,
         bula_res, age_immigration, employment_year_arrival, employment_one_year_arrival, employment_two_year_arrival, employment_now,
         everything())


# Subset final -----------------------------------------------------------------


# Keep only the first survey year of each person ID
# syear == erstbefr
# Rows: 10.141
data <- data  %>%
  group_by(pid) %>%
  filter(syear == first_int)

# Keep only migrants >18
# age >= 18
# Rows: 9.722
data <- data %>%
  subset(age >= 18 & age <2017)

# Keep only migrants with >18 in immigration year
# age_immigration >= 18
# Rows: 8.908
data <- data %>%
  subset(age_immigration >= 18)

# Keep only if federal state residence not missing
# !is.na(bula_res)
# Rows: 8.309
data <- data  %>%
  subset(!is.na(bula_res))


# Save data --------------------------------------------------------------------

# refugee data
refugee_data <- data
save(refugee_data, file = file.path(path_out, "refugee_data.RData"))






