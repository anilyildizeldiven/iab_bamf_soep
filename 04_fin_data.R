############################ Finalize Data ######################################
# Input data set: 
# data_clean
# Output data set:
# data_final
# Aim: Create final data set with variables
# 01 - Federal State of first residence
# 02 - Employment 
# 03 - Individual-level characteristics

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/"
path_data_soep <- file.path(base_path, "soep_data", "data")
path_data_soep_raw <- file.path(base_path, "soep_data", "raw")
path_data_processed <- file.path(base_path, "soep_data", "processed")
path_data_final <- file.path(base_path, "soep_data", "final")

# Load data --------------------------------------------------------------------

# data_clean
# Rows: 13.517
load(file.path(path_data_processed, "data_clean.RData"))

# Rename data ------------------------------------------------------------------

data <- data_clean


## 01 - FEDERAL STATE OF FIRST RESIDENCE ---------------------------------------
# Variables:
# bula_res
# Output:
# 1 to 16


# Nothing to modify



## 02 - EMPLOYMENT -------------------------------------------------------------
# Variables:
# employment_year_arrival
# employment_one_year_arrival
# employment_two_year_arrival
# employment_three_year_arrival
# employment_four_year_arrival
# Output:
# 1 - yes
# 0 - no

# Nothing to modify

## 03 - INDIVIDUAL-LEVEL CHARACTERISTICS ---------------------------------------

# sex
# Output:
# 1 - male
# 0 - female
data <- data %>%
  mutate(sex = ifelse(sex==1,1,0))

# refugee_sample
# refugees: 17, 18, 19, 24
# migrants: 15, 16, 25, 26
# Output: 
# 1 - refugees
# 0 - migrants
data <- data %>%
  mutate(refugee_sample = ifelse(psample %in% c(17,18,19,24),1,0))

# same_state
# Same state if:
# 1. Both partners same bula_res
# 2. Single person
# Output:
# 1 - yes
# 0 - no
data <- data %>%
  mutate(id_help = paste(pmin(pid, parid), pmax(pid, parid), sep = "_"))  %>%
  group_by(id_help) %>%
  mutate(same_state = ifelse(length(unique(bula_res)) == 1, 1, 0)) %>%
  ungroup() %>%
  select(-c("id_help"))

# free_case
# Free case if:
# 1. No partner
# 2. Partner but same_state = 0
# 3. Partner not in data set
# Output:
# 1 - yes
# 0 - no
data <- data %>%
  mutate(free_case = ifelse(parid == -2 | same_state == 0 | (!parid %in% pid & parid != -2) , 1, 0))

# school_degree_low
# Low: Leaving school without a qualification
# Output:
# 1 - yes
# 0 - no

data <- data %>%
  mutate(school_degree_low = ifelse((!is.na(school_degree_ref) & school_degree_ref == 1)|
                                      (!is.na(school_degree_mig) & !is.na(school_country) &
                                         school_country == 3 & school_degree_mig ==1), 1, 0)) %>%
  mutate(school_degree_low = ifelse((is.na(school_degree_ref) & is.na(school_degree_mig)), NA, school_degree_low))

# school_degree_med
# Med: Completion of secondary school
# Output:
# 1 - yes
# 0 - no
data <- data %>%
  mutate(school_degree_med = ifelse((!is.na(school_degree_ref) & school_degree_ref == 2)| 
                                    (!is.na(school_degree_ref) & school_degree_ref == 5)| 
                                    (!is.na(school_degree_mig) & !is.na(school_country) &
                                     school_country == 3 & school_degree_mig ==2), 1,0)) %>%
  mutate(school_degree_med = ifelse((is.na(school_degree_ref) & is.na(school_degree_mig)), NA, school_degree_med)) 

# school_degree_high
# High: Completion of a secondary school with a practical orientation 
# Completion of a secondary school with a general orientation
# Output:
# 1 - yes
# 0 - no
data <- data %>%
  mutate(school_degree_high = ifelse((!is.na(school_degree_ref) & school_degree_ref == 3)| 
                                      (!is.na(school_degree_ref) & school_degree_ref == 4)| 
                                      (!is.na(school_degree_mig) & !is.na(school_country) &
                                         school_country == 3 & school_degree_mig ==3), 1,0)) %>%
  mutate(school_degree_high = ifelse((is.na(school_degree_ref) & is.na(school_degree_mig)), NA, school_degree_high)) 

# school_years
data <- data %>%
  mutate(school_years = ifelse((!is.na(school_country) & school_country <=2), NA, school_years))
                               
                              
# vocational_training
# Output:
# 1- yes
# 0 - no

data <- data %>%
  mutate(vocational_training = ifelse(vocational_training == 1, 1, 0))


# Format columns ---------------------------------------------------------------

# Integers 
data <- data %>%
  mutate(pid = as.integer(pid),
         hid = as.integer(hid),
         parid = as.integer(parid),
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
         corigin = as.integer(corigin),
         school_years = as.integer(school_years)
  )

# Numerical
data <- data %>%
  mutate(sex = as.numeric(sex),
         refugee_sample = as.numeric(refugee_sample),
         free_case = as.numeric(free_case),
         partner = as.numeric(partner),
         religious_affiliation = as.numeric(religious_affiliation),
         german_speaking = as.numeric(german_speaking),
         german_writing = as.numeric(german_writing),
         german_reading = as.numeric(german_reading),
         school_degree_low = as.numeric(school_degree_low),
         school_degree_med = as.numeric(school_degree_med),
         school_degree_high = as.numeric(school_degree_high),
         vocational_training = as.numeric(vocational_training),
         employment_year = as.numeric(employment_year),
         employment_year_arrival = as.numeric(employment_year_arrival),
         employment_one_year_arrival= as.numeric(employment_one_year_arrival),
         employment_two_year_arrival= as.numeric(employment_two_year_arrival),
         employment_three_year_arrival= as.numeric(employment_three_year_arrival),
         employment_four_year_arrival= as.numeric(employment_four_year_arrival)
         )


# Final data  ------------------------------------------------------------------


# Keep relevant final columns
data <- data %>%
  select(c(
    "pid", "hid", "parid", "syear", "first_int", "last_int", "immiyear", "psample",
    "age", "age_immigration", "bula_res", "birth_year", "birth_month", "corigin",
    "school_years", "sex", "refugee_sample", "free_case", "partner",
    "religious_affiliation", "german_speaking", "german_writing", "german_reading",
    "school_degree_low", "school_degree_med", "school_degree_high", "vocational_training",
    "employment_year", "employment_year_arrival", "employment_one_year_arrival", 
    "employment_two_year_arrival", "employment_three_year_arrival", "employment_four_year_arrival"
  ))


# Order columns
data <- data %>%
  select(pid, hid, syear, 
         first_int, last_int, immiyear, psample, 
         age, sex, birth_year, birth_month, free_case,
         partner, parid,
         bula_res, age_immigration, employment_year, employment_year_arrival, employment_one_year_arrival, employment_two_year_arrival,
         employment_three_year_arrival, employment_four_year_arrival,
         everything())

# Subset data ------------------------------------------------------------------

# Keep only working age population: 18-67
# Rows: 13.462
data <- data %>%
  filter(age_immigration >= 18 & age_immigration <= 67)

# Remove implausible values

# employment_year < immiyear
impl_1 <- data[data$employment_year < data$immiyear & data$employment_year != 0 & !is.na(data$employment_year),] # 206

# immiyear > syear
impl_2 <- data[data$immiyear > data$syear,] # 0

# school_years > age_immigration
impl_3 <- data[data$school_years > data$age_immigration & !is.na(data$school_years),] # 0

implausible <- integer()
implausible <- c(implausible, impl_1$pid, impl_2$pid, impl_3$pid)
data <- data[!(data$pid %in% implausible),]

# Save data --------------------------------------------------------------------

# Rows: 13.256
# Ref: 8.677
# Mig: 4.579
data_final <- data
save(data_final, file = paste0(path_data_final,"/data_final.RData"))


















