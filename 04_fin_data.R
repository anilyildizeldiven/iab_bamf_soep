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

# Load data --------------------------------------------------------------------

# data_prep
# Rows: 164.138
load(file.path(path_data_processed, "data_clean.RData"))

# Rename data ------------------------------------------------------------------

data <- data_clean


## 01 - FEDERAL STATE OF FIRST RESIDENCE ---------------------------------------
# Variables:
# bula_res


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


# Employment 0 Year of Arrival
data <- data %>%
  mutate(employment_year_arrival = ifelse(employment_year==immiyear, 1, 0)) %>%
  mutate(employment_year_arrival = ifelse(is.na(employment_year), NA, employment_year_arrival)) %>%
  mutate(employment_year_arrival = ifelse(employment_year==0,0,employment_year_arrival))


# Employment 1 Year after Arrival
data <- data %>%
  mutate(employment_one_year_arrival = ifelse(employment_year-immiyear==1, 1, 0)) %>%
  mutate(employment_one_year_arrival = ifelse(is.na(employment_year), NA, employment_one_year_arrival)) %>%
  mutate(employment_one_year_arrival = ifelse(employment_one_year_arrival== 0 & employment_year==0,0,employment_one_year_arrival)) %>%
  mutate(employment_one_year_arrival = ifelse(employment_one_year_arrival == 0 & employment_year_arrival==1,1,employment_one_year_arrival)) 
  

# Employment 2 Years after Arrival
data <- data %>%
  mutate(employment_two_year_arrival = ifelse(employment_year-immiyear==2, 1, 0)) %>%
  mutate(employment_two_year_arrival = ifelse(is.na(employment_year), NA, employment_two_year_arrival)) %>%
  mutate(employment_two_year_arrival = ifelse(employment_two_year_arrival== 0 & employment_year==0,0,employment_two_year_arrival)) %>%
  mutate(employment_two_year_arrival = ifelse(employment_two_year_arrival == 0 & employment_one_year_arrival==1,1,employment_two_year_arrival)) 

# Employment 3 Years after Arrival
data <- data %>%
  mutate(employment_three_year_arrival = ifelse(employment_year-immiyear==3, 1, 0)) %>%
  mutate(employment_three_year_arrival = ifelse(is.na(employment_year), NA, employment_three_year_arrival)) %>%
  mutate(employment_three_year_arrival = ifelse(employment_three_year_arrival== 0 & employment_year==0,0,employment_three_year_arrival)) %>%
  mutate(employment_three_year_arrival = ifelse(employment_three_year_arrival == 0 & employment_two_year_arrival==1,1,employment_three_year_arrival)) 

# Employment 4 Years after Arrival
data <- data %>%
  mutate(employment_four_year_arrival = ifelse(employment_year-immiyear==4, 1, 0)) %>%
  mutate(employment_four_year_arrival = ifelse(is.na(employment_year), NA, employment_four_year_arrival)) %>%
  mutate(employment_four_year_arrival = ifelse(employment_four_year_arrival== 0 & employment_year==0,0,employment_four_year_arrival)) %>%
  mutate(employment_four_year_arrival = ifelse(employment_four_year_arrival == 0 & employment_three_year_arrival==1,1,employment_four_year_arrival)) 


## 03 - INDIVIDUAL-LEVEL CHARACTERISTICS ---------------------------------------

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
  mutate(same_state = ifelse(length(unique(bula_res)) == 1, 1, 0))

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



































