################################## Clean Data ####################################
# Input: data_prep_3
# output: refugee_data


# Load packages ------
library(data.table)
library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/refugee/"
path_out <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/final/"


# Load data ------------

# data_prep_3 
# Rows: 88.074
# Columns: 31
load(paste0(path_data_processed, "data_prep_3.RData"))

# Rename data --------

data <- data_prep_3

# Create columns ----

## Age -----
data <- data %>%
  mutate(age = syear - gebjahr)

## Federal State Residence ----
# bula_res == lr3367_h if lr3367_h >= 1 or

data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(lr3367_h >= 1, lr3367_h, NA_real_ )) %>%
  fill(bula_res, .direction = "downup") 


# bula_res == lr2074_h if lr2074_h >= 1 or
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(is.na(bula_res) & lr2074_h >= 1, lr2074_h, bula_res)) %>%
  fill(bula_res, .direction = "downup") 

# bula_res == place_bula if place_bula >= 1 or
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(is.na(bula_res) & place_bula >= 1, place_bula, bula_res)) %>%
  fill(bula_res, .direction = "downup") 


# bula_res == bula if is.na(bula_res) & lr3235 %in% c(1)
data <- data %>%
  group_by(pid) %>%
  mutate(bula_res = ifelse(is.na(bula_res) & lr3235 %in% c(1), bula, bula_res))  %>%
  fill(bula_res, .direction = "downup") 


data_test <- data %>%
  select(c("pid","syear", "erstbefr", "bula_res", "bula", "lr3367_h", "lr2074_h", "lr3235", "place_bula", "place_type"))

# Subset Data -------

# Keep only refugees >18
# age >= 18
# Rows: 48.662
data <- data %>%
  subset(age >= 18 & age <2017)

# Keep only those that were not born in germany
# germborn == 2
# Rows: 48.433
data <- data  %>%
  subset(germborn == 2)

# Keep those with direct migration background
# migback == 2
# Rows: 48.433
data <- data  %>%
  subset(migback == 2)

# Keep only those with direct refugee experience
# arefback == 2
# Rows: 47.720
data <- data  %>%
  subset(arefback == 2)

# Keep only if first interview and last interview information not missing
# Rows: 41.118
data <- data  %>%
  subset(erstbefr != -2 & letztbef != -2)

# Keep only if corigin known
# corigin != -1
# Rows: 41.109
data <- data  %>%
  subset(corigin != -1)

# Keep only those that immigrated 2 years before first survey
# erstbefr - immiyear <= 2
# Rows: 31.225
data <- data  %>%
  subset(erstbefr - immiyear <= 2)

# Keep only if federal state residence not missing
# !is.na(bula_res)
# Rows: 29.628
data <- data  %>%
  subset(!is.na(bula_res))

# Keep only the first survey year of each person ID
# syear == erstbefr
# Rows: 6573
data <- data  %>%
  group_by(pid) %>%
  filter(syear == erstbefr)



# Rename columns ---------

data <- data %>%
  rename(first_res = lr3367_h,
         number_res = lr3235,
         employment = plb0022_h,
         year_asyl = plj0666,
         residence_title = plj0680_v2,
         religious_affiliation = plh0258_v9,
         marital_status = plj0626,
         mother_tongue = lr2076,
         german_speaking = lm0128i01,
         german_writing = lm0128i02,
         german_reading = lm0128i03,
         school_abroad_certificate = lr3079,
         country_last_school = lr3077_v1,
         school_degree = lb0191,
         arrival_alone = lr3132,
         arrival_family = lr3133,
         help_refuge_family = lb1246,
         help_refuge_friends_v1 = lb1247_v1,
         help_refuge_friends_v2 = lb1247_v2,
         help_refuge_no = lb1248,
         education_outside_ger = lb0228)

# Remove columns --------

data <- data %>%
  select(-c("eintritt", "austritt", "migback", 
            "arefback", "germborn", "bula",
            "place_type", "residence_title",
            "mother_tongue", "arrival_alone",
            "help_refuge_friends_v1", "help_refuge_friends_v2", "help_refuge_no"
            ))

# Order columns ---------

data <- data %>%
  select(pid, hid, syear, 
         erstbefr, letztbef, immiyear, psample, 
         age, bula_res, employment,
         sex, gebjahr, gebmonat,
         partner, parid,
         everything())

# Modify columns ---------

# Data type
data <- data %>%
  mutate(pid = as.integer(pid),
         hid = as.integer(hid),
         syear = as.integer(syear),
         erstbefr = as.integer(erstbefr),
         letztbef = as.integer(letztbef),
         immiyear = as.integer(immiyear),
         psample = as.integer(psample),
         age = as.integer(age),
         bula_res = as.integer(bula_res),
         gebjahr = as.integer(gebjahr),
         gebmonat = as.integer(gebmonat),
         partner = as.factor(partner),
         parid = as.integer(parid),
         corigin = as.integer(corigin),
         first_res = as.integer(first_res),
         number_res = as.integer(number_res),
         year_asyl = as.integer(year_asyl),
         country_last_school = as.integer(country_last_school))
         

# Factors 
data <- data %>%
  mutate(employment = factor(employment, levels = c(9, 1, 2, 3, 4, 5, 7, 10),
                            labels = c("no", "yes", "yes", "yes", "yes", "yes", "yes", "yes")),
         sex = factor(sex, levels = c(1,2), labels = c("male", "female")),
         partner = factor(partner, levels = c(0, 1, 2, 3, 4, 5),
                          labels = c("no", "married", "cohabitation", "prob_married", "prob_cohabitation", "not_clear")),
         religious_affiliation = factor(religious_affiliation, levels = c(4, 5, 6, 7), 
                                        labels = c("muslim", "other", "atheist", "christian")),
         marital_status = factor(marital_status, levels = c( 1, 2, 3, 4, 5, 6, 7),
                                 labels = c("single", "married", "cohabitation", "separated", "cohabitation ended", "widowed", "partner deceased")),
         
         german_speaking = factor(german_speaking, levels = c( 1, 2, 3, 4, 5),
                                  labels = c("yes", "yes", "yes", "normal", "no")),
         german_reading = factor(german_reading, levels = c(1, 2, 3, 4, 5),
                                  labels = c( "yes", "yes", "yes", "normal", "no")),
         german_writing = factor(german_writing, levels = c(1, 2, 3, 4, 5),
                                 labels = c("yes", "yes", "yes", "normal", "no")),
         school_abroad_certificate = factor(school_abroad_certificate, levels = c( 1, 2, 3, 4, 5),
                                            labels = c("low", "medium", "high", "high", "medium")),
         school_degree = factor(school_degree, levels = c(1, 2, 3, 4, 5, 6 ),
                         labels = c("low", "medium", "medium", "high", "high", "medium")),
         arrival_family = factor(arrival_family, levels = c(-2, 1),
                                 labels= c("no", "yes")),
         help_refuge_family = factor(help_refuge_family, levels = c(-2, 1),
                                     labels= c("no", "yes")),
         education_outside_ger = factor(education_outside_ger, levels = c( 1, 2),
                                        labels= c( "yes", "no"))
         
         
  )

# Integers
data <- data %>%
  mutate(first_res = ifelse(first_res == -8, NA, first_res),
         number_res = ifelse(number_res %in% c(-8, -5,-1), NA, number_res),
         year_asyl = ifelse(year_asyl %in% c(-5,-2,-1), NA, year_asyl),
         country_last_school = ifelse(country_last_school %in% c(-8, -5,-2,-1), NA, country_last_school)
  )

         
        
# Save data ----------

# refugee data
refugee_data <- data
save(refugee_data, file = paste0(path_out,"refugee_data.RData"))






