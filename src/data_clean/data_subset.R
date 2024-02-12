################################## Subset Data ####################################
# Input: data_prep_3
# Subset data


# Load packages ------
library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/refugee/"
path_out <- ""


# Load data ------------

## data_prep_3 -----
# Rows: 88.074
# Columns: 31
load(paste0(path_data_processed, "data_prep_3.RData"))

# Rename data --------

data <- data_prep_3

# Create columns ----

## Age -----
data <- data %>%
  mutate(age = syear - gebjahr)


# Subset data -------

# Keep only refugees >18
# age >= 18
# Rows: 48.855
data <- data %>%
  subset(age >= 18)

# Keep only those that were not born in germany
# germborn == 2
# Rows: 48.592
data <- data  %>%
  subset(germborn == 2)

# Keep those with direct migration background
# migback == 2
# Rows: 48.592
data <- data  %>%
  subset(migback == 2)

# Keep only those that immigrated to Germany 2014 (2 years before survey)
# immiyear >= 2014
# Rows: 40.706
data <- data  %>%
  subset(immiyear >= 2014)

# Keep only those with direct refugee experience
# arefback == 2
# Rows: 40.334
data <- data  %>%
  subset(arefback == 2)

# Keep only if federal state is not missing (Remark: the same for bula)
# bula_h != -2
# Rows: 40.087
data <- data  %>%
  subset(bula_h != -2)

# Keep only if first interview and last interview information not missing
# Rows: 37.749
data <- data  %>%
  subset(erstbefr != -2 & letztbef != -2)

# Keep only if corigin known
# corigin != -1
data <- data  %>%
  subset(corigin != -1)


# Check data ---------

# Check if bula_h changes for some pid syear

# Fill data -------------

# place_typ
data <- data %>%
  group_by(pid) %>%
  fill(place_type)

# lr3367_h
data <- data %>%
  group_by(pid) %>%
  fill(lr3367_h)

# lr3235
data <- data %>%
  group_by(pid) %>%
  fill(lr3235)


# Federal State Subset ---------

# Keep only if place_type ==3 or lr3367_h==bula or lr3235<=2
# Meaning:
# Keep only if place longest residency
# Keep only if first residence equals residence of hh
# Keep only if number os residence <= 2 (very probable that refugees change
# within state)
data <- data %>%
  subset(place_type ==3 | lr3367_h==bula | lr3235 <=2)


# Create Survey years -----------

## 2016 ------

# Subset
data_16 <- data %>%
  subset(syear ==2016)

# Unique
pids_16 <- unique(data_16$pid)


## 2017 --------

# Subset
#data_17 <- data %>%
  #filter(syear == 2017 & !(pid %in% pids_16))


# Manipulate survey data ---------


# Remove not necessary columns
data_16 <- data_16 %>%
  select(-c("eintritt",   "austritt",   "erstbefr",   "letztbef",
            "migback",    "arefback",   "germborn",
            "bula_h", "place_type", "lr3367_h",   "lr3235",
            "lr2110",     "lr2098",     "lr2099",
            "plj0666",    "plj0680_v2",
            "plm0558","lr2076", "lr3132",
            "lr3133", "lb1246", "lb1247_v1", "lb1247_v2", "lr3077_v1",
            "lb0228"))

# Create new columns and rename

data_16 <- data_16 %>%
  mutate(pid = as.integer(pid),
         parid = as.integer(parid),
         hid = as.integer(hid),
         syear = as.integer(syear),
         immiyear = as.integer(immiyear),
         sex = factor(sex, levels = c(1,2), labels = c("male", "female")),
         gebjahr = as.integer(gebjahr),
         gebmonat = as.integer(gebmonat),
         partner = as.factor(partner),
         psample = as.integer(psample),
         corigin = as.integer(corigin),
         bula = as.integer(bula),
         plb0022_h = factor(plb0022_h, levels = c(-1, 1, 2, 3, 4, 5, 7, 9, 10),
                            labels = c(NA_character_, "yes", "yes", "yes", "yes", "yes", "yes", "no", "yes")),
         lm0128i01 = factor(lm0128i01, levels = c(-5, -1, 1, 2, 3, 4, 5),
                            labels = c(NA_character_, NA_character_, "yes", "yes", "yes", "normal", "no")),
         plh0258_v9 = factor(plh0258_v9, levels = c(-5, -2, -1, 4, 5, 6, 7), 
                             labels = c(NA_character_, NA_character_, NA_character_, "muslim", "other", "atheist", "christian")),
         plj0626  = factor(plj0626, levels = c(-3, -1, 1, 2, 3, 4, 5, 6),
                                                  labels = c(NA, NA, "not married", "married", "cohabitation", "separated", "cohabitation ended", "widowed")),
         lr3079 = factor(lr3079, levels = c(-5, -2, -1, 1, 2, 3, 4, 5),
                         labels = c(NA_character_, NA_character_, NA_character_, "no degree", "middle school", "practical-further", "general-further", "other certificate")),
         lb0191 = factor(lb0191, levels = c(-5, -2, -1, 1, 2, 3, 4, 5, 6 ),
                         labels = c(NA_character_, NA_character_, NA_character_, "no school", "8th grade", "10th grade", "techincal college", "upper", "other")),
         lb1248 = factor(lb1248, levels = c(-5, -2, -1, 1),
                         labels = c(NA_character_, "yes", NA_character_, "no"))) %>%
  rename(employment = plb0022_h,
         german_speaking = lm0128i01,
         religion = plh0258_v9,
         marital_status = plj0626,
         school_abroad_certificate = lr3079,
         school_degree = lb0191,
         help_refugee = lb1248)
         
        






