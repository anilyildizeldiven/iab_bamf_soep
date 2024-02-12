######################### EDA - Federal States ############################


# Load packages ------
library(dplyr)
library(haven)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/refugee/"
path_out <- ""


# Load data ------------

## data_prep_1 -----
# Rows: 88.074
# Columns: 33
load(paste0(path_data_processed, "data_prep_1.RData"))


# Subset data ----------

## Create "age" variable -----
data_prep_1 <- data_prep_1 %>%
  mutate(age = erstbefr - gebjahr)

## Keep only if .... -----

# Keep only refugees >18
# age >= 18
data <- data_prep_1 %>%
  subset(age >= 18)

# Keep only those that were not born in germany
# germborn == 2
data <- data  %>%
  subset(germborn == 2)

# Keep those with direct migration background
# migback == 2
data <- data  %>%
  subset(migback == 2)

# Keep only those that immigrated to Germany 2014
# immiyear >= 2014
data <- data  %>%
  subset(immiyear >= 2014)

# Keep only those with direct refugee experience
# arefback == 2
data <- data  %>%
  subset(arefback == 2)

# Keep only if federal state is not missing
# bula_h != -2
data <- data  %>%
  subset(bula_h != -2)

# Keep bula only if place_type ==3 or bula == lr3367_h
data <- data %>%
  subset(place_type == 3 | bula == lr3367_h)



# Subset columns -------

data <- data %>%
  select(-c("bula_h","bula_v1","bula_v2","wein_v1","wein_v2", "wein_v3","hader_v1","hader_v2" ,"hadq",
            "lr3367_v1",  "lr3367_v2",  "lr2074_h",   "lr2074_v1"))

# Check -------

data_fed <- data %>%
  select(pid, hid, syear, erstbefr, immiyear,
         bula_h, bula, lr3367_h, lr2074_h, place_type) %>%
  filter(!is.na(lr3367_h)& !(lr3367_h %in% c(-8, -5, -2)))

match <- data_fed %>%
  filter(bula != lr3367_h)
# in 81% of the cases the first residence matches with household federal state

# Remarks:
# bula and bula_h is the same
# Only for syear 2019-2021 the variable "lr3367_h" (1.Wohnort in De)












