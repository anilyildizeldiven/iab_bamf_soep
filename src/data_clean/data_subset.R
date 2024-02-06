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
data <- data %>%
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


# Fill data -------------


df <- data %>%
  group_by(pid) %>%
  fill(plj0666)







