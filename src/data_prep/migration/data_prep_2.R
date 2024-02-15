############################## 2 - Prepare Data ####################################
# Migrants and Employment
# Input data set: data_prep_1, pl_subset_ref, biol
# Output data set: data_prep_2
# Aim: Merge employment outcome of interest


# Load packages ------
library(dplyr)
library(haven)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_soep_raw <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/raw/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/"
path_data_processed_mig <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/migrants/"
path_out <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/migrants/"

# Load data ------------

## data_prep_1 -----
# Rows: 88.074
# Columns: 23
load(paste0(path_data_processed_mig, "data_prep_1.RData"))

## pl_subset_mig ------
# Rows: 32.737
# Columns: 5157
load(paste0(path_data_processed, "pl_subset_mig.RData"))

## biol -------
# Rows: 130.429
# Columns: 3579
biol <- read_dta(paste0(path_data_soep, "biol.dta"))


# Keep relevant columns -------

## biol ------
# Rows: 130.429
# Columns: 6
# Relevant questions:
# lm0607i01 - In which year were you employed in Germany at your first job?
# lb1421 - Have you ever been employed in Germany?
# lb0265 - Are you currently employed?


biol_variables <- c("pid", "hid", "syear",
                    "lm0607i01", "lb1421", "lb0265")

biol_subset <- biol[ , biol_variables]



## pl_subset_ref ------
# Rows: 25.451
# Columns: 
# Relevant questions:
# plb0022_h: Are you currently employed? 
# Answer options:
# 1 - full employment
# 2 - part-time
# 3 - ausbildung
# 4 - geringfügig
# 5 - altersteilzeit
# 6 - wehrdienst
# 7 - FSJ
# 8 - Werkstatt für Behinderte
# 9 - no
# -1 - not known

pl_variables <- c("pid", "hid", "syear",
                  "plb0022_h")


pl_subset <- pl_subset_mig[,pl_variables]


# Merge data ------
# input: data_prep_1, biol_subset, pl_subset
# output: data_prep_2
# Rows: 88.074
# Columns: 33

data_prep_2 <- data_prep_1 %>%
  left_join(pl_subset, by = c("pid", "hid", "syear")) #%>%
# left_join(biol_subset, by = c("pid", "hid", "syear"))



# Save data ------

# data_prep_2
save(data_prep_2, file = paste0(path_out,"data_prep_2.RData"))





















