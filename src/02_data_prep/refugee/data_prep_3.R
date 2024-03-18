############################## 3 - Prepare Data ####################################
# Refugees and socio-Demographic Characteristics
# Input data set: data_prep_2
# Output data set: data_prep_3
# Aim: merge free-case no/yes and further data


# Load packages ------
library(dplyr)
library(haven)

# Base path ------
base_path <- "C:/Users/ru23kek/Desktop/projects/"

# Define paths ------
path_data_soep <- file.path(base_path, "data", "soepdata")
path_data_processed <- file.path(base_path, "iab_bamf_soep", "soepdata", "processed")
path_data_processed_ref <- file.path(path_data_processed, "refugee")
path_out <- path_data_processed_ref

# Load data ------

## data_prep_2 -----
# Rows: 88.074
load(file.path(path_data_processed_ref, "data_prep_2.RData"))

## pl_subset_ref ------
# Rows: 25.451
load(file.path(path_data_processed, "pl_subset_ref.RData"))

## biol -------
# Rows: 130.429
biol <- read_dta(file.path(path_data_soep, "biol.dta"))

## bioimmig -------
# Rows: 443.643
bioimmig <- read_dta(file.path(path_data_soep, "bioimmig.dta"))



# Keep relevant columns -------


## pl -----------
# Relevant questions:
# plh0258_h - Religious affiliation
# pld0131_h - Marital status
# plj0698 - english_read
# plj0699 - english_write
# plj0700 - english_speak


pl_variables <- c("pid", "hid", "syear",
                 "plh0258_h", "pld0131_h",
                 "plj0698", "plj0699", "plj0700")


pl_subset <- pl_subset_ref[,pl_variables]


## biol ------
# Relevant questions:
# lm0128i01 - German speaking before arrival (Zuzug)
# lm0128i02 - German writing before arrival (Zuzug)
# lm0128i03 - German listening before arrival (Zuzug)
# lr2087 - read and write level
# lr2088 - Read level
# lr2109 - Write level
#  - Country last attended school (only relevant for migration)
# lr3079 - Degree school
# lb0187 - Schooling years
# lb1246 - Help relatives
# lb1247_v1 and lb1247_v2 - Help friends
# lb1248 - Help no one
# lb0228 - Vocational Training Outside Germany
# lr3142 - Reason left (family)
# lr3168 - Reason Germany (family)


biol_variables <- c("pid", "hid", "syear",
                    "lm0128i01", "lm0128i02", 
                    "lm0128i03","lr2087",
                    "lr2088", "lr2109", "lr3079",
                    "lb0187", "lb1246", 
                    "lb1247_v1", "lb1247_v2",
                    "lb1248", "lb0228", "lr3142", "lr3168")

biol_subset <- biol[ , biol_variables]

## bioimmig -------
# Relevant questions
# biwfam - Already Had Family In Country
# bifamc - Contacts With Family In Germany
# bifamcl - Moved To Same City,Town As Family

bioimmig_variables <- c("pid", "hid", "syear",
                    "biwfam", "bifamc", 
                    "bifamcl")

bioimmig_subset <- bioimmig[ , bioimmig_variables]


## Merge data ----------
# input: data_prep_2, biol_subset, pl_subset
# output: data_prep_3

data_prep_3 <- data_prep_2 %>%
  left_join(pl_subset, by = c("pid", "hid", "syear")) %>%
  left_join(biol_subset, by = c("pid", "hid", "syear")) %>%
  left_join(bioimmig_subset, by = c("pid", "hid", "syear")) 

# Save data ------

# data_prep_3
save(data_prep_3, file = file.path(path_out, "data_prep_3.RData"))








