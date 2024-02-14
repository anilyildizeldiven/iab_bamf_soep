############################## 3 - Prepare Data ####################################
# Refugees and Socio-Demographic Characteristics
# Input data set: data_prep_2
# Output data set: data_prep_3
# Aim: merge free-case no/yes and further data


# Load packages ------
library(dplyr)
library(haven)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/"
path_data_processed_ref <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/refugee/"
path_out <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/refugee/"

# Load data ------------

## data_prep_2 -----
# Rows: 88.074
# Columns: 33
load(paste0(path_data_processed_ref, "data_prep_2.RData"))


## pl_subset_ref ------
# Rows: 25.451
# Columns: 5157
load(paste0(path_data_processed, "pl_subset_ref.RData"))


## biol -------
# Rows: 130.429
# Columns: 3579
biol <- read_dta(paste0(path_data_soep, "biol.dta"))



# Keep relevant columns -------

## biol ------
# Relevant questions:
# lr2076 - Mother tongue
# lm0128i01 - German before arrival (Zuzug)
# lr3079 - School Certificate Abroad
# lr3077_v1 - Country last attended school
# lb0191 - School degree
# lr3132 - Arrival in Germany alone
# lr3133 - Arrival with friends
# lb1246 - Help relatives
# lb1247_v1 and lb1247_v2 - Help friends
# lb1248 - Help no one
# lr3168 - Familiy there
# lb0228 - Vocational Training Outside Germany

biol_variables <- c("pid", "hid", "syear",
                    "lr2076", "lm0128i01", "lm0128i02", 
                    "lm0128i03", "lr3079", "lr3077_v1", 
                    "lb0191","lr3132", "lr3133", "lr3168",
                    "lb1246", "lb1247_v1", "lb1247_v2",
                    "lb1248", "lb0228")

biol_subset <- biol[ , biol_variables]


## pl -----------
# Relevant questions:
# plj0666 - Asylum application year
# plj0680_v2 - Current residence permit
# plh0258_v9 - Religious affiliation
# plm0558 - Frequency praying
# plj0626 - Marital status


pl_variables <- c("pid", "hid", "syear",
                  "plj0666", "plj0680_v2", "plh0258_v9",
                  "plj0626")


pl_subset <- pl_subset_ref[,pl_variables]



## Merge data ----------
# input: data_prep_2, biol_subset, pl_subset
# output: data_prep_3

data_prep_3 <- data_prep_2 %>%
  left_join(pl_subset, by = c("pid", "hid", "syear")) %>%
  left_join(biol_subset, by = c("pid", "hid", "syear"))

# Save data ------

# data_prep_3
save(data_prep_3, file = paste0(path_out,"data_prep_3.RData"))








