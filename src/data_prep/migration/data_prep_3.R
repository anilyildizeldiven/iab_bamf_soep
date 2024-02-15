############################## 3 - Prepare Data ####################################
# Migrants and Socio-Demographic Characteristics
# Input data set: data_prep_2
# Output data set: data_prep_3
# Aim: merge free-case no/yes and further data


# Load packages ------
library(dplyr)
library(haven)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/"
path_data_processed_mig <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/migrants/"
path_out <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep/soepdata/processed/migrants/"

# Load data ------------

## data_prep_2 -----
# Rows: 76.064
load(paste0(path_data_processed_mig, "data_prep_2.RData"))


## pl_subset_ref ------
# Rows: 25.451
load(paste0(path_data_processed, "pl_subset_mig.RData"))


## biol -------
# Rows: 130.429
biol <- read_dta(paste0(path_data_soep, "biol.dta"))



# Keep relevant columns -------

## biol ------
# Relevant questions:

# lm0128i01 - German speaking before arrival (Zuzug)
# lm0128i02 - German writing before arrival (Zuzug)
# lm0128i03 - German reading before arrival (Zuzug)
# lb0191 - School degree
# lb0228 - Vocational Training Outside Germany
# lb1246 - Help relatives
# lb1247_v1 and lb1247_v2 - Help friends
# lb1248 - Help no one
# lr3079 - School Certificate Abroad (X) instead:
# lb0188 - School-Leaving Degree
# lb0186 - Country last attended school 
# lr3168 - Reason Germany: Familiy there (X) instead
# lb1245_h - Main reason for living in the place of residence [harmonised]

# lr2076 - Mother tongue (X)
# lr3132 - Arrival in Germany alone
# lr3133 - Arrival with friends



biol_variables <- c("pid", "hid", "syear",
                     "lm0128i01", "lm0128i02", 
                    "lm0128i03", "lb0188", "lr3077_v1", 
                    "lb0191", "lb1245_h",
                    "lb1246", "lb1247_v1", "lb1247_v2",
                    "lb1248", "lb0228")

biol_subset <- biol[ , biol_variables]


## pl -----------
# Relevant questions:

# plh0258_v9 - Religious affiliation
# plj0626 - Marital status


# plj0666 - Asylum application year
# plj0680_v2 - Current residence permit
# plh0258_v9 - Religious affiliation
# plm0558 - Frequency praying
# plj0626 - Marital status


pl_variables <- c("pid", "hid", "syear",
                  "plj0626")


pl_subset <- pl_subset_mig[,pl_variables]



## Merge data ----------
# input: data_prep_2, biol_subset, pl_subset
# output: data_prep_3

data_prep_3 <- data_prep_2 %>%
  left_join(pl_subset, by = c("pid", "hid", "syear")) %>%
  left_join(biol_subset, by = c("pid", "hid", "syear"))

# Save data ------

# data_prep_3
save(data_prep_3, file = paste0(path_out,"data_prep_3.RData"))








