############################## Read Data ####################################

# Load package
library(dplyr)
library(haven)

# Define paths to data sources
path_data_soep_raw <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/raw/"
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_out <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep_project/soepdata/processed/"


# Load data ------------

# Data Set: pl
# About: 
#
#
# Rows: 763.223
# Columns: 5157
pl <- read_dta(paste0(path_data_soep, "pl.dta"))



# Data Set: biol
biol <- read_dta(paste0(path_data_soep, "biol.dta"))



# Data Set: ppathl
# Rows: 1.285.062
# Columns: 45
ppathl <- read_dta(paste0(path_data_soep, "ppathl.dta"))


# Subset data ------

# Data Set : ppathl
# Aim: 
# Keep "pid" for refugee sample
# Samples:
# M1 - 15: 2013 Migration (1995-2011)
# M2 - 16: 2015 Migration (2009-2013)
# M3 - 17: 2016 Refugee (2013-2015)
# M4 - 18: 2016 Refugee/family (2013-2015)
# M5 - 19: 2017 Refugee (2013-2016)
# M6 - 24: 2020 Refugee
# M7 - 25: 2020 Migration
# M8 - 26: 2020 Migration Professionals
# Keep further variables of interest
# "psample": Refugee Sample
# "immiyear": Year an individual immigrated to Germany (>=2011)
# "germborn": Respondent born in Germany (==2)
# "corigin": Country where respondent was born (!= -1)
# "migback": direct or indirect migration (==2)
# "arefback": is respondent refugee (==2 or ==3)

# Keep only samples of interest
# Rows: 164.138
# Columns: 45
ppathl_subset <- ppathl[ppathl$psample %in% c(15, 16, 17, 18, 19, 24, 25, 26), ]

# Rows: 76.064
# Columns: 45
ppathl_subset_mig <- ppathl[ppathl$psample %in% c(15, 16, 25, 26), ]
# Rows: 88.074
# Columns: 45
ppathl_subset_ref <- ppathl[ppathl$psample %in% c(17, 18, 19, 24), ]


# Keep only variables of interest
# Rows: 164.138
# Columns: 12
ppathl_variables <- c("pid", "syear", "sex", "gebjahr",
                      "erstbefr", "psample", "letztbef",
                      "immiyear", "germborn", "corigin",
                      "gebmonat", "migback")
ppathl_subset <- ppathl_subset[,ppathl_variables]

# Keep unique personal IDs 
# Vector length: 39.349
unique_pid <- unique(ppathl_subset$pid)
# Vector length: 17.069
unique_pid_mig <- unique(ppathl_subset_mig$pid)
# Vector length: 22.280
unique_pid_ref <- unique(ppathl_subset_ref$pid)

# Data Set: pl
# Aim:
# Keep "pid" for refugee sample
# Match with "ppathl"

# Keep rows of refugee sample
# Rows: 58.188
# Columns: 5157
pl_subset <- pl[pl$pid %in% unique_pid, ]

# Rows: 32.737
# Columns: 5157
pl_subset_mig <- pl[pl$pid %in% unique_pid_mig, ]

# Rows: 25.451
# Columns: 5157
pl_subset_ref <- pl[pl$pid %in% unique_pid_ref, ]


# Keep variables of interest



# Create main ----------



# Save data -------

# pl_subset
save(pl_subset, file = paste0(path_out,"pl_subset.RData"))

# pl_subset_mig
save(pl_subset, file = paste0(path_out,"pl_subset.RData"))

# pl_subset_ref
save(pl_subset, file = paste0(path_out,"pl_subset.RData"))












