############################ Prepare Data ######################################
# Input data set: 
# 1.) ppathl_ref_mig
# 2.) pl_ref_mig
# 3.) hbrutto
# 4.) biol
# 5.) regionl
# 6.) bioregion
# 7.) biojob
# Output data set:
# data_prep
# Aim: Keep columns to create variables 
# 01 - Federal State of first residence
# 02 - Employment 
# 03 - Individual-level characteristics

# Load packages ----------------------------------------------------------------
library(dplyr)
library(haven)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/"
path_data_soep <- file.path(base_path, "soep_data", "data")
path_data_soep_raw <- file.path(base_path, "soep_data", "raw")
path_data_processed <- file.path(base_path, "soep_data", "processed")


# Load data --------------------------------------------------------------------
## ppathl_ref_mig -----
# Rows: 164.138
load(file.path(path_data_processed, "ppathl_ref_mig.RData"))

## ppathl_ref_mig -----
# Rows: 164.138
load(file.path(path_data_processed, "pl_ref_mig.RData"))

## hbrutto -------
# Rows: 501.622
hbrutto <- read_dta(file.path(path_data_soep, "hbrutto.dta"))

## biol -------
# Rows: 130.429
biol <- read_dta(file.path(path_data_soep, "biol.dta"))

## regionl -----
# Rows: 652.238
regionl <- read_dta(file.path(path_data_soep, "regionl.dta"))

## bioregion ----
# Rows: 68.489
bioregion <- read_dta(file.path(path_data_soep_raw, "bioregion.dta"))

## biojob -----
# Rows: 96.177
biojob <- read_dta(file.path(path_data_soep, "biojob.dta"))


# 01 - FEDERAL STATE OF FIRST RESIDENCE ----------------------------------------


## Keep relevant columns of data sets -------

# ppathl
ppathl_01 <- ppathl_ref_mig %>%
  select(c("pid", "hid", "parid",
           "syear","eintritt", "austritt",
           "erstbefr", "letztbef", "immiyear",
           "sex", "gebjahr", "gebmonat",
           "partner", "psample","migback",
           "arefback","germborn", "corigin"))

# biol
# lb1436_h: First residence in Germany (2019-2021)
# lr2074_h: First residence in Germany (2016-2018)
# lr3367_h: First residence in Germany (2019-2021)
# lr3235: Number of accommodations in Germany
biol_01 <- biol %>%
  select(c("pid","hid", "syear",
           "lb1436_h", "lr2074_h", "lr3367_h",
           "lr3235"))

# regionl
# bula: Federal state of the household in the year of survey
regionl_01 <- regionl %>%
  select(c("hid", "syear", "bula"))

# bioregion
# place_bula: Federal state location of the person
# place_type: Type of location
bioregion_01 <- bioregion %>%
  select(c("pid", "syear", "place_bula",
           "place_type")) 

duplicates <- bioregion_01 %>%
  group_by(pid,syear) %>%
  filter(n()>1)

bioregion_01 <- bioregion_01 %>%
  distinct(pid, syear, .keep_all = TRUE)

## Merge data ---------

data_prep_1 <- ppathl_01 %>%
  left_join(biol_01, by = c("pid", "hid", "syear")) %>%
  left_join(regionl_01,  by = c("hid", "syear")) %>%
  left_join(bioregion_01, by = c( "pid", "syear"))



# 02 - EMPLOYMENT --------------------------------------------------------------

## Keep relevant columns of data sets -------

# pl
# plb0022_h: Are you currently employed? 
pl_02 <- pl_ref_mig %>%
  select("pid", "hid", "syear",
         "plb0022_h")


# biol
# lb1421: Have you ever been employed in Germany? (2019-2021)
# lm0607i02: Never employed (2016-2018)
# lr2110: Have you ever been employed in Germany?
# lr3031: Age first job (before move)
# lr3032: No job (before move)
# lr3033_h: No job in country of origin (harmonized)
# lr3033_v1: No job in country of origin
# lm0626: Job before move to Germany
# lm0607i01: When did you start your first paid job in Germany?
# lm0615i01: 1. Job In Germany: Year
# lr2098: In which year were you in your first job in Germany?
# lb0266_h: What was the last year you were employed? 

biol_02 <- biol %>%
  select(c("pid", "hid", "syear",
           "lb1421", "lm0607i02",
           "lr2110", "lr3031", "lr3032",
           "lr3033_h", "lr3033_v1", "lm0626",
           "lm0607i01", "lm0615i01",
           "lr2098", "lb0266_h", ))
     
# biojob
# agefjob: Age at first job
# einstieg_artk: Year of first job (different generation process)
# einstieg_pbio: Year of first job 
# stillfj: Still employed in first job
# occmove: Number of occupational changes
 

biojob_02 <- biojob %>%
  select(c("cid","pid",
           "agefjob", "einstieg_artk","einstieg_pbio", 
           "stillfj", "yearlast", "occmove"))


## Merge data ---------

data_prep_2 <- data_prep_1 %>%
  left_join(pl_02, by = c("pid", "hid", "syear")) %>%
  left_join(biol_02, by = c("pid", "hid", "syear")) %>%
  left_join(biojob_02,  by = c("pid")) 


# 03 - INDIVIDUAL LEVEL CHARACTERISTICS ----------------------------------------


## Keep relevant columns of data sets -------

# pl_ref_mig 
# plh0258_h: Religious affiliation
# pld0131_h: Marital status

pl_03 <- pl_ref_mig %>%
  select("pid", "hid", "syear",
         "plh0258_h", "pld0131_h")

# biol 
# lm0128i01: German speaking before arrival (Zuzug)
# lm0128i02: German writing before arrival (Zuzug)
# lm0128i03: German listening before arrival (Zuzug)
# lr3079: Degree school (for Refugees)
# lb0188: Degree school (for Migrants)
# lb0191: Degree school (for Refugees)
# lb0186_v1: School country 
# lb0186_v2: School country
# lb0186_v3: School country
# lb0187: Schooling years
# lb0228: Vocational Training Outside Germany


biol_03 <- biol %>%
  select(c("pid", "hid", "syear",
           "lm0128i01", "lm0128i02", "lm0128i03",
           "lr3079", "lb0188", "lb0191",
            "lb0186_v1", "lb0186_v2", "lb0186_v3",
           "lb0187", "lb0228"))

## Merge data ---------

data_prep_3 <- data_prep_2 %>%
  left_join(pl_03, by = c("pid", "hid", "syear")) %>%
  left_join(biol_03, by = c("pid", "hid", "syear"))

# Save data --------------------------------------------------------------------

data_prep <- data_prep_3
save(data_prep, file = paste0(path_data_processed,"/data_prep.RData"))










