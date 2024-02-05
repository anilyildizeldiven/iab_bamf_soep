################################### EDA #######################################
# Input: data_prep_1
# Explore data to data_prep_1
# Refugee characteristics and location


# Load packages ------
library(dplyr)
library(haven)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# Define paths -------
path_data_soep <- "C:/Users/ru23kek/Desktop/projects/data/soepdata/"
path_data_processed <- "C:/Users/ru23kek/Desktop/projects/iab_bamf_soep_project/soepdata/processed/refugee/"
path_out <- ""


# Load data ------------

## data_prep_1 -----
# Rows: 88.074
# Columns: 27
load(paste0(path_data_processed, "data_prep_1.RData"))



# Manipulate  data ------

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


# EDA --------


## 2016 ------


# Keep only those that were surveyed in 2016 and immigrated in 2016
# syear == 2016
data_16 <- data %>%
  subset(syear == 2016) %>%
  subset(immiyear == 2016)

# Create bar chart
# x-axis: federal states
# y-axis percent of households
# by corigin

# Get percent of unique households in each federal state
data_16 <- data_16 %>%
  group_by(bula_h) %>%
  mutate(percent_unique = n_distinct(hid) / n_distinct(data$hid) * 100)

# Convert to correct data format
data_16$corigin <- as.factor(data_16$corigin)
data_16$bula_h <- as.integer(data_16$bula_h)

# Plot
'ggplot(data_16, aes(x = bula_h, y = percent_unique, fill = corigin)) +
  geom_bar(stat = "identity") +
  labs(x = "Federal State", y = "Percent of Households") +
  scale_fill_brewer(palette = "Paired") +
  scale_x_continuous(breaks = 1:16) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) '

ggplot(data_16, aes(x = bula_h, y = percent_unique, fill = corigin)) +
  geom_bar(stat = "identity") +
  labs(x = "Federal State", y = "Percent of Households") +
  scale_fill_viridis(name = "Country of Origin",  option = "viridis", discrete = TRUE) +
  scale_x_continuous(breaks = 1:16) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



# Create bar chart
# x-axis: federal states
# y-axis percent of refugees
# by corigin
data_16 <- data_16 %>%
  group_by(bula_h) %>%
  mutate(percent_pid = n_distinct(pid) / n_distinct(data$pid) * 100)


ggplot(data_16, aes(x = bula_h, y = percent_pid, fill = corigin)) +
  geom_bar(stat = "identity") +
  labs(x = "Federal State", y = "Percent of Refugees/Asylum Seekers") +
  scale_fill_viridis(name = "Country of Origin",  option = "viridis", discrete = TRUE) +
  scale_x_continuous(breaks = 1:16) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 





## (1)  ---------
# How many refugees were surveyed each year?


# Survey year 2016
# 4465 refugees
# 3504 households
data_16 <- data_prep_1[data_prep_1$erstbefr == 2016,]
length(unique(data_16$pid))
length(unique(data_16$hid))


# Survey year 2017
data_17 <- data_prep_1[data_prep_1$syear == 2017 & data_prep_1$erstbefr == 2017 & data_prep_1$eintritt == 2017 & data_prep_1$age >=18 & data_prep_1$germborn == 2 & data_prep_1$arefback == 2 & data_prep_1$migback == 2& data_prep_1$immiyear != -1,]
data_17 <- data_prep_1[data_prep_1$austritt ==2018 & data_prep_1$age >=18 ,]

length(unique(data_17$pid))

# Survey year 2018
data_18 <- data_prep_1[data_prep_1$erstbefr == 2018,]
length(unique(data_18$pid))

# Survey year 2019


# Survey year 2020
data_20 <- data_prep_1[data_prep_1$eintritt == 2020,]
length(unique(data_20$pid))

# Survey year 2021


## (2) -----------
# What is the distance between survey year and immigration year







