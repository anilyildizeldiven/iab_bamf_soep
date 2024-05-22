##################### Exploratory Data Analysis ################################
# Input data set: 
# data_final
# Aim: EDA
# 1. Number of missings per column
# 2. Distributions 
# 3. 

# Load packages ----------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(scales)
library(xtable)
library(psych)
library(UpSetR)
library(naniar)
library(patchwork)
library(ggplotify)

# Define paths -----------------------------------------------------------------
base_path <- "/Users/clarastrasser/"
path_data_final <- file.path(base_path, "soep_data", "final")

# Load data --------------------------------------------------------------------

# data_final
# Rows: 13.256
load(file.path(path_data_final, "data_final.RData"))

# Rename data ------------------------------------------------------------------

data <- data_final

# EDA --------------------------------------------------------------------------

## Unique values ---------------------------------------------------------------
# Result: 13.234
n_distinct(data$pid)
# Result: 16
n_distinct(data$bula_res)


## MISSINGS --------------------------------------------------------------------

### Missings by variable -------------------------------------------------------

table_missings <- print(data.frame(Variable = names(data), 
                 Count = sapply(data, function(x) sum(is.na(x))), 
                 Percentage = sapply(data, function(x) sum(is.na(x)) / length(x) * 100)))
table_missings <- xtable::xtable(table_missings, caption = "Missing Values")
print(table_missings, include.rownames=FALSE)

### Missings by variable set ---------------------------------------------------

plot_missings <- gg_miss_upset(data, nsets = n_var_miss(data))
plot_missings <- as.ggplot(plot_missings)
ggsave("output/plots/plot_missings.png", plot = plot_missings)

### Missings of employment year by year of immigration -------------------------

data_missings <- data %>%
  mutate(missing = ifelse(is.na(employment_year),1,0))

missing_summary <- aggregate(missing ~ immiyear + refugee_sample + syear, data = data_missings, sum)

ggplot(missing_summary, aes(x = immiyear, y = missing, color = as.factor(refugee_sample))) +
  geom_line() +
  geom_point() +
  labs(
    title = "Number of Missing Values by Year and Refugee Sample",
    x = "Immigration Year",
    y = "Number of Missing Values",
    color = "Refugee Sample"
    
  ) +
  theme_minimal()

### Missings of employment year by syear observations --------------------------

data_missings <- data %>%
  mutate(missing = ifelse(is.na(employment_year),1,0))

total_observations <- aggregate(missing ~ syear, data = data_missings, length)

missing_values <- aggregate(missing ~ syear, data = data_missings, sum)

missing_summary <- merge(total_observations, missing_values, by = "syear")

names(missing_summary) <- c("syear", "total", "missing")

missing_summary$percentage_missing <- (missing_summary$missing / missing_summary$total) * 100


## DISTRIBUTION ----------------------------------------------------------------

### Distribution numerical -----------------------------------------------------

var_num <- c("immiyear", "age", "birth_year", "age_immigration", "school_years")
data[var_num] <- lapply(data[var_num], as.numeric)

summary_stats_num <- data.frame(
  Variable = names(data[var_num]),
  SD = apply(data[var_num], 2, sd, na.rm = TRUE),
  Min. = apply(data[var_num], 2, min, na.rm = TRUE),
  Q1 = apply(data[var_num], 2, quantile, probs = 0.25, na.rm = TRUE),
  Median = apply(data[var_num], 2, median, na.rm = TRUE),
  Mean = apply(data[var_num], 2, mean, na.rm = TRUE),
  Q3 = apply(data[var_num], 2, quantile, probs = 0.75, na.rm = TRUE),
  Max. = apply(data[var_num], 2, max, na.rm = TRUE)
)

table_num <- xtable::xtable(summary_stats_num)
print(table_num, include.rownames=FALSE)

### Distribution factors -------------------------------------------------------

var_cat <- c("psample", "sex", "free_case", "bula_res", "corigin",
             "employment_one_year_arrival","employment_two_year_arrival", "employment_three_year_arrival", 
             "employment_four_year_arrival", "refugee_sample", "german_speaking", 
             "german_writing", "german_reading", "school_degree_low",
             "school_degree_med", "school_degree_high", "vocational_training")

data[var_cat] <- lapply(data[var_cat], as.factor)

summary_stats_cat <- data.frame(
  Variable = character(0),
  Category = character(0),
  Frequency = numeric(0),
  Percentage = numeric(0),
  stringsAsFactors = FALSE
)

for (var in var_cat) {
  var_counts <- table(data[[var]])
  var_perc <- var_counts / sum(var_counts) * 100
  
  var_summary <- data.frame(
    Variable = rep(var, length(var_counts)),
    Category = names(var_counts),
    Frequency = as.numeric(var_counts),
    Percentage = as.numeric(var_perc),
    stringsAsFactors = FALSE
  )
  
  summary_stats_cat <- rbind(summary_stats_cat, var_summary)
}


latex_table_individual <- xtable::xtable(summary_stats_cat)
print(xtable(latex_table_individual), include.rownames=FALSE)


## RELATIONSHIPS ---------------------------------------------------------------



### Employment 1 Year and Refugee Sample ---------------------------------------
data_summary <- data %>%
  group_by(refugee_sample, employment_one_year_arrival) %>%
  summarize(count = n(), .groups = 'drop')

# Create the plot
ggplot(data_summary, aes(x = factor(refugee_sample), y = count, fill = factor(employment_one_year_arrival))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Refugee Sample", y = "Count", fill = "Employment 1 Year",
       title = "Distribution of Employment Status by Refugee Sample") +
  scale_fill_manual(values = c("0" = "#66A182", "1" = "#2E4057")) +
  theme_minimal()


### Employment 1 Year and German Level -----------------------------------------
data_summary <- data %>%
  group_by(german_writing, employment_one_year_arrival) %>%
  summarize(count = n(), .groups = 'drop')

# Create the plot
ggplot(data_summary, aes(x = factor(german_writing), y = count, fill = factor(employment_one_year_arrival))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Level", y = "Count", fill = "Employment 1 Year",
       title = "Distribution of Employment Status by German Level") +
  scale_fill_manual(values = c("0" = "#66A182", "1" = "#2E4057")) +
  theme_minimal()



### Employment 1 Year and Federal State ----------------------------------------

data_summary <- data %>%
  group_by(bula_res, employment_one_year_arrival) %>%
  summarize(count = n(), .groups = 'drop')

# Create the plot
ggplot(data_summary, aes(x = factor(bula_res), y = count, fill = factor(employment_one_year_arrival))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Federal State", y = "Count", fill = "Employment 1 Year",
       title = "Distribution of Employment Status by Federal State") +
  scale_fill_manual(values = c("0" = "#66A182", "1" = "#2E4057")) +
  theme_minimal()




### Employment 1 Year and School Degree High -----------------------------------

data_summary <- data %>%
  group_by(school_degree_high, employment_one_year_arrival) %>%
  summarize(count = n(), .groups = 'drop')

# Create the plot
ggplot(data_summary, aes(x = factor(school_degree_high), y = count, fill = factor(employment_one_year_arrival))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "School Degree High", y = "Count", fill = "Employment 1 Year",
       title = "Distribution of Employment Status by School Degree High") +
  scale_fill_manual(values = c("0" = "#66A182", "1" = "#2E4057")) +
  theme_minimal()


### Employment 1 Year and Sex -----------------------------------

data_summary <- data %>%
  group_by(sex, employment_one_year_arrival) %>%
  summarize(count = n(), .groups = 'drop')

# Create the plot
ggplot(data_summary, aes(x = factor(sex), y = count, fill = factor(employment_one_year_arrival))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Sex", y = "Count", fill = "Employment 1 Year",
       title = "Distribution of Employment Status by Sex") +
  scale_fill_manual(values = c("0" = "#66A182", "1" = "#2E4057")) +
  theme_minimal()



### Employment 1 Year and Free Case -----------------------------------

data_summary <- data %>%
  group_by(free_case, employment_one_year_arrival) %>%
  summarize(count = n(), .groups = 'drop')

# Create the plot
ggplot(data_summary, aes(x = factor(free_case), y = count, fill = factor(employment_one_year_arrival))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Free Case", y = "Count", fill = "Employment 1 Year",
       title = "Distribution of Employment Status by Free Case") +
  scale_fill_manual(values = c("0" = "#66A182", "1" = "#2E4057")) +
  theme_minimal()



























