# Test



data_1 <- rbind(refugee_data, migrants_data)

# Lframe
Lframe_1 <- data_1 %>%
  filter(immiyear <= 2015)

# Rframe
Rframe_1 <- data_1 %>%
  filter(immiyear >= 2016)

sum(is.na(Lframe_1$employment_one_year_arrival))
#no  yes 
#9086  282 


# Lframe
Lframe_2 <- data_final %>%
  filter(immiyear <= 2015)

# Rframe
Rframe_2 <- data_final %>%
  filter(immiyear >= 2016)
