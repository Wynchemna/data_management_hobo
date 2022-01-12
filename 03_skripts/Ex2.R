# Description
# Data Quality check


# ---- load libraries ----
library(tidyverse)
library(lubridate)
library(zoo)


# ---- load data ----
data_10min <- read_csv(file = 'https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2022/10_minute/10347355.csv',  col_names = T) 


# ---- Check for plausible values 1.1 ----
data_10min_QC1 <- data_10min %>% 
	mutate(qc1 = if_else(between(temp, -20, 70), 0, 1))

sum(data_10min_QC1$qc1, na.rm = T)


# ---- Check for plausible rate of change 1.2 ----
data_10min_QC2 <- data_10min_QC1 %>% 
	mutate(qc2 = if_else(lag(temp) > (temp + 1) | lag(temp) < (temp - 1), 1, 0))

sum(data_10min_QC2$qc2, na.rm = T)


# ---- Check for minimum variability 1.3 ----
data_10min_QC3 <- data_10min_QC2 %>% 
	mutate(qc3 = if_else(lag(temp, n = 5) == temp, 1, 0))
	
sum(data_10min_QC3$qc3, na.rm = T)


x <- 1:50
lag(x, n = 5)


test <- tibble(temp = c(4.52, 4.52, 4.52, 4.53, 4.55, 4.58, 4, 4, 4, 4, 4, 4, 5, 5, 8, 7, 9, 9, 9, 5, 9, 9)) %>% 
	mutate(qc3 = if_else(all_equal(temp, lag(temp, n = 5)), 1, 0))


temp = c(4.52, 4.52, 4.52, 4.53, 4.55, 4.58, 4, 4, 4, 4, 4, 4, 5, 5, 8, 7, 9, 9, 9, 5, 9, 9)

