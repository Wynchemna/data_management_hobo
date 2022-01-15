# ---
# 	title: "Exercise 4"
# author: "Felix Radtke"
# date: "15 1 2022"
# output: html_document
# ---


### Loaded packages

library("lubridate")
library("tidyverse")
library("zoo")

rm(list=ls())


#-------------------------------------------------------------------------
############################### load Data ################################
#-------------------------------------------------------------------------
hobo_10min <- data_10min <- read_csv(file = 'https://raw.githubusercontent.com/data-hydenv/data/master/hobo/2022/10_minute/10347355.csv') 

hobo_hourly <- read_csv("https://raw.githubusercontent.com/Wynchemna/data_management_hobo/main/02_data_processed/10347355_Th.csv")

# ---- 1 mean temperature ----
hobo_indices <- tibble(T_avg = round(mean(hobo_hourly$th), 3))


# ---- 2 mean day temperature ----
hobo_hourly_daytime <- hobo_hourly %>% 
	mutate(h = hour(dttm)) %>% 
	filter(h > 5 & h < 18) 

hobo_indices <- hobo_indices %>% 
	mutate(T_d = round(mean(hobo_hourly_daytime$th), 3))


# ---- 3 mean night temperature ----
hobo_hourly_nighttime <- hobo_hourly %>% 
	mutate(h = hour(dttm)) %>% 
	filter(h < 6 | h > 17) 

hobo_indices <- hobo_indices %>% 
	mutate(T_n = round(mean(hobo_hourly_nighttime$th), 3))


# ---- 4 mean daily temperature amplitude ----
hobo_daily_range <- hobo_hourly %>% 
	mutate(date = date(dttm)) %>% 
	group_by(date) %>% 
	summarise(T_max = max(th), T_min = min(th)) %>% 
	ungroup() %>% 
	mutate(range = T_max-T_min)

hobo_indices <- hobo_indices %>% 
	mutate(T_amp = round(mean(hobo_daily_range$range), 3))


# ---- 5 most rapid temperature change in 6 hours ----
t6h_data <- hobo_hourly %>%
	mutate(t6h_1 = abs(th - lead(th)),
	       t6h_2 = abs(th - lead(th, n = 2)),
	       t6h_3 = abs(th - lead(th, n = 3)),
	       t6h_4 = abs(th - lead(th, n = 4)),
	       t6h_5 = abs(th - lead(th, n = 5)),
	       t6h_6 = abs(th - lead(th, n = 6))) %>%
	rowwise() %>%
	mutate(t6h = max(t6h_1, t6h_2, t6h_3, t6h_4, t6h_5, t6h_6, na.rm = T)) %>% 

hobo_indices <- hobo_indices %>% 
	mutate(T_6h = max(t6h_data$t6h))

# ---- 6 median light intensity during the day ----
hobo_10min_mean_lux <- hobo_10min %>% 
	mutate(h = hour(dttm)) %>% 
	filter(h > 5 & h < 18) 

hobo_indices <- hobo_indices %>% 
	mutate(L_avg = median(hobo_10min_mean_lux$lux))


# ---- 7 hour:minute of maximum light intensity ----
hobo_10min_max_lux <- hobo_10min %>% 
	mutate(h = hour(dttm)) %>% 
	filter(h > 5 & h < 18) %>% 
	mutate(hm = hm(format(dttm, "%H:%M"))) %>% 
	group_by(hm) %>% 
	summarise(meanlux = mean(lux, na.rm = TRUE)) %>% 
	ungroup() %>% 
	filter(meanlux == max(meanlux))



hobo_indices <- hobo_indices %>% 
	mutate(L_max = hobo_10min_max_lux$meanlux)

# ---- 8 fraction of missing values ----
# 


# ---- 9 ID of ref. station with highest R² ----
# 


# ---- 10 R² value of corresponding ref.station ----
# 