# Description
# Data Quality check


# ---- load libraries ----
library(tidyverse)
library(lubridate)
library(zoo)

# ---- functions ----

meanNA <- function(x){mean(x, na.rm = TRUE)}



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
	mutate(qc3_V1 = lag(temp, n = 1)) %>% 
	mutate(qc3_V2 = lag(temp, n = 2)) %>% 
	mutate(qc3_V3 = lag(temp, n = 3)) %>% 
	mutate(qc3_V4 = lag(temp, n = 4)) %>% 
	mutate(qc3_V5 = lag(temp, n = 5)) %>% 
	
	mutate(qc3 = if_else(qc3_V1 == temp & qc3_V2 == temp & qc3_V3 == temp & qc3_V4 == temp & qc3_V5 == temp, 1, 0)) %>% 
	select(id, dttm, temp, lux, qc1, qc2, qc3)

sum(data_10min_QC3$qc3, na.rm = T)


# test <- tibble(temp = c(4.52, 4.52, 4.52, 4.53, 4.55, 4.58, 4, 4, 4, 4, 4, 4, 5, 5, 8, 7, 9, 9, 9, 5, 9, 9)) %>% 
# 	# mutate(qc3 = if_else(lag(temp, n = 5) != temp, 0, 1))
# 	# mutate(qc3_V1 = lag(temp, n = 1)) %>% 
# 	# mutate(qc3_V2 = lag(temp, n = 2)) %>% 
# 	# mutate(qc3_V3 = lag(temp, n = 3)) %>% 
# 	# mutate(qc3_V4 = lag(temp, n = 4)) %>% 
# 	# mutate(qc3_V5 = lag(temp, n = 5)) %>% 
# 	
# 	mutate(qc3_V1 = lag(temp, n = 1)) %>% 
# 	mutate(qc3_V2 = temp[-1]) %>% 
# 	mutate(qc3_V3 = lag(temp, n = 3)) %>% 
# 	mutate(qc3_V4 = lag(temp, n = 4)) %>% 
# 	mutate(qc3_V5 = lag(temp, n = 5)) %>%
# 	
# 	mutate(qc3 = if_else(qc3_V1 == temp & qc3_V2 == temp & qc3_V3 == temp & qc3_V4 == temp & qc3_V5 == temp, 1, 0)) %>% 
# 	select(temp, qc3)



# ---- Check influence of sunlight on temperature 1.4 ----

data_10min_QC4 <- data_10min_QC3 %>% 
	mutate(SIC_class = case_when(
		between(lux, 0, 10) ~ "night",
		between(lux, 10, 500) ~ "dawn",
		between(lux, 500, 2000) ~ "overcast_full",
		between(lux, 2000, 15000) ~ "overcast_light",
		between(lux, 15000, 20000) ~ "clear_sky",
		between(lux, 20000, 50000) ~ "sunshine",
		lux > 50000 ~ "sunshine_bright",
	)) %>% 
	mutate(time = substring(dttm, 12, 20)) %>% 
	mutate(day_night = if_else(between(as.numeric(as.POSIXct(time, format="%H:%M:%S")),
					   as.numeric(as.POSIXct('06:00:00', format="%H:%M:%S")), 
					   as.numeric(as.POSIXct('17:59:59', format="%H:%M:%S"))), 
				   "day","night")) %>% 
	# transform time to milliseconds, depending on the actual day


	mutate(lux_sun = if_else(SIC_class == "sunshine" & day_night == "day", 1, 0),
	       lux_sun = rollapply(lux_sun, width = 3, FUN = sum, fill = NA),
	       lux_sunbright = if_else(SIC_class == "sunshine_bright" & day_night == "day", 1, 0),
	       lux_sunbright = rollapply(lux_sunbright, width = 7, FUN = sum, fill = NA),
	       qc4 = if_else(lux_sun > 0 | lux_sunbright > 0, 1, 0)) %>% 
	select(id, dttm, temp, lux, qc1, qc2, qc3, qc4)



# ---- Flag datapoints with at least one QC-Fail ----
data_10min_QC_flag <- data_10min_QC4 %>% 
	mutate(qc_total = qc1 + qc2 + qc3 + qc4) %>% 
	select(id, dttm, temp, lux, qc1, qc2, qc3, qc4, qc_total)

# ---- convert to hourly-data if less than 2 flags ----
data_hourly <- data_10min_QC_flag %>% 
	mutate(dttm = floor_date(dttm, 'hour')) %>% 
	group_by(dttm) %>% 
	summarise(th = round(meanNA(temp), 3),
		  lux = round(meanNA(lux), 3),
		  qc1 = sum(qc1),
		  qc2 = sum(qc2),
		  qc3 = sum(qc3),
		  qc4 = sum(qc4),
		  qc_total = sum(qc_total)) %>% 
	rename(date_time = dttm) %>% 
	ungroup() 

# hours with more than one flag will be declared as NA
data_hourly$th[which(data_hourly$qc_total > 1)] <- NA

# delete all not needed collums
data_hourly_upload <- data_hourly %>% 
	select(date_time, th)


	
	

