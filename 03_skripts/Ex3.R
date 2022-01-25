# # ---
# # 	title: "Exercise 3"
# # author: "Felix Radtke"
# # date: "14 1 2022"
# # output: html_document
# # ---
# 
# 
### Loaded packages

library("lubridate")
library("tidyverse")
library("zoo")
library("tibbletime")
# 
# rm(list=ls())
# 
# # import
# my_hobo <- read.csv("https://raw.githubusercontent.com/Wynchemna/data_management_hobo/main/02_data_processed/10347355_hourly.csv")
# 
# my_hobo <- as_tibble(my_hobo) %>% 
# 	mutate(dttm = ymd_hms(date_time)) %>% 
# 	mutate(my_hobo = th) %>% 
# 	select("dttm", "my_hobo")
# 
# # dwd airport
# dwd_airport <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/dwd_airport.csv")
# 
# dwd_airport <- as_tibble(dwd_airport) %>% 
# 	mutate(dttm = ymd_hms(dttm))
# 
# # dwd urban
# dwd_urban <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/dwd_urban.csv")
# 
# dwd_urban <- as_tibble(dwd_urban) %>% 
# 	mutate(dttm = ymd_hms(dttm))
# 
# # uni meteo
# uni_meteo <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/uni_meteo.csv")
# 
# uni_meteo <- as_tibble(uni_meteo) %>% 
# 	mutate(dttm = ymd_hms(dttm))
# 
# # wbi
# wbi <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/wbi.csv")
# 
# wbi <- as_tibble(wbi) %>% 
# 	mutate(dttm = ymd_hms(dttm))
# 
# # join
# data <- left_join(my_hobo, dwd_airport, by = "dttm") %>% 
# 	left_join(., dwd_urban, by = "dttm") %>% 
# 	left_join(., uni_meteo, by = "dttm") %>% 
# 	left_join(., wbi, by = "dttm")
# 
# 
# # long format
# data_long <- data %>%
# 	pivot_longer(cols = -c(dttm), 
# 		     names_to = "station",
# 		     values_to = "temp")
# head(data_long)
# 
# 
# #plot
# ggplot(data = data_long, aes(x = dttm, y = temp)) +
# 	geom_line(aes(lty = station, colour = station)) +
# 	theme_bw() +
# 	labs(title = "Comparison across stations",
# 	     x = "Date", y = "Temperature (°C)")
# 

#############################################################

#############################################################
#-------------------------------------------------------------------------
############################### load Data ################################
#-------------------------------------------------------------------------

hobo_hourly <- read_csv("https://raw.githubusercontent.com/Wynchemna/data_management_hobo/main/02_data_processed/10347355_hourly.csv", show_col_types = FALSE)
hobo_hourly$date_time <- as.POSIXct(hobo_hourly$date_time)

#---------------------------------------------------------------------------
################## 3. Filling Gaps with regression model ###################
#---------------------------------------------------------------------------

#----- 3.1 Reference Stations -------

# start and end point of time series for reference station
start <- as.POSIXct("2021-12-13 00:00:00")
end <- as.POSIXct("2022-01-09 23:00:00")


# DWD data Flugplatz #1443
DWD <- read.delim("https://raw.githubusercontent.com/Wynchemna/data_management_hobo/main/01_1_Zusatzdaten/klima_dwd_01443.txt", sep = ";", header = TRUE, dec = ".")

DWD <- as_tibble(DWD) %>%
	select("MESS_DATUM", "TT_TU") %>%
	rename(dttm = MESS_DATUM, temp = TT_TU) %>%
	mutate(dttm = ymd_h(dttm)) %>%
	mutate(temp = round(temp, 3)) %>%
	filter(dttm > as.POSIXct("2021-12-13 00:00:00") & dttm <= as.POSIXct("2022-01-10 00:00:00"))

DWD$temp[which(DWD$temp == -999.0)] <- NA


# urban DWD data #13667
DWD_urban <- read.delim("https://raw.githubusercontent.com/Wynchemna/data_management_hobo/main/01_1_Zusatzdaten/klima_urban_dwd_13667.txt", sep = ";", header = TRUE, dec = ".")

DWD_urban <- as_tibble(DWD_urban) %>%
	select("MESS_DATUM", "LUFTTEMPERATUR") %>%
	rename(dttm = MESS_DATUM, temp = LUFTTEMPERATUR) %>%
	mutate(dttm = ymd_h(dttm)) %>%
	mutate(temp = round(temp, 3)) %>%
	filter(dttm > start & dttm <= as.POSIXct("2022-01-10 00:00:00"))


# climate data Uni Freiburg - Meteo Garden #13667
Uni <- read.csv("https://raw.githubusercontent.com/Wynchemna/data_management_hobo/main/01_1_Zusatzdaten/klima_uni_13667.csv")

Uni <- as_tibble(Uni) %>%
	select("UTC", "Lufttemperatur") %>%
	rename(dttm = UTC, temp = Lufttemperatur) %>%
	mutate(dttm = ymd_hms(dttm)) %>%
	mutate(temp = round(temp, 3)) %>%
	filter(dttm > start & dttm <= as.POSIXct("2022-01-10 00:00:00"))


# climate data WBI
WBI <- read.csv2("https://raw.githubusercontent.com/Wynchemna/data_management_hobo/main/01_1_Zusatzdaten/klima_wbi.csv")

WBI <- as_tibble(WBI) %>%
	rename(date = Tag, time = Stunde, temp = AVG_TA200) %>%
	mutate(date = parse_date(date, format = "%d.%m.%Y")) %>%
	mutate(time = parse_time(time, format = "%H:%M")) %>%
	mutate(dttm = ymd_hms(paste(date,time))) %>%
	select("dttm", "temp") %>%
	mutate(temp = round(temp, 3)) %>%
	filter(dttm > start & dttm <= as.POSIXct("2022-01-10 00:00:00"))

# New tibble with all reference stations
refs <- tibble(day_time = hobo_hourly$date_time, my_hobo = hobo_hourly$th, 
	       dwd_airport = DWD$temp, dwd_urban = DWD_urban$temp,
	       uni_meteo = Uni$temp, wbi = WBI$temp)

# long format
data_long <- refs %>%
	pivot_longer(cols = -c(day_time),
		     names_to = "station",
		     values_to = "temp")
head(data_long)


# #plot
ggplot(data = data_long, aes(x = day_time, y = temp)) +
	geom_line(aes(colour = station)) +
	theme_bw() +
	labs(title = "Comparison across stations",
	     x = "", y = "Temperature (°C)")

# ######
# # lm
# 
# ## DWD_Urban matches best
# # linear models
# summary(lm(my_hobo ~ dwd_airport, refs))$r.squared
# # 0.9673027
# 
# summary(lm(my_hobo ~ dwd_urban, refs))$r.squared
# # 0.9753459
# 
# summary(lm(my_hobo ~ uni_meteo, refs))$r.squared
# # 0.9855592
# 
# summary(lm(my_hobo ~ wbi, refs))$r.squared
# # 0.9852601
# 
# lm_uni_meteo <- lm(my_hobo ~ uni_meteo, refs)
# # Coefficients:
# # (Intercept)    uni_meteo  
# # -0.3236       1.0141  
# 
# 
# #Seems like uni-meteo fits best
# 
# # predict NA values
input <- data.frame(refs$uni_meteo)
predicted <- predict(lm_Uni, newdata = input)

# linearregression with wbi
export <- refs %>%
	mutate(th = ifelse(is.na(my_hobo), 0.9852601*uni_meteo+0.4635, my_hobo)) %>%
	mutate(origin = ifelse(is.na(my_hobo), "R", "H")) %>%
	select("day_time", "th", "origin")
# 
# 
# 

####################
#------ 3.2 Regression model -------

lm_DWD <- lm(refs$dwd_airport ~ refs$my_hobo, na.action=na.omit)
summary(lm_DWD)$r.squared # R² = 0.9673027
lm_DWD_urban <- lm(refs$dwd_urban ~ refs$my_hobo, na.action=na.omit)
summary(lm_DWD_urban)$r.squared # R² = 0.9753459
lm_Uni <- lm(refs$uni_meteo ~ refs$my_hobo, na.action=na.omit)
summary(lm_Uni)$r.squared # R² = 0.9855592
lm_WBI <- lm(refs$wbi ~ refs$my_hobo, na.action=na.omit)
summary(lm_WBI)$r.squared # R² = 0.9852601
# A higher R² indicates a more suitable reference station to 
# fill data gaps by data from a reference series

# Best R² value was found for uni_meteo and is 0.9855592.

# input <- data.frame(refs$uni_meteo)
# 
# # predict missing temperature values for time series
# predicted <- predict(lm_Uni, newdata = input)

# linearregression with wbi
export <- refs %>%
	mutate(th = ifelse(is.na(my_hobo), 0.9855592*uni_meteo+0.398482, my_hobo)) %>%
	mutate(origin = ifelse(is.na(my_hobo), "R", "H")) %>%
	select("day_time", "th", "origin")

# 
# hobo_filled <- hobo_hourly %>% 
# 	mutate(origin = if_else(is.na(th), "R", "H"),
# 	       th = if_else(is.na(th), predicted, th)) %>% 
# 	mutate(th = round(th, 3)) %>% 
# 	select(date_time, th, origin)

head(hobo_filled, 10)
sum(which(hobo_filled$th == NA)) # 0
# R = filled with R regression model (lm_WBI) and H = origin Hobo data (my_hobo)


## Export
write.csv(export, file = "", append = FALSE, quote = FALSE, sep = ",",
	  eol = "\n", na = "NA", dec = ".", row.names = FALSE,
	  col.names = TRUE, qmethod = c("escape", "double"),
	  fileEncoding = "")
