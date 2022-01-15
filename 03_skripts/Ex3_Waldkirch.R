# ---
# 	title: "Exercise 3"
# author: "Felix Radtke"
# date: "14 1 2022"
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

hobo_hourly <- read_csv("https://raw.githubusercontent.com/Wynchemna/data_management_hobo/main/02_data_processed/10801132_hourly.csv")
#, show_col_types = FALSE
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
	theme_minimal() +
	theme(legend.position = c(0.90, 0.85)) +
	labs(title = "Comparison across stations",
	     x = "", y = "Temperature (°C)")



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

# results of lm calculation
lm_sum <- tibble(lm_DWD = summary(lm_DWD)$r.squared,
		 lm_DWD_urban = summary(lm_DWD_urban)$r.squared,
		 lm_Uni = summary(lm_Uni)$r.squared,
		 lm_WBI = summary(lm_WBI)$r.squared)
sort(lm_sum)

# A higher R² indicates a more suitable reference station to 
# fill data gaps by data from a reference series

# Best R² value was found for uni_meteo and is 0.9855592.

# input <- data.frame(refs$uni_meteo)
# 
# # predict missing temperature values for time series
# predicted <- predict(lm_Uni, newdata = input)

# linearregression with wbi
export <- refs %>%
	mutate(th = ifelse(is.na(my_hobo), 0.9335046*wbi+0.12715, my_hobo)) %>%
	mutate(origin = ifelse(is.na(my_hobo), "R", "H")) %>%
	rename(dttm = day_time) %>% 
	select("dttm", "th", "origin")



head(export, 10)
sum(is.na(export$th)) #0
# R = filled with R regression model (lm_uni_meteo) and H = origin Hobo data (my_hobo)

# plot zum überprüfen
export_validate <- tibble(dttm = export$dttm,
			  hobo_original = hobo_hourly$th,
			  #ref_station = Uni$temp,
			  hobo_corrected = export$th) 

export_validate_long <- export_validate %>% 
	pivot_longer(!dttm, names_to = 'origin', values_to = 'temp')

ggplot(data = export_validate_long, aes(x = dttm, y = temp)) +
	geom_line(aes(colour = origin)) +
	geom_line(data = Uni, col = 'grey', lty = 2) +
	theme_minimal() +
	scale_color_manual(values = c(hobo_corrected = 'red', hobo_original = 'darkblue', ref_station = 'grey')) +
	theme(legend.position = c(0.90, 0.85)) +
	labs(title = "Comparison across origin",
	     x = "", y = "Temperature (°C)")


## Export
# stand PC
write_csv(x=export, file = 'A:/radtk/OneDrive/Dokumente/Data_management_storage/data_management_hobo/02_data_processed/10801132_Th.csv')

# surface
write_csv(x=export, file = 'C:/Users/radtk/OneDrive/Dokumente/Data_management_storage/data_management_hobo/02_data_processed/10801132_Th.csv')