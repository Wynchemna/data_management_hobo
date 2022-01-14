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
library("tibbletime")

rm(list=ls())

# import
my_hobo <- read.csv("https://raw.githubusercontent.com/Wynchemna/data_management_hobo/main/02_data_processed/10347355_hourly.csv")

my_hobo <- as_tibble(my_hobo) %>% 
	mutate(dttm = ymd_hms(date_time)) %>% 
	mutate(my_hobo = th) %>% 
	select("dttm", "my_hobo")

# dwd airport
dwd_airport <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/dwd_airport.csv")

dwd_airport <- as_tibble(dwd_airport) %>% 
	mutate(dttm = ymd_hms(dttm))

# dwd urban
dwd_urban <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/dwd_urban.csv")

dwd_urban <- as_tibble(dwd_urban) %>% 
	mutate(dttm = ymd_hms(dttm))

# uni meteo
uni_meteo <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/uni_meteo.csv")

uni_meteo <- as_tibble(uni_meteo) %>% 
	mutate(dttm = ymd_hms(dttm))

# wbi
wbi <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/wbi.csv")

wbi <- as_tibble(wbi) %>% 
	mutate(dttm = ymd_hms(dttm))

# join
data <- left_join(my_hobo, dwd_airport, by = "dttm") %>% 
	left_join(., dwd_urban, by = "dttm") %>% 
	left_join(., uni_meteo, by = "dttm") %>% 
	left_join(., wbi, by = "dttm")


# long format
data_long <- data %>%
	pivot_longer(cols = -c(dttm), 
		     names_to = "station",
		     values_to = "temp")
head(data_long)


#plot
ggplot(data = data_long, aes(x = dttm, y = temp)) +
	geom_line(aes(lty = station, colour = station)) +
	theme_bw() +
	labs(title = "Comparison across stations",
	     x = "Date", y = "Temperature (°C)")


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
refs <- tibble(day_time = hobo_hourly$date_time, my_hobo = hobo_hourly$temp, 
	       dwd_airport = DWD$temp, dwd_urban = DWD_urban$temp,
	       uni_meteo = Uni$temp, wbi = WBI$temp)
