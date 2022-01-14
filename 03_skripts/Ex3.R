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
my_hobo <- read.csv("https://raw.githubusercontent.com/imifrenzel/hyd_data_management/main/10610854_hourly.csv")
my_hobo <- my_hobo %>% 
	mutate(dttm = ymd_hms(date_time)) %>% 
	mutate(my_hobo = th) %>% 
	select("dttm", "my_hobo")
dwd_airport <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/dwd_airport.csv")
dwd_airport <- dwd_airport %>% 
	mutate(dttm = ymd_hms(dttm))
dwd_urban <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/dwd_urban.csv")
dwd_urban <- dwd_urban %>% 
	mutate(dttm = ymd_hms(dttm))
uni_meteo <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/uni_meteo.csv")
uni_meteo <- uni_meteo %>% 
	mutate(dttm = ymd_hms(dttm))
wbi <- read.csv("https://raw.githubusercontent.com/jasperschalla/station_data/master/wbi.csv")
wbi <- wbi %>% 
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
