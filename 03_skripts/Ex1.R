# Description
# Read raw data from mx hobo


# ---- load libraries ----
library(tidyverse)
library(lubridate)

# ---- load raw data ----
# Surface
data_raw <- read_csv(file = 'C:/Users/radtk/OneDrive/Dokumente/Data_management_storage/data_management_hobo/01_data_raw/hobo/2022/raw/10347355.csv', skip = 1, col_names = T) 

# Stand PC
data_raw <- read_csv(file = 'A:/radtk/OneDrive/Dokumente/Data_management_storage/data_management_hobo/01_data_raw/hobo/2022/raw/10347355.csv', skip = 1, col_names = T) 


#janitor::clean_names(data) if skip = 1

col_names <- c('id', 'dttm', 'temp', 'lux', 'battery','attached', 'detached', 'host', 'stopped', 'eof')
names(data_raw) <- col_names


# ---- save data with clean names ----
#Surface
write_csv(x=data_raw, file = 'C:/Users/radtk/OneDrive/Dokumente/Data_management_storage/data_management_hobo/02_data_processed/10347355_cleannames.csv')

# Stand PC
write_csv(x=data_raw, file = 'A:/radtk/OneDrive/Dokumente/Data_management_storage/data_management_hobo/02_data_processed/10347355_cleannames.csv')



# ---- look at the data ----
plot(x = data$id, y = data$temp, type = 'l') #temp
plot(x = data$id, y = data$lux, type = 'l') #lux


# # ---- choose needed collums ----
# data_raw_4c <- data_raw %>% 
# 	mutate(dttm = dmy_hms(dttm)) %>% 
# 	mutate(dttm_2 = substring(dttm, 1, 10),
# 	       dttm_2 = ymd(dttm_2),
# 	       dttm_2 = ymd(dttm_2)) %>% 
# 	mutate(timestamp = substring(dttm, 12, 19),
# 	       timestamp = hms(timestamp)) %>% 
# 	filter(dttm < '2022-01-09 23:50:10') %>% 
# 	select(id, dttm, temp, lux) 
# tail(data_raw_4c)


# ---- filter data to 4 full weeks ----

data <- data_raw %>% 
	select(id, dttm, temp, lux) %>% 
	mutate(dttm = dmy_hms(dttm, quiet = T)) %>% 
	mutate(date = date(dttm)) %>% 
	mutate(timestamp = substring(dttm, 12, 19),
	       timestamp = hms(timestamp)) %>% 
	filter('2021-12-12' < date & date < '2022-01-10') %>%
	mutate(id = row_number(id)) %>% 
	mutate(dttm = format(dttm, '%Y-%m-%d %H:%M:%S')) %>% 
	select(id, dttm, temp, lux)

# ---- save data with clean names ----
#Surface
write_csv(x=data, file = 'C:/Users/radtk/OneDrive/Dokumente/Data_management_storage/data_management_hobo/02_data_processed/10347355.csv')

#Stand-PC
write_csv(x=data, file = 'A:/radtk/OneDrive/Dokumente/Data_management_storage/data_management_hobo/02_data_processed/10347355.csv')










# Tagesmittel_grafik <- data %>% 
# 	ggplot(aes(x = dttm, y = temp)) + theme_bw(base_size = 15) +
# 	geom_line(color = "darkblue") +
# 	labs(title = "Abfluss im Tagesmittel",
# 	     #subtitle = Name,
# 	     x = "Datum",
# 	     y = "Abfluss [mÂ³/s]")




