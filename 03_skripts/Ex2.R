# Description
# Data Quality check


# ---- load libraries ----
library(tidyverse)
library(lubridate)


# ---- load raw data ----
# Surface
data <- read_csv(file = 'C:/Users/radtk/OneDrive/Dokumente/Data_management_storage/data_management_hobo/02_data_processed/10347355.csv',  col_names = T) 

# Stand PC
data <- read_csv(file = 'A:/radtk/OneDrive/Dokumente/Data_management_storage/data_management_hobo/02_data_processed/10347355.csv',  col_names = T) 
