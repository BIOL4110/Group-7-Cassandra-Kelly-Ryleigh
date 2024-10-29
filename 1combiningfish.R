#getting fishbase tables we want
library(rfishbase)

species_fbase<-fb_tbl("species")
fb_tables()

trophic_fbase<-fb_tbl("diet")

#remove remark columns----
Fish_Contaminant_Data_1993_1998$REMARK1<-NULL
Fish_Contaminant_Data_1993_1998$REMARK2<-NULL
Fish_Contaminant_Data_1993_1998$REMARK3<-NULL
Fish_Contaminant_Data_1993_1998$REMARK_CODE<-NULL

Fish_Contaminant_Data_1999_2001$REMARK1<-NULL
Fish_Contaminant_Data_1999_2001$REMARK2<-NULL
Fish_Contaminant_Data_1999_2001$REMARK3<-NULL
Fish_Contaminant_Data_1999_2001$REMARK_CODE<-NULL

Fish_Contaminant_Data_2002_2004$REMARK1<-NULL
Fish_Contaminant_Data_2002_2004$REMARK2<-NULL
Fish_Contaminant_Data_2002_2004$REMARK3<-NULL
Fish_Contaminant_Data_2002_2004$REMARK_CODE<-NULL

Fish_Contaminant_Data_2005_2018$REMARK1<-NULL
Fish_Contaminant_Data_2005_2018$REMARK_CODE<-NULL

#cleaning up and making dates = dates
library(lubridate)
install.packages("janitor")
library(janitor)
library(tidyverse)

Fish_Contaminant_1 <- Fish_Contaminant_Data_1993_1998 %>% 
  clean_names() %>% 
  mutate(sample_date = ymd(sample_date))

Fish_Contaminant_2 <- Fish_Contaminant_Data_1999_2001 %>% 
  clean_names() %>% 
  mutate(sample_date = ymd(sample_date))

Fish_Contaminant_3 <- Fish_Contaminant_Data_2002_2004 %>% 
  clean_names() %>% 
  mutate(sample_date = ymd(sample_date))

Fish_Contaminant_4 <- Fish_Contaminant_Data_2005_2018 %>% 
  clean_names() %>% 
  mutate(sample_date = dmy(sample_date))

#combine datasets
fish_1993_2018<-rbind(Fish_Contaminant_1,Fish_Contaminant_2,Fish_Contaminant_3,Fish_Contaminant_4)

#adding in North South designation
North <- filter(fish_1993_2018, latitude_ddmmss>=490000)
South <- filter(fish_1993_2018, latitude_ddmmss<490000)
North<-add_column(North, ONTARIO="North")
South<-add_column(South, ONTARIO="South")

#adding North South column into combined dataset
fish_1993_2018<-rbind(North, South)




