#packages
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("rfishbase")
library(rfishbase)
install.packages("lubridate")
library(lubridate)
install.packages("janitor")
library(janitor)
library(readxl)

#import contaminant data
Fish_Contaminant_Data_1993_1998 <- read_excel("Fish Contaminant Data 1993-1998.xlsx")
Fish_Contaminant_Data_1999_2001 <- read_excel("Fish Contaminant Data 1999-2001.xlsx")
Fish_Contaminant_Data_2002_2004 <- read_excel("Fish Contaminant Data 2002-2004.xlsx")
Fish_Contaminant_Data_2005_2018 <- read_excel("Fish Contaminant Data 2005-2018.xlsx")

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

#cleaning up and making dates = dates----
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

#combine datasets----
fish_1993_2018<-rbind(Fish_Contaminant_1,Fish_Contaminant_2,Fish_Contaminant_3,Fish_Contaminant_4)

#adding in North South designation----
North <- filter(fish_1993_2018, latitude_ddmmss>=490000)
South <- filter(fish_1993_2018, latitude_ddmmss<490000)
North<-add_column(North, ontario="North")
South<-add_column(South, ontario="South")

#adding North South column into combined dataset
fish_1993_2018<-rbind(North, South)

#remove unnecessary columns and NAs
fish_1993_2018$waterbody_code<-NULL
fish_1993_2018$location_description<-NULL
fish_1993_2018$field_collection_no<-NULL
fish_1993_2018$lims_sample_no<-NULL
fish_1993_2018$sample_portion_no<-NULL
fish_1993_2018$portion_type_code<-NULL
fish_1993_2018$test_code<-NULL

fish_1993_2018<-na.omit(fish_1993_2018)

#keep only Hg and PCB data----
PCB <- filter(fish_1993_2018, grepl("chlorobiphenyl", parameter_name, fixed = TRUE) | 
                grepl("PCB", parameter_name, fixed = TRUE) | 
                grepl("(Cl)biphenyl", parameter_name, fixed = TRUE))
Hg<-filter(fish_1993_2018, grepl("Mercury", parameter_name, fixed = TRUE))

#convert picograms to nanograms----
PCB<-PCB %>% 
  mutate(value = case_when(unit=="PICOGRAM PER GRAM WET" ~ value*0.001,
                           unit=="NANOGRAM" ~ value*1,
                           unit=="NANOGRAM PER GRAM WET" ~ value*1,
                           unit=="ng/g wet" ~ value*1))
#remove incorrect units
PCB$unit<-NULL
#add correct unit
PCB<-add_column(PCB, unit="ng/g wet")
#remove different types of PCB
PCB$parameter_name<-NULL
#add PCB as parameter
PCB<-add_column(PCB, parameter_name="PCB")

#combine Hg and PCB data
fish_Hg_PCB<-rbind(PCB, Hg)

#remove non-fish samples----
fish_Hg_PCB<-filter(fish_Hg_PCB, portion_type_desc!="NOT FISH SPECIES  OTHER ANIMAL")

#rename species_name to FBname
fish_Hg_PCB<-fish_Hg_PCB %>% rename(FBname=species_name)

#make common names lower case
fish_Hg_PCB$FBname<-tolower(fish_Hg_PCB$FBname)

#rename "wild-caught" data common names----
fish_Hg_PCB <- fish_Hg_PCB %>% 
  mutate(FBname = str_replace_all(FBname, "yellow perch", "american yellow perch"))

fish_Hg_PCB <- fish_Hg_PCB %>% 
  mutate(FBname = str_replace_all(FBname, "burbot (ling)", "burbot"))

fish_Hg_PCB <- fish_Hg_PCB %>% 
  mutate(FBname = str_replace_all(FBname, "largemouth bass", "largemouth black bass"))

fish_Hg_PCB <- fish_Hg_PCB %>% 
  mutate(FBname = str_replace_all(FBname, "brown trout", "sea trout"))

fish_Hg_PCB <- fish_Hg_PCB %>% 
  mutate(FBname = str_replace_all(FBname, "siscowet", "lake trout"))

fish_Hg_PCB <- fish_Hg_PCB %>% 
  mutate(FBname = str_replace_all(FBname, "humper (banker) lake trout", "lake trout"))

fish_Hg_PCB <- fish_Hg_PCB %>% 
  mutate(FBname = str_replace_all(FBname, "bowfin", "freshwater bream"))

fish_Hg_PCB <- fish_Hg_PCB %>% 
  mutate(FBname = str_replace_all(FBname, "aurora trout", "brook trout"))

#load fishbase tables----
species_1<-fb_tbl("species")
trophic_1<-fb_tbl("diet")

#make common names lower case
species_1$FBname<-tolower(species_1$FBname)

#cut down to required columns
species_1<-species_1 %>% select("SpecCode","Genus","Species","FBname")
trophic_1<-trophic_1 %>% select("Speccode","SampleStage","Troph")

#rename Speccode to match trophic_1
species_1<-species_1 %>% rename(Speccode=SpecCode)

#filter to only adult and juv/adult trophic levels
trophic_1<-filter(trophic_1, grepl("adult", SampleStage, fixed = TRUE))

#average trophic position by Speccode
trophic_2 <- trophic_1 %>%
  group_by(Speccode) %>%
  summarize(trophavg = mean(Troph, na.rm = TRUE), .groups = 'drop')

#add genus, species and Speccode to contaminant data
fish_genus<-merge(fish_Hg_PCB,species_1,by="FBname")

#add trophavg to contaminant data
fish_troph<-merge(fish_genus,trophic_2,by="Speccode")


#fishbase get depth info----
species_2<-fb_tbl("species")
depth<-species_2 %>% select("SpecCode","DemersPelag")
depth<-depth %>% rename(Speccode=SpecCode)
fish_T_D<-merge(fish_troph,depth,by="Speccode")

#age max----
popchar<-fb_tbl("popchar")
popchar2<-popchar %>% select("Speccode","tmax")

popchar2<-na.omit(popchar2)
popchar3 <- popchar2 %>%
  group_by(Speccode) %>%
  summarize(lifespan = max(tmax, na.rm = TRUE), .groups = 'drop')

#add age max to fish contaminant data
fish_TDL<-merge(fish_T_D,popchar3,by="Speccode")

#removing more unnecessary columns
fish_TDL$submission_no<-NULL
fish_TDL$species_code<-NULL
fish_TDL$TrophicAverage<-NULL

#trophic level bins
fish_TDL<-fish_TDL %>% 
  mutate(trophic_level = floor(trophavg))

#adding trophic average, trophic level, depth, and lifespan to Hg and PCB----
PCB <- filter(fish_TDL, grepl("PCB", parameter_name, fixed = TRUE))
Hg<-filter(fish_TDL, grepl("Mercury", parameter_name, fixed = TRUE))


