Fish_Contaminant_Data_2005_2018 %>% 
  ggplot(aes(x=SAMPLE_DATE, y=VALUE, color=LOCATION_NAME)) + 
  geom_point(alpha=0.2) + 
  facet_wrap(~SPECIES_NAME) + 
  show.legend = FALSE

library(tidyverse)
library(dplyr)
library(ggplot2)
  
LakeErie <- filter(Fish_Contaminant_Data_2005_2018, grepl("Lake Erie", LOCATION_NAME, fixed = TRUE))
LakeHuron <- filter(Fish_Contaminant_Data_2005_2018, grepl("Lake Huron", LOCATION_NAME, fixed = TRUE))
LakeMichigan <- filter(Fish_Contaminant_Data_2005_2018, grepl("Lake Michigan", LOCATION_NAME, fixed = TRUE))
LakeOntario <- filter(Fish_Contaminant_Data_2005_2018, grepl("Lake Ontario", LOCATION_NAME, fixed = TRUE))
LakeSuperior <- filter(Fish_Contaminant_Data_2005_2018, grepl("Lake Superior", LOCATION_NAME, fixed = TRUE))
GreatLakes <- filter(Fish_Contaminant_Data_2005_2018, grepl("Lake Erie", LOCATION_NAME, fixed = TRUE) | grepl("Lake Huron", LOCATION_NAME, fixed = TRUE) | grepl("Lake Michigan", LOCATION_NAME, fixed = TRUE) | grepl("Lake Ontario", LOCATION_NAME, fixed = TRUE) | grepl("Lake Superior", LOCATION_NAME, fixed = TRUE))


FarNorth <- filter(Fish_Contaminant_Data_2005_2018, LATITUDE_DDMMSS>=500000)
South <- filter(Fish_Contaminant_Data_2005_2018, LATITUDE_DDMMSS<500000)

FarNorthPCB <- filter(FarNorth, PARAMETER_NAME=="PCB; total")
FarNorthHg <- filter(FarNorth, PARAMETER_NAME=="Mercury")

GreatLakesPCB <- filter(GreatLakes, PARAMETER_NAME=="PCB; total")
GreatLakesHg <- filter(GreatLakes, PARAMETER_NAME=="Mercury")

SouthPCB <- filter(South, PARAMETER_NAME=="PCB; total")
SouthHg <- filter(South, PARAMETER_NAME=="Mercury")

ggplot(FarNorthPCB, aes(x=PARAMETER_NAME, y=VALUE)) +
  geom_boxplot()

ggplot(SouthPCB, aes(x=PARAMETER_NAME, y=VALUE)) +
  geom_boxplot()

FarNorthPCB<-add_column(FarNorthPCB, ONTARIO="North")
SouthPCB<-add_column(SouthPCB, ONTARIO="South")

PCB<-rbind(FarNorthPCB, SouthPCB)

ggplot(PCB, aes(x=ONTARIO, y=VALUE)) +
  geom_jitter(alpha=0.5) 

PCBno_out <- filter(PCB, VALUE<35000)

ggplot(PCBno_out, aes(x=ONTARIO, y=VALUE)) +
  geom_jitter(alpha=0.5) +
  ylab("PCB (ng/g wet)")

FarNorthHg<-add_column(FarNorthHg, ONTARIO="North")
SouthHg<-add_column(SouthHg, ONTARIO="South")

Hg<-rbind(FarNorthHg, SouthHg)

ggplot(Hg, aes(x=ONTARIO, y=VALUE)) +
  geom_jitter(alpha=0.5) +
  ylab("Hg (ug/g wet)")




