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
  geom_jitter(alpha=0.1) +
  ylab("PCB (ng/g wet)")

FarNorthHg<-add_column(FarNorthHg, ONTARIO="North")
SouthHg<-add_column(SouthHg, ONTARIO="South")

Hg<-rbind(FarNorthHg, SouthHg)

ggplot(Hg, aes(x=ONTARIO, y=VALUE)) +
  geom_jitter(alpha=0.5) +
  ylab("Hg (ug/g wet)")


GreatLakesTroph2 <- filter(GreatLakesHg, grepl("White Sucker", SPECIES_NAME, fixed = TRUE) | grepl("Spottail Shiner", SPECIES_NAME, fixed = TRUE) | grepl("Emerald Shiner", SPECIES_NAME, fixed = TRUE))
GreatLakesTroph2 <- add_column(GreatLakesTroph2, TROPHICLVL="2")

GreatLakesTroph3 <- filter(GreatLakesHg, grepl("Common Carp", SPECIES_NAME, fixed = TRUE) | grepl("Rock Bass", SPECIES_NAME, fixed = TRUE) | grepl("Black Crappie", SPECIES_NAME, fixed = TRUE))
GreatLakesTroph3 <- add_column(GreatLakesTroph3, TROPHICLVL="3")

GreatLakesTroph4 <- filter(GreatLakesHg, grepl("Northern Pike", SPECIES_NAME, fixed = TRUE) | grepl("Rainbow Trout", SPECIES_NAME, fixed = TRUE) | grepl("White Bass", SPECIES_NAME, fixed = TRUE))
GreatLakesTroph4 <- add_column(GreatLakesTroph4, TROPHICLVL="4")

GreatLakesTroph <- rbind(GreatLakesTroph2, GreatLakesTroph3, GreatLakesTroph4)

ggplot(GreatLakesTroph, aes(x=TROPHICLVL,y=VALUE)) +
  geom_jitter(alpha=0.1) +
  xlab("Trophic Level") +
  ylab("Mercury (ug/g wet)") +
  scale_x_discrete(labels=c("2"="Primary Consumers", "3"="Secondary Consumers", "4"="Tertiary Consumers"))


ggplot(PCB, aes(fill=ONTARIO, y=LENGTH_CM, x=SPECIES_NAME))+
  geom_bar(position="dodge", stat="identity")

ggplot(GreatLakesPCB, aes(x=VALUE, y=WEIGHT_GRAM))+
  geom_point()+
  facet_wrap("SPECIES_NAME")
#fish


