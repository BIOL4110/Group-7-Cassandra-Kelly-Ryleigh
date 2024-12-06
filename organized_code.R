#packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rfishbase)
library(lubridate)
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
Fish_Contaminant_Data_1993_1998 <- Fish_Contaminant_Data_1993_1998 %>% 
  clean_names() %>% 
  mutate(sample_date = ymd(sample_date))

Fish_Contaminant_Data_1999_2001 <- Fish_Contaminant_Data_1999_2001 %>% 
  clean_names() %>% 
  mutate(sample_date = ymd(sample_date))

Fish_Contaminant_Data_2002_2004 <- Fish_Contaminant_Data_2002_2004 %>% 
  clean_names() %>% 
  mutate(sample_date = ymd(sample_date))

Fish_Contaminant_Data_2005_2018 <- Fish_Contaminant_Data_2005_2018 %>% 
  clean_names() %>% 
  mutate(sample_date = dmy(sample_date))

#combine datasets----
fish_1993_2018<-rbind(Fish_Contaminant_Data_1993_1998,Fish_Contaminant_Data_1999_2001,Fish_Contaminant_Data_2002_2004,Fish_Contaminant_Data_2005_2018)

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

##contaminants fish list
#"white sucker","yellow perch","lake whitefish","northern pike","walleye","burbot (ling)","lake trout","sucker family","smallmouth bass","sauger","mooneye","brook trout","sturgeon","cisco (lake herring)","black crappie","longnose sucker","blackfin cisco","shortjaw cisco","channel catfish","freshwater drum","white bass","coho salmon","rainbow trout","brown bullhead","common carp","largemouth bass","white perch","brown trout","chinook salmon","common shiner","bigmouth buffalo","rock bass","round whitefish","american eel","rainbow smelt","redhorse sucker","bluegill","pumpkinseed","white crappie","goldfish","siscowet","humper (banker) lake trout","atlantic salmon","pink salmon","bloater","bowfin","chub (not c. artedii)","splake","muskellunge","salmon hybrid","aurora trout","whitefish hybrid","gizzard shad","shorthead redhorse","silver redhorse","goldeye","fallfish","golden redhorse sucker","spotted sucker","golden shiner"
##missing fish once combined with FBtrophic levels
#"white sucker",______________,"lake whitefish","northern pike","walleye",_______________,"lake trout",_______________,"smallmouth bass","sauger","mooneye","brook trout","sturgeon",______________________,"black crappie","longnose sucker","blackfin cisco","shortjaw cisco","channel catfish",_________________,"white bass","coho salmon","rainbow trout","brown bullhead","common carp",_________________,"white perch",_____________,"chinook salmon","common shiner",__________________,"rock bass","round whitefish","american eel","rainbow smelt",_________________,"bluegill","pumpkinseed","white crappie","goldfish",__________,____________________________,"atlantic salmon","pink salmon","bloater",________,_______________________,________,"muskellunge",_______________,______________,__________________,______________,____________________,_________________,"goldeye","fallfish",________________________,________________,"golden shiner"  
##problems
#"sucker family","cisco (lake herring)","freshwater drum","bigmouth buffalo","redhorse sucker","chub (not c. artedii)","splake","salmon hybrid","whitefish hybrid","gizzard shad","shorthead redhorse","silver redhorse","golden redhorse sucker","spotted sucker"
##names to be fixed
#"yellow perch","burbot (ling)","largemouth bass","brown trout","siscowet","humper (banker) lake trout","bowfin","aurora trout"

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

#adding trophic average, trophic level, depth, and lifespan to Hg and PCB----
PCB <- filter(fish_TDL, grepl("PCB", parameter_name, fixed = TRUE))
Hg<-filter(fish_TDL, grepl("Mercury", parameter_name, fixed = TRUE))

#prep data for log transformation
fish_TDL<-fish_TDL %>% mutate(ifelse(value=="0","0.000001",value))
fish_TDL$value<-NULL
fish_TDL<-fish_TDL %>% rename(value=`ifelse(value == "0", "0.000001", value)`)
fish_TDL<-fish_TDL %>% transform(value=as.numeric(value))

#log transform data
fish_TDL2<-fish_TDL %>% 
  mutate(log_value = log(value))

#year column added
fish_TDL2<-fish_TDL2 %>%
  mutate(fish_TDL2, year(sample_date))
fish_TDL2<- fish_TDL2 %>% 
  rename("year" = "year(sample_date)")

#rename some columns
fish_TDL2<-fish_TDL2 %>% rename(trophic_level=trophavg)
fish_TDL2<-fish_TDL2 %>% rename(habitat=DemersPelag)

#year and log in new contaminant sets
PCB2 <- filter(fish_TDL2, grepl("PCB", parameter_name, fixed = TRUE))
Hg2<-filter(fish_TDL2, grepl("Mercury", parameter_name, fixed = TRUE))

#change column names for log_value
PCB2<-PCB2 %>% rename(log_PCB_concentration=log_value)
Hg2<-Hg2 %>% rename(log_mercury_concentration=log_value)

#model fitting PCB
model7a<-lm(log_PCB_concentration~trophic_level+habitat+lifespan+year, data=PCB2, na.action="na.fail")
#model Hg
model7b<-lm(log_mercury_concentration~trophic_level+habitat+lifespan+year, data=Hg2, na.action="na.fail")

#dredge confirm best lm ----
install.packages("MuMIn")
library(MuMIn)
dredge(model7a)
dredge(model7b)

#checking model assumptions Hg
#install.packages("performance")
#library(performance)
#check_model(model7b)
#plot(model7b)
#summary(model7b)

#checking model assumptions PCB
#check_model(model7a)
#plot(model7a)
#summary(model7a)

##Marginal effects

#install.packages
#install.packages("margins")
#library(margins)
#margin7a<-margins(model7a)
#summary(margin7a)
#plot(margin7a)
#margin7b<-margins(model7b)
#summary(margin7b)
#plot(margin7b)

#change demersal to reference group----
fish_TDL3 <- fish_TDL2 %>% 
  mutate(habitat = str_replace_all(habitat, "demersal", "ademersal"))

#scale function
fish_TDL3<-fish_TDL3 %>%
  mutate(scaled_trophic_level=scale(trophic_level, center = TRUE))
fish_TDL3<-fish_TDL3 %>%
  mutate(scaled_lifespan=scale(lifespan, center = TRUE))
fish_TDL3<-fish_TDL3 %>%
  mutate(scaled_year=scale(year, center = TRUE))

PCB3 <- filter(fish_TDL3, grepl("PCB", parameter_name, fixed = TRUE))
Hg3<-filter(fish_TDL3, grepl("Mercury", parameter_name, fixed = TRUE))

#scaled models
scale_model7a<-lm(log_value~scaled_trophic_level+habitat+scaled_lifespan+scaled_year, data=PCB3, na.action="na.fail")
scale_model7b<-lm(log_value~scaled_trophic_level+habitat+scaled_lifespan+scaled_year, data=Hg3, na.action="na.fail")
summary(scale_model7a)
summary(scale_model7b)

#visualize models----
install.packages("visreg")
library(visreg)
visreg(model7a)
visreg(model7b)

#better visreg graphs with border Hg
Hg_graph1<-visreg(model7b, gg=T)[[1]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Trophic Level")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

Hg_graph2<-visreg(model7b, gg=T, points=T)[[2]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Habitat")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

Hg_graph3<-visreg(model7b, gg=T)[[3]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Lifespan (years)")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

Hg_graph4<-visreg(model7b, gg=T)[[4]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Year Sampled")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))


#better visreg graphs with border PCB
PCB_graph1<-visreg(model7a, gg=T)[[1]]+
  labs(y="Log of PCB Concentration (ng/g wet)", x="Trophic Level")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

PCB_graph2<-visreg(model7a, gg=T, points=T)[[2]]+
  labs(y="Log of PCB Concentration (ng/g wet)", x="Habitat")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

PCB_graph3<-visreg(model7a, gg=T)[[3]]+
  labs(y="Log of PCB Concentration (ng/g wet)", x="Lifespan (years)")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

PCB_graph4<-visreg(model7a, gg=T)[[4]]+
  labs(y="Log of PCB Concentration (ng/g wet)", x="Year Sampled")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

#pair graphs in panel
library(gridExtra)

grid.arrange(Hg_graph1,PCB_graph1, nrow=1)
grid.arrange(Hg_graph2,PCB_graph2, nrow=1)
grid.arrange(Hg_graph3,PCB_graph3, nrow=1)
grid.arrange(Hg_graph4,PCB_graph4, nrow=1)

#get unlogged values for slope
#PCB
#year
exp(1.67130)
#lifespan
exp(-0.16724)
#trophic level
exp(0.10658)
#benthopelag
exp(0.03858)
#pelag
exp(-0.56732)
#pelag-neritic
exp(1.17833)
#pelag-oceanic
exp(0.89299)

#Hg
#year
exp(0.138256)
#lifespan
exp(-0.004027)
#trophic
exp(0.435831)
#bentho
exp(0.536542)
#pelag
exp(0.694841)
#pelag-neritic
exp(0.043402)
#pelag-oceanic
exp(-0.842412)



