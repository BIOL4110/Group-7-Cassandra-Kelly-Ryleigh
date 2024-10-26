#remove unnecessary columns and NAs
fish_simplified<-fish_1993_2018

fish_simplified$waterbody_code<-NULL
fish_simplified$location_description<-NULL
fish_simplified$field_collection_no<-NULL
fish_simplified$lims_sample_no<-NULL
fish_simplified$sample_portion_no<-NULL
fish_simplified$portion_type_code<-NULL
fish_simplified$test_code<-NULL

fish_no_na<-na.omit(fish_simplified)

#keep only Hg and PCB data
unique(fish_no_na$parameter_name)
PCB <- filter(fish_no_na, grepl("chlorobiphenyl", parameter_name, fixed = TRUE) | 
                grepl("PCB", parameter_name, fixed = TRUE) | 
                grepl("(Cl)biphenyl", parameter_name, fixed = TRUE))
Hg<-filter(fish_no_na, grepl("Mercury", parameter_name, fixed = TRUE))

fish_Hg_PCB<-rbind(PCB, Hg)

unique(fish_Hg_PCB$species_name)

not_fish<-filter(fish_Hg_PCB, grepl("OTHER ANIMAL", portion_type_desc, fixed = TRUE))

unique(fish_Hg_PCB$portion_type_desc)
fish_Hg_PCB<-filter(fish_Hg_PCB, portion_type_desc!="NOT FISH SPECIES  OTHER ANIMAL")

