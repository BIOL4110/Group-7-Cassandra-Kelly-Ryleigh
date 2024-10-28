
install.packages("taxa")
library(taxa)

#simplifying down to only needed columns and renaming SpecCode
trophic_fbase<-trophic_fbase %>% select("Speccode","SampleStage","Troph") %>% 
  rename(SpecCode=Speccode)

species_fbase<-species_fbase %>% select("SpecCode","FBname")

#merging into one table
trophic_species <- merge(species_fbase,trophic_fbase, by  = "SpecCode") 

#averaging duplicate trophic levels
Trophic_avg <- trophic_species %>%
  group_by(SpecCode, SampleStage, FBname) %>%
  summarize(TrophicAverage = mean(Troph, na.rm = TRUE), .groups = 'drop')

#make our two datasets have lower case 
Trophic_avg$FBname<-tolower(Trophic_avg$FBname)

fish_Hg_PCB$species_name<-tolower(fish_Hg_PCB$species_name)

