
install.packages("taxa")
library(taxa)

#simplifying down to only needed columns and renaming SpecCode
trophic_fbase<-trophic_fbase %>% select("Speccode","SampleStage","Troph") %>% 
  rename(SpecCode=Speccode)

species_fbase<-species_fbase %>% select("SpecCode","FBname")

#merging into one table
trophic_species <- merge(species_fbase,trophic_fbase, by  = "SpecCode") 

#now what?

