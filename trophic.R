
install.packages("taxa")
library(taxa)

#simplifying down to only needed columns and renaming SpecCode
trophic_fbase<-trophic_fbase %>% select("Speccode","SampleStage","Troph") %>% 
  rename(SpecCode=Speccode)

species_fbase<-species_fbase %>% select("SpecCode","FBname")

#merging into one table
trophic_species <- merge(species_fbase,trophic_fbase, by  = "SpecCode") 

#now what?

Trophic_avg <- trophic_species %>%
  group_by(SpecCode, SampleStage) %>%
  summarize(TrophicAverage = mean(Troph, na.rm = TRUE), .groups = 'drop')


#Maybe need to tidy the data? Perhaps it is best to just average the trophic levels for each species
#to make things simpler? (Done above)
