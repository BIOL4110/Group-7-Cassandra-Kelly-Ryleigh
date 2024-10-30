
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

#filter for adult fish
trophic_level<-filter(Trophic_avg, SampleStage=="adults")

#merge contaminant dataset and trophic
trophic_level<-trophic_level %>% rename(species_name=FBname)

trophic_Hg_PCB<-merge(fish_Hg_PCB,trophic_level,by="species_name")


#Unit conversion: PCB Nanograms to picograms to micrograms
install.packages("measurements")
library(measurements)

Nano_to_pico <- Units %>%
  mutate( = case_when(unit, 
    Units == "Picogram per gram wet" ~ Units * 1000,
    Units == "Nanogram"  ~ Units,
    TRUE ~ NA_real_  # Handle unexpected units
  ))

# Convert all weights to grams

unit_fix <- trophic_Hg_PCB %>%
  mutate(value = case_when(
    unit == ("NANOGRAM PER WET GRAM") ~ value * 1000,
    unit == "PICOGRAM PER WET GRAM"  ~ value,
    TRUE ~ NA_real_  # Handle unexpected units
  )) 





