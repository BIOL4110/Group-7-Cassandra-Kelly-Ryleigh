#load fishbase tables again (new name)
species_1<-fb_tbl("species")

trophic_1<-fb_tbl("diet")

#make common names lower case
species_1$FBname<-tolower(species_1$FBname)

#cut down to required columns
species_2<-species_1 %>% select("SpecCode","Genus","Species","FBname")
trophic_2<-trophic_1 %>% select("Speccode","SampleStage","Troph")

#rename Speccode to match trophic_1
species_2<-species_2 %>% rename(Speccode=SpecCode)

#filter to only adult and juv/adult trophic levels
trophic_2<-filter(trophic_2, grepl("adult", SampleStage, fixed = TRUE))

#add genus (and species) to trophic_2
trophic_genus<-merge(trophic_2,species_2,by="Speccode")

#rename species_name to FBname
fish_Hg_PCB<-fish_Hg_PCB %>% rename(FBname=species_name)

#add genus (and species) to contaminant data
fish_genus<-merge(fish_Hg_PCB,species_2,by="FBname")

#averaging duplicate trophic levels
trophic_genus2 <- trophic_genus %>%
  group_by(Genus) %>%
  summarize(TrophicAverage = mean(Troph, na.rm = TRUE), .groups = 'drop')

#adding trophic average to contaminants data
fish_genus_troph<-merge(fish_genus,trophic_genus2,by="Genus")


##DON'T RUN THIS YET, THIS IS JUST ME MESSING AROUND!
#list of all fish in cleaned up contaminant dataset
fish_list<-unique(fish_Hg_PCB$FBname)

scientific_fish_list<-common_to_sci(c(fish_list))


