fish_PCB<-filter(fish_genus_troph, grepl("chlorobiphenyl", parameter_name, fixed = TRUE) | 
                   grepl("PCB", parameter_name, fixed = TRUE) | 
                   grepl("(Cl)biphenyl", parameter_name, fixed = TRUE))


fish_PCB2<-fish_PCB %>% 
  mutate(value = case_when(unit=="PICOGRAM PER GRAM WET" ~ value*0.001,
                           unit=="NANOGRAM" ~ value*1,
                           unit=="NANOGRAM PER GRAM WET" ~ value*1,
                           unit=="ng/g wet" ~ value*1))

fish_PCB2$unit<-NULL
fish_PCB2<-add_column(fish_PCB2, unit="ng/g wet")

