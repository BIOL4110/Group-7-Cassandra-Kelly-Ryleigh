PCB <- filter(trophic_Hg_PCB, grepl("chlorobiphenyl", parameter_name, fixed = TRUE) | 
                grepl("PCB", parameter_name, fixed = TRUE) | 
                grepl("(Cl)biphenyl", parameter_name, fixed = TRUE))
Hg<-filter(trophic_Hg_PCB, grepl("Mercury", parameter_name, fixed = TRUE))

Hg %>% ggplot(aes(TrophicAverage,value))+
  geom_jitter()+
  geom_smooth()

Hg %>% ggplot(aes(weight_gram,value))+
  geom_point()+
  geom_smooth()

PCB %>% ggplot(aes(weight_gram,value))+
  geom_point()+
  geom_smooth()

