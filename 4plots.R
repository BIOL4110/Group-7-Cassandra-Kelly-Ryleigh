PCB <- filter(fish_genus_troph, grepl("chlorobiphenyl", parameter_name, fixed = TRUE) | 
                grepl("PCB", parameter_name, fixed = TRUE) | 
                grepl("(Cl)biphenyl", parameter_name, fixed = TRUE))
Hg<-filter(fish_genus_troph, grepl("Mercury", parameter_name, fixed = TRUE))

Hg %>% ggplot(aes(TrophicAverage,value))+
  geom_jitter(alpha=0.1)+
  ylab("Mercury (ug/g wet)")+
  xlab("Trophic Level")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

Hg %>% ggplot(aes(weight_gram,value))+
  geom_point()+
  geom_smooth()

PCB %>% ggplot(aes(weight_gram,value))+
  geom_point()+
  geom_smooth()

ggplot(fish_genus_troph, aes(x=ONTARIO, y=value)) +
  geom_jitter(alpha=0.5)