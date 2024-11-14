#redo trophic position by Speccode
trophic_2 <- trophic_1 %>%
  group_by(Speccode) %>%
  summarize(trophavg = mean(Troph, na.rm = TRUE), .groups = 'drop')

fish_troph<-merge(fish_genus,trophic_2,by="Speccode")



species_2<-fb_tbl("species")

#fishbase get depth info
depth<-species_2 %>% select("SpecCode","DemersPelag")
depth<-depth %>% rename(Speccode=SpecCode)
fish_T_D<-merge(fish_troph,depth,by="Speccode")

PCB <- filter(fish_TDL, grepl("chlorobiphenyl", parameter_name, fixed = TRUE) | 
                grepl("PCB", parameter_name, fixed = TRUE) | 
                grepl("(Cl)biphenyl", parameter_name, fixed = TRUE))
Hg<-filter(fish_TDL, grepl("Mercury", parameter_name, fixed = TRUE))


#age max
popchar2<-popchar %>% select("Speccode","tmax")

popchar2<-na.omit(popchar2)
popchar3 <- popchar2 %>%
  group_by(Speccode) %>%
  summarize(lifespan = max(tmax, na.rm = TRUE), .groups = 'drop')

#add age max to fish contaminant data
fish_TDL<-merge(fish_T_D,popchar3,by="Speccode")


#linear regression models PCB
model1a<-lm(value~trophavg, data=PCB)
model2a<-lm(value~DemersPelag, data=PCB)
model3a<-lm(value~lifespan, data=PCB)
model4a<-lm(value~DemersPelag+lifespan, data=PCB)
model5a<-lm(value~trophavg+DemersPelag, data=PCB)
model6a<-lm(value~trophavg+DemersPelag+lifespan, data=PCB)

#calculate AIC
AIC(model1a, model2a, model3a, model4a, model5a, model6a)

#linear regression models Hg
model1b<-lm(value~trophavg, data=Hg)
model2b<-lm(value~DemersPelag, data=Hg)
model3b<-lm(value~lifespan, data=Hg)
model4b<-lm(value~DemersPelag+lifespan, data=Hg)
model5b<-lm(value~trophavg+DemersPelag, data=Hg)
model6b<-lm(value~trophavg+DemersPelag+lifespan, data=Hg)

#calculate AIC
AIC(model1b, model2b, model3b, model4b, model5b, model6b)

#qqplot
qqnorm(Hg$value)
qqnorm(PCB$value)


#trophic level bins
fish_TDL<-fish_TDL %>% 
  mutate(trophic_level = floor(trophavg))






#playing with graphs
Hg %>% ggplot(aes(trophavg,value))+
  geom_jitter(alpha=0.1)+
  ylab("Mercury (ug/g wet)")+
  xlab("Trophic Level")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

PCB %>% ggplot(aes(trophavg,value))+
  geom_jitter(alpha=0.1)+
  ylab("PCB (ng/g wet)")+
  xlab("Trophic Level")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

Hg %>% ggplot(aes(sex,value))+
  geom_jitter(alpha=0.1)+
  ylab("Mercury (ug/g wet)")+
  xlab("Sex")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

PCB %>% ggplot(aes(sex,value))+
  geom_jitter(alpha=0.1)+
  ylab("PCB (ng/g wet)")+
  xlab("Sex")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



Hg %>% ggplot(aes(lifespan,value))+
  geom_jitter(alpha=0.1)+
  ylab("Mercury (ug/g wet)")+
  xlab("Lifespan (years)")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

PCB %>% ggplot(aes(lifespan,value))+
  geom_jitter(alpha=0.1)+
  ylab("PCB (ng/g wet)")+
  xlab("Lifespan (years)")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



Hg %>% ggplot(aes(sample_date,value))+
  geom_jitter(alpha=0.1)+
  ylab("Mercury (ug/g wet)")+
  xlab("Date")+
  theme_bw()+
  geom_smooth()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

PCB %>% ggplot(aes(sample_date,value))+
  geom_jitter(alpha=0.1)+
  ylab("PCB (ng/g wet)")+
  xlab("Date")+
  theme_bw()+
  geom_smooth()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



GreatLakes <- filter(fish_genus, grepl("Lake Erie", location_name, fixed = TRUE) | grepl("Lake Huron", location_name, fixed = TRUE) | grepl("Lake Michigan", location_name, fixed = TRUE) | grepl("Lake Ontario", location_name, fixed = TRUE) | grepl("Lake Superior", location_name, fixed = TRUE))

GreatLakes_Hg<-filter(GreatLakes, grepl("Mercury", parameter_name, fixed = TRUE))

GreatLakes_PCB <- filter(GreatLakes, grepl("chlorobiphenyl", parameter_name, fixed = TRUE) | 
                grepl("PCB", parameter_name, fixed = TRUE) | 
                grepl("(Cl)biphenyl", parameter_name, fixed = TRUE))

GreatLakes_Hg %>% ggplot(aes(TrophicAverage,value))+
  geom_jitter(alpha=0.1)+
  ylab("Mercury (ug/g wet)")+
  xlab("Trophic Level")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

GreatLakes_PCB %>% ggplot(aes(TrophicAverage,value))+
  geom_jitter(alpha=0.1)+
  ylab("PCB (ng/g wet)")+
  xlab("Trophic Level")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))




