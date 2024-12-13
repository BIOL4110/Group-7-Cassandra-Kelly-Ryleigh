Hg2 %>% ggplot(aes(trophavg,log_value))+
                 geom_jitter(alpha=0.1)+
                 ylab("Log of Mercury (ug/g wet)")+
                 xlab("Trophic Level")+
                 theme_bw()+
                 theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

Hg2 %>% ggplot(aes(trophavg,value))+
  geom_jitter(alpha=0.1)+
  ylab("Log of Mercury (ug/g wet)")+
  xlab("Trophic Level")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


figure1<-plot(margin7b)
figure2<-plot(margin7a)
##didn't work
grid.arrange(figure2, figure1)




#model fitting PCB
modela<-lm(log_value~trophavg+DemersPelag+lifespan+year, data=PCB3, na.action="na.fail")
#model Hg
modelb<-lm(log_value~trophavg+DemersPelag+lifespan+year, data=Hg3, na.action="na.fail")




install.packages("visreg")
library(visreg)
visreg(model7a)
visreg(model7b)

#attempt at margins with scale models... DIDN'T WORK
install.packages("margins")
library(margins)
margin1a<-margins(scale_model7a)
summary(margin7a)
plot(margin7a)
margin1b<-margins(scale_model7b)
summary(margin7b)
plot(margin7b)



#inverse transformation----
fish_TDL4<-fish_TDL3 %>% 
  mutate(inverse_value = 1/(value))

PCB4 <- filter(fish_TDL4, grepl("PCB", parameter_name, fixed = TRUE))
Hg4<-filter(fish_TDL4, grepl("Mercury", parameter_name, fixed = TRUE))


#visualize data
PCB4 %>% ggplot(aes(x=inverse_value))+
  geom_histogram()
Hg4 %>% ggplot(aes(x=inverse_value))+
  geom_histogram()
##both worse than log transformation


#root transformation----
fish_TDL4<-fish_TDL3 %>% 
  mutate(root_value = sqrt(value))

PCB4 <- filter(fish_TDL4, grepl("PCB", parameter_name, fixed = TRUE))
Hg4<-filter(fish_TDL4, grepl("Mercury", parameter_name, fixed = TRUE))


#visualize data
PCB4 %>% ggplot(aes(x=root_value))+
  geom_histogram()
Hg4 %>% ggplot(aes(x=root_value))+
  geom_histogram()
##both worse than log transformation

install.packages("pracma")
library(pracma)

#cube root transformation----
fish_TDL4<-fish_TDL4 %>% 
  mutate(cuberoot_value = nthroot((value),3))

PCB4 <- filter(fish_TDL4, grepl("PCB", parameter_name, fixed = TRUE))
Hg4<-filter(fish_TDL4, grepl("Mercury", parameter_name, fixed = TRUE))


#visualize data
PCB4 %>% ggplot(aes(x=cuberoot_value))+
  geom_histogram()
Hg4 %>% ggplot(aes(x=cuberoot_value))+
  geom_histogram()
##not better

#fourth root transformation----
fish_TDL4<-fish_TDL4 %>% 
  mutate(fourthroot_value = nthroot((value),4))

PCB4 <- filter(fish_TDL4, grepl("PCB", parameter_name, fixed = TRUE))
Hg4<-filter(fish_TDL4, grepl("Mercury", parameter_name, fixed = TRUE))


#visualize data
PCB4 %>% ggplot(aes(x=fourthroot_value))+
  geom_histogram()
Hg4 %>% ggplot(aes(x=fourthroot_value))+
  geom_histogram()
##not better


#3rd inverse transformation----
fish_TDL4<-fish_TDL4 %>% 
  mutate(thirdinverse_value = 1/((value)^3))

PCB4 <- filter(fish_TDL4, grepl("PCB", parameter_name, fixed = TRUE))
Hg4<-filter(fish_TDL4, grepl("Mercury", parameter_name, fixed = TRUE))


#visualize data
PCB4 %>% ggplot(aes(x=inverse_value))+
  geom_histogram()
Hg4 %>% ggplot(aes(x=inverse_value))+
  geom_histogram()
##not better



#inverse log transformation----
fish_TDL4<-fish_TDL4 %>% 
  mutate(inverselog_value = 1/log(value))

PCB4 <- filter(fish_TDL4, grepl("PCB", parameter_name, fixed = TRUE))
Hg4<-filter(fish_TDL4, grepl("Mercury", parameter_name, fixed = TRUE))


#visualize data
PCB4 %>% ggplot(aes(x=inverselog_value))+
  geom_histogram()
Hg4 %>% ggplot(aes(x=inverselog_value))+
  geom_histogram()
##not better



#log10 transformation----
fish_TDL4<-fish_TDL4 %>% 
  mutate(log10_value = log10(value))

PCB4 <- filter(fish_TDL4, grepl("PCB", parameter_name, fixed = TRUE))
Hg4<-filter(fish_TDL4, grepl("Mercury", parameter_name, fixed = TRUE))


#visualize data
PCB4 %>% ggplot(aes(x=log10_value))+
  geom_histogram()
Hg4 %>% ggplot(aes(x=log10_value))+
  geom_histogram()
##not better


#exp transformation----
fish_TDL4<-fish_TDL4 %>% 
  mutate(exp_value = exp(value))

PCB4 <- filter(fish_TDL4, grepl("PCB", parameter_name, fixed = TRUE))
Hg4<-filter(fish_TDL4, grepl("Mercury", parameter_name, fixed = TRUE))


#visualize data
PCB4 %>% ggplot(aes(x=exp_value))+
  geom_histogram()
Hg4 %>% ggplot(aes(x=exp_value))+
  geom_histogram()
##bad


#model fitting Hg----
model7b<-lm(log_mercury_concentration~trophic_level+habitat+lifespan+year, data=Hg2, na.action="na.fail")
#dredge
ms1<-dredge(model7b)
#average of top 2 models
summary(model.avg(ms1,subset=delta<4))
model7b_avg<-model.avg(ms1,subset=delta<4)

#visualize averaged model
visreg(model7b_avg)
##error: object 'log_mercury_concentration' not found


library(ggplot2)

#lm with species and latitude
model8a<-lm(log_PCB_concentration~trophic_level+habitat+lifespan+year+FBname+latitude_ddmmss, data=PCB2, na.action="na.fail")
model8b<-lm(log_mercury_concentration~trophic_level+habitat+lifespan+year+FBname+latitude_ddmmss, data=Hg2, na.action="na.fail")
#dredge 'em
dredge(model8a)
dredge(model8b)

#lm with species and location
model9a<-lm(log_PCB_concentration~trophic_level+habitat+lifespan+year+FBname+location_name, data=PCB2, na.action="na.fail")
model9b<-lm(log_mercury_concentration~trophic_level+habitat+lifespan+year+FBname+location_name, data=Hg2, na.action="na.fail")
#dredge 'em
dredge(model9a)
dredge(model9b)
##long drawn-out non-sense


#better visreg graphs without border
visreg(model7b, gg=T)[[1]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Trophic Level")+
  geom_point(alpha=0.1)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

visreg(model7b, gg=T, points=T)[[2]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Habitat")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

visreg(model7b, gg=T)[[3]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Lifespan (years)")+
  geom_point(alpha=0.1)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

visreg(model7b, gg=T)[[4]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Year Sampled")+
  geom_point(alpha=0.1)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))


#better visreg graphs with border Hg
visreg(model7b, gg=T)[[1]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Trophic Level")+
  geom_point(alpha=0.1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

visreg(model7b, gg=T, points=T)[[2]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Habitat")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

visreg(model7b, gg=T)[[3]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Lifespan (years)")+
  geom_point(alpha=0.1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

visreg(model7b, gg=T)[[4]]+
  labs(y="Log of Mercury Concentration (ug/g wet)", x="Year Sampled")+
  geom_point(alpha=0.1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))


#better visreg graphs with border PCB
visreg(model7a, gg=T)[[1]]+
  labs(y="Log of PCB Concentration (ng/g wet)", x="Trophic Level")+
  geom_point(alpha=0.1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

visreg(model7a, gg=T, points=T)[[2]]+
  labs(y="Log of PCB Concentration (ng/g wet)", x="Habitat")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

visreg(model7a, gg=T)[[3]]+
  labs(y="Log of PCB Concentration (ng/g wet)", x="Lifespan (years)")+
  geom_point(alpha=0.1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

visreg(model7a, gg=T)[[4]]+
  labs(y="Log of PCB Concentration (ng/g wet)", x="Year Sampled")+
  geom_point(alpha=0.1)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))

