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



