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

grid.arrange(figure2, figure1)




#model fitting PCB
modela<-lm(log_value~trophavg+DemersPelag+lifespan+year, data=PCB3, na.action="na.fail")
#model Hg
modelb<-lm(log_value~trophavg+DemersPelag+lifespan+year, data=Hg3, na.action="na.fail")


install.packages("visreg")
library(visreg)
visreg(model7a)
visreg(model7b)
