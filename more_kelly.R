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

