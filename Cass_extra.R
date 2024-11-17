library(ggplot2)

##Hypothesis 1
#Prediction 1 - Trophic Level vs Contaminant Concentration 
#PCB
#Figures
PCB_P1 <- ggplot(PCB2, aes(trophavg, value))+
  geom_point(alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  #labs
theme_bw()

PCB_P1

#Hg
Hg_P1 <- ggplot(Hg2, aes(trophavg, value))+
  geom_point(alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  #labs
  theme_bw()
Hg_P1

#Test
kruskal.test(data = PCB2, value ~ trophavg)
#significant results

kruskal.test(data = Hg2, value ~ trophavg)
#significant

##Prediction 2 - Lifespan vs concentration
##Figures 
PCB_P2 <- ggplot(PCB2, aes(lifespan, value))+
  geom_point(alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  #labs
  theme_bw()

PCB_P2

#Tests
kruskal.test(data = PCB2, value ~ lifespan)
#significant

Hg2_P2 <- ggplot(Hg2, aes(lifespan, value))+
                  geom_point(alpha = 0.5, shape = 2) +
                  geom_smooth(method = "lm", se = FALSE) +
                  #labs
                  theme_bw()
Hg2_P2

kruskal.test(data = Hg2, value ~ lifespan)
#significant

#Prediction 3 
PCB_P3 <- ggplot(PCB2, aes(DemersPelag, value))+
  geom_point(alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  #labs
  theme_bw()

PCB_P3

##make axis based on the order of actual lake depth

#Test
kruskal.test(data = PCB2, value ~ DemersPelag)

Hg2_P3 <- ggplot(Hg2, aes(DemersPelag, value))+
  geom_point(alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  #labs
  theme_bw()

Hg2_P3

kruskal.test(data = Hg2, value ~ DemersPelag)



