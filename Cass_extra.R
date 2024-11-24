library(ggplot2)

Normal distributed/log stuff (from Kelly)
#log transform data for normal dist
fish_TDL2<-fish_TDL %>% 
  mutate(log_value = log(value))
PCB2 <- filter(fish_TDL2, grepl("PCB", parameter_name, fixed = TRUE))
Hg2<-filter(fish_TDL2, grepl("Mercury", parameter_name, fixed = TRUE))

install.packages("dharma")

#New column for year to make sure it is seperate

#done with fish_TDL2 to get log transformed data
#New column for year (seperate from just date)

fish_TDL2<-fish_TDL2 %>%
  mutate(fish_TDL2, year(sample_date))

fish_TDL2<-fish_TDL2 %>% rename("(year(sample_date)" = "year")

fish_TDL2<- fish_TDL2 %>% 
  rename("year" = "year(sample_date)")

PCB2 <- PCB2 %>%
  mutate(PCB2, year(sample_date))

PCB2<- PCB2 %>% 
  rename("year" = "year(sample_date)")

Hg2 <- Hg2 %>%
  mutate(Hg2, year(sample_date))

Hg2 <- Hg2 %>%
  rename("year" = "year(sample_date)")


#qqplot
#Hg
qqnorm(Hg2$value)
qqline()

qqnorm(Hg2$log_value)
qqline()

Hg2
#PCB
PCB2<-filter(PCB2,value>0)

qqnorm(PCB2$value)
qqline()

qqnorm(PCB2$log_value)
qqline()

#Hypothesis 2

qqnorm(PCB2$year)
qqline()

qqnorm(Hg2$year)
qqline()



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
#OR
PCB_P1model<- lm(log_value ~ trophavg, data = PCB2)
summary(PCB_H1model)
#Insignificant

kruskal.test(data = Hg2, value ~ trophavg)
#significant


Hg_P1model<-lm(value ~ trophavg, data = Hg2)
summary(Hg_P1model)
#significant
#Don't need to do anova now because trophic level isn't grouped right?


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

PCB_P2model<-lm(value ~ lifespan, data = Hg2)
summary(PCB_P2model)
#insignificant


Hg2_P2 <- ggplot(Hg2, aes(lifespan, value))+
                  geom_point(alpha = 0.5, shape = 2) +
                  geom_smooth(method = "lm", se = FALSE) +
                  #labs
                  theme_bw()
Hg2_P2


kruskal.test(data = Hg2, value ~ lifespan)
#significant

Hg2_P2model<-lm(value ~ lifespan, data = Hg2)
summary(Hg2_P2model)
#insignificant

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

PCB_P3model<-lm(value ~ DemersPelag, data = PCB2)
summary(PCB_P3model)
#insignificant


Hg2_P3 <- ggplot(Hg2, aes(DemersPelag, value))+
  geom_point(alpha = 0.5, shape = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  #labs
  theme_bw()

Hg2_P3

kruskal.test(data = Hg2, value ~ DemersPelag)


Hg2_P3model<-lm(value ~ DemersPelag, data = Hg2)
summary(Hg2_P3model)
#insignificant


#Hypothesis 2
#Prediction 1:
#PCB
PCB_P4 <- ggplot(PCB2, aes(year, value)) +
  geom_smooth(method='lm') +
theme_bw()
#add labs
PCB_P4

#Test
PCB_model<-lm(value ~ year, data = PCB2)
summary(PCB_model)

  
#Mercury 
Hg_P4 <- ggplot(Hg2, aes(year, value)) +
  geom_smooth(method='lm') +
  theme_bw()
Hg_P4
  
Hg_model <- lm(value ~ year, data = Hg2)
summary(Hg_model)


##Marginal effects

#install.packages
install.packages("marginaleffects")
library("marginaleffects")

#Create linear model

mod1<-lm(value~trophavg, data=Hg)

predictions(mod1)

plot_cap()

marginaleffects(mod1) # slope for each row of the original data 

summary(mod1) #Average marginal effect

plot_cme() #Conditional marginal effects

marginalmeans()



##Useful links
#https://www.rdocumentation.org/packages/marginaleffects/versions/0.7.0
#https://www.sscc.wisc.edu/sscc/pubs/Rmisc/margins.html

