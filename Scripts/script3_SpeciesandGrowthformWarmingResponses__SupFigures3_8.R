###Supplementary figures to 8


##setting up space
rm(list=ls())
setwd("C:/Users/Giejszju/Desktop/pollination")

##Importing data and packages
library(ggplot2)
library(nlme)

ff2017 <-read.csv("./Data/2016_2017_phenology_first flower.csv")
ff2016 <- read.csv("./Data/2015_2016_phenology_first flower.csv")
fa <- read.csv("./Data/2016_2017_phenology_abundance_peak.csv")

###excluding species which are underrrepresented in the dataset


##############################################################################################
##########################2016/2017 austral summer first flower ##############################
##############################################################################################

ff <- ff2017
fflim<-ff[ff$Species!="COPPER",]
fflim<-fflim[fflim$Species!="GAUCOL",]
fflim<-fflim[fflim$Species!="GONMIC",]
fflim<-fflim[fflim$Species!="HAIRY ROSETTE",]
fflim<-fflim[fflim$Species!="LEPSCO",]
fflim<-fflim[fflim$Species!="LEUFRA",]### ones and twos
fflim<-fflim[fflim$Species!="ASTER",]
fflim<-fflim[fflim$Species!="CHIPAL",]
fflim<-fflim[fflim$Species!="HEBVEN",]
fflim<-fflim[fflim$Species!="ORCHID",] ####threes
fflim<-fflim[fflim$Species!="HEBTET",]
fflim$Species<-factor(fflim$Species)

p1<-ggplot(fflim, aes(x=Species, y=Days, fill=Warmed)) + 
  geom_boxplot()+
  scale_x_discrete(labels=c("Calluna vulgaris (W)",
                                            "Celmisia glabulosa (F)", 
                                            "Celmisia gracilenta (F)", 
                                            "Celmisia spectabilis (F)",
                                            "Dracophyllum recurvum (W)",
                                            "Dracophyllum subulatum (W)",
                                            "Drosera fraserii (F)",
                                            "Epacris alpina (W)",
                                            "Euphrasia cuneata (F)",
                                            "Ozothamnus vauvilliersii (W)",
                                            "Pentachondra pumila (W)",
                                            "Pimeala microphylla (W)",
                                            "Wahlenbergia pygamaea (F)"))+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values=c("steelblue3", "tomato2"), labels=c("Ambient", "Experimental warming"))+
  ylab("Days past astronomical spring (2017)")+ 
  theme(legend.position="top")+
  scale_y_continuous(breaks=seq(64,187,20))+ 
  theme_light()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  theme(legend.position="top")+ 
  theme(legend.title=element_blank())+ 
  geom_text(x=1, y=183, label="*", size=15)+ 
  geom_text(x=2, y=184, label="NS", size=10)+ 
  geom_text(x=3, y=184, label="NS", size=10)+ 
  geom_text(x=4, y=184, label="NS", size=10)+ 
  geom_text(x=5, y=184, label="NS", size=10)+ 
  geom_text(x=6, y=184, label="NS", size=10)+ 
  geom_text(x=7, y=184, label="NS", size=10)+ 
  geom_text(x=8, y=184, label="NS", size=10)+ 
  geom_text(x=9, y=183, label="*", size=15)+ 
  geom_text(x=10, y=184, label="NS", size=10)+ 
  geom_text(x=11, y=184, label="NS", size=10)+ 
  geom_text(x=12, y=184, label="NS", size=10)+ 
  geom_text(x=13, y=184, label="NS", size=10)+
  theme(text = element_text(size=30))


p2<-ggplot(fflim, aes(x=Growth.form, y=Days, fill=Warmed)) + 
  geom_boxplot()+ 
  scale_fill_manual(values=c("steelblue3", "tomato2"), labels=c("Ambient", "Experimental warming"))+
  ylab("Days past astronomical spring (2017)")+ 
  theme(legend.position="top")+
  theme(text = element_text(size=30))+ 
  theme_light()+
  scale_x_discrete(labels=c("Forb","Woody shrub")) +
  xlab("Growth form")+ 
  scale_y_continuous(breaks=seq(64,187,20))+ 
  theme(legend.title=element_blank())+ 
  theme(legend.position="top")+ 
  geom_text(x=1, y=164, label="NS", size=30)



p2.5<-ggplot(fflim, aes(x=Growth.form, y=Days, fill=Warmed)) + 
  geom_boxplot(lwd = 2)+ 
  scale_fill_manual(values=c("steelblue3", "tomato2"), labels=c("Ambient", "Experimental warming"))+
  ylab("Days past astronomical spring (2016)")+ 
  theme_light() +
  xlab("Growth form")+ 
  scale_y_continuous(breaks=seq(64,187,20))+ 
  geom_text(x=1, y=184, label="NS", size=20)+ 
  geom_text(x=2, y=184, label="NS", size=20)+
  scale_x_discrete(labels=c("Forb","Woody shrub"))+ 
  theme(legend.position="top")+ 
  theme(legend.title=element_blank())+
  theme(text = element_text(size=40))+ theme(legend.text=element_text(size=40))



###significance testing 
forbs2016<-lme(Days~Warmed, random=~1|Species, 
         data=fflim[fflim$Growth.form=="forb",]) 
anova(forbs2016) ###NEED


woody2016<-lme(Days~Warmed, random=~1|Species, 
         data=fflim[fflim$Growth.form=="woody",]) 

anova(woody2016) ####WRONG

ds2016<-t.test(Days~Warmed, data=fflim[fflim$Species=="DRASUB",]) #NEED
cv2016<-t.test(Days~Warmed, data=fflim[fflim$Species=="CALVUL",]) #NEED


##############################################################################################
##########################2015/2016 austral summer first flower ##############################
##############################################################################################
ff<-ff2016
colnames(ff)[5]<-"Days.from.Spring"


fflim<-ff[ff$Species!="copche",]
fflim<-fflim[fflim$Species!="copper",]
fflim<-fflim[fflim$Species!="drofra",]
fflim<-fflim[fflim$Species!="hyprad",]
fflim<-fflim[fflim$Species!="lepsco",]
fflim<-fflim[fflim$Species!="leufra",]### ones and twos
fflim<-fflim[fflim$Species!="pracol",]
fflim<-fflim[fflim$Species!="raoalb",]
fflim2<-fflim[fflim$Species!="hebtet",]
fflim2<-fflim2[fflim2$Species!="chipal",]

fflim$Species<-factor(fflim$Species)


p3<-ggplot(fflim2, aes(x=Species, y=Days.from.Spring, fill=Warm)) + 
  geom_boxplot()+
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values=c("steelblue3", "tomato2"), labels=c("Ambient", "Experimental warming"))+
  ylab("Days past astronomical spring (2016)")+ 
  theme(legend.position="top")+
  scale_x_discrete(labels=c("Calluna vulgaris (W)",
                            "Celmisia glabulosa (F)", 
                            "Celmisia gracilenta (F)", 
                            "Celmisia spectabilis (F)",
                            "Dracophyllum recurvum (W)",
                            "Dracophyllum subulatum (W)",
                            "Epacris alpina (W)",
                            "Euphrasia cuneata (F)",
                            "Ozothamnus vauvilliersii (W)",
                            "Pentachondra pumila (W)",
                            "Wahlenbergia pygamaea (F)"))+
  scale_y_continuous(breaks=seq(64,187,20))+ 
  theme_light()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  theme(legend.position="top")+ 
  theme(legend.title=element_blank())+ 
  geom_text(x=1, y=173, label="**", size=15)+ 
  geom_text(x=2, y=174, label="NS", size=10)+ 
  geom_text(x=3, y=174, label="NS", size=10)+ 
  geom_text(x=4, y=174, label="NS", size=10)+ 
  geom_text(x=5, y=174, label="NS", size=10)+ 
  geom_text(x=6, y=174, label="NS", size=10)+ 
  geom_text(x=7, y=174, label="NS", size=10)+ 
  geom_text(x=8, y=174, label="NS", size=10)+ 
  geom_text(x=9, y=174, label="NS", size=10)+ 
  geom_text(x=10, y=174, label="NS", size=10)+ 
  geom_text(x=11, y=174, label="NS", size=10)+
  theme(text = element_text(size=30))

    
fflim2<-fflim[fflim$Growth.form!="C3 grass",]    

p4<-ggplot(fflim2, aes(x=Growth.form, y=Days.from.Spring, fill=Warm)) + 
  geom_boxplot(lwd = 2)+ 
  scale_fill_manual(values=c("steelblue3", "tomato2"), labels=c("Ambient", "Experimental warming"))+
  ylab("Days past astronomical spring (2016)")+ 
  theme_light() +
  xlab("Growth form")+ 
  geom_text(x=1, y=184, label="NS", size=20)+ 
  geom_text(x=2, y=184, label="NS", size=20)+
  scale_x_discrete(labels=c("Forb","Woody shrub"))+ 
  theme(legend.position="top")+ 
  theme(legend.title=element_blank())+
  theme(text = element_text(size=40))+ theme(legend.text=element_text(size=40))+
  scale_y_continuous(breaks=seq(64,187,20),limits = c(64,187))




###NEED
forbs2015<-lme(Days.from.Spring~Warm, random=~1|Species, 
         data=fflim[fflim$Growth.form=="Forb",]) 
anova(forbs2015)
woody2015<-lme(Days.from.Spring~Warm, random=~1|Species, 
         data=fflim[fflim$Growth.form=="Woody",]) 
anova(woody2015)
ds2015<-t.test(Days.from.Spring~Warm, data=fflim[fflim$Species=="drasub",])
cv2015<-t.test(Days.from.Spring~Warm, data=fflim[fflim$Species=="calvul",])
ds2015 ####NEED
ds2016####NEED
cv2015####NEED
cv2016 ####NEED





##############################################################################################
########################## abundance #########################################################
##############################################################################################
ff<-fa

fflim<-ff[ff$Species!="ASTER",]
fflim<-fflim[fflim$Species!="CHIPAL",]
fflim<-fflim[fflim$Species!="COPPER",]
fflim<-fflim[fflim$Species!="GAUCOL",]
fflim<-fflim[fflim$Species!="GONMIC",]
fflim<-fflim[fflim$Species!="HAIRY ROSETTE",]### ones-threes
fflim<-fflim[fflim$Species!="HEBVEN",]
fflim<-fflim[fflim$Species!="LEPSCO",]
fflim<-fflim[fflim$Species!="LEUFRA",]
fflim<-fflim[fflim$Species!="ORCHID",]
fflim<-fflim[fflim$Species!="HEBTET",]
fflim$Species<-factor(fflim$Species)

p5<-ggplot(fflim, aes(x=Species, y=Days, fill=Warmed)) + 
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_fill_manual(values=c("steelblue3", "tomato2"), labels=c("Ambient", "Experimental warming"))+
  ylab("Days past astronomical spring (2017)")+ 
  theme(legend.position="top")+
scale_x_discrete(labels=c("Calluna vulgaris (W)",
                          "Celmisia glabulosa (F)", 
                          "Celmisia gracilenta (F)", 
                          "Celmisia spectabilis (F)",
                          "Dracophyllum recurvum (W)",
                          "Dracophyllum subulatum (W)",
                          "Drosera fraserii (F)",
                          "Epacris alpina (W)",
                          "Euphrasia cuneata (F)",
                          "Ozothamnus vauvilliersii (W)",
                          "Pentachondra pumila (W)",
                          "Pimeala microphylla (W)",
                          "Wahlenbergia pygamaea (F)"))+
  theme(text = element_text(size=30))+
  scale_y_continuous(breaks=seq(64,187,20))+ 
  theme_light()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  theme(legend.position="top")+ 
  theme(legend.title=element_blank())+ 
  geom_text(x=1, y=183, label="*", size=15)+ 
  geom_text(x=2, y=184, label="NS", size=10)+ 
  geom_text(x=3, y=184, label="NS", size=10)+ 
  geom_text(x=4, y=184, label="NS", size=10)+ 
  geom_text(x=5, y=184, label="NS", size=10)+ 
  geom_text(x=6, y=183, label="*", size=15)+ 
  geom_text(x=7, y=184, label="NS", size=10)+ 
  geom_text(x=8, y=184, label="NS", size=10)+ 
  geom_text(x=9, y=183, label="*", size=15)+ 
  geom_text(x=10, y=184, label="NS", size=10)+ 
  geom_text(x=11, y=184, label="NS", size=10)+ 
  geom_text(x=12, y=184, label="NS", size=10)+ 
  geom_text(x=13, y=184, label="NS", size=10)+
  theme(text = element_text(size=30))

p6<-ggplot(fflim, aes(x=Growth.form, y=Days, fill=Warmed)) + 
  geom_boxplot(lwd = 2)+ 
  scale_fill_manual(values=c("steelblue3", "tomato2"), labels=c("Ambient", "Experimental warming"))+
  ylab("Days past astronomical spring (2017)")+ 
  theme_light() +
  xlab("Growth form")+ 
  scale_y_continuous(breaks=seq(64,187,20))+ 
  geom_text(x=1, y=184, label="NS", size=20)+ 
  geom_text(x=2, y=184, label="***", size=20)+
  scale_x_discrete(labels=c("Forb","Woody shrub"))+ 
  theme(legend.position="top")+ 
  theme(legend.title=element_blank())+
  theme(text = element_text(size=40))+ theme(legend.text=element_text(size=40))

forbs_abund<-lme(Days~Warmed, random=~1|Species, 
               data=fflim[fflim$Growth.form=="forb",]) 
anova(forbs_abund)

woody_abund<-lme(Days~Warmed, random=~1|Species, 
               data=fflim[fflim$Growth.form=="woody",]) 
anova(woody_abund)

ds_abund<-t.test(Days~Warmed, data=fflim[fflim$Species=="DRASUB",])
cv_abund<-t.test(Days~Warmed, data=fflim[fflim$Species=="CALVUL",])
ds_abund
cv_abund


p1 #2016first flowerspp
p2#2015first flowerspp
p3#2016first flowergf
p4#2015first flowergf
p5#2016abundflowerspp
p6#2016abundflowergf