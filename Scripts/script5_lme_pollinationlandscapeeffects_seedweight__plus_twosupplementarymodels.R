#script5_lme_pollinationlandscapeeffects_seedweight__plus_twosupplementarymodels


#set up space
rm(list=ls())
setwd("C:/Users/Giejszju/Desktop/pollination")

#import data and packages
sw<-read.csv("./Data/seed weights.csv")
library(nlme)


########adjustment of datasets for plant height###

sw$weight.adj<- 0
str(sw)

for (i in 1:nrow(sw)) {
  n <-  log(as.numeric(sw[i,6]))-0.0005004*sw[i,7]
  sw$weight.adj[i] <- n
}  ####based on outputs from script 1



######################################################################
############ MAIN MANUSCRIPT MODEL: landscape effects on pollination
sw2<-sw[sw$Treatment=="N",]
sw2$Proportion.native<-as.numeric(as.character(sw2$Proportion.native))
sw2$Average.number.of.flowers<-as.numeric(as.character(sw2$Average.number.of.flowers.on.25m.transect))
sw$weight.adj2<-as.numeric(sw$weight.adj2)

m2s<-lme(weight.adj~Proportion.native*Average.number.of.flowers, 
         random=~1|Date/Latitude, 
         data=sw2)
plot(m2s)
summary(m2s)


##############################################################################
#####secondary model: adjustment of datasets for local hand pollinated weight
h<-sw[sw$Treatment=="H",]
mwbt<-aggregate(h[, 6], list(h$Trial), mean)### calculating mean by trial

names(mwbt)[names(mwbt) == "Group.1"] <- "Trial"
names(mwbt)[names(mwbt) == "x"] <- "h.wt" ###reformatting for merge

sw3 <- merge(mwbt, sw2, by = 'Trial') #merging

sw3$weight.adj2<- 0

for (i in 1:nrow(sw3)) {
  n <-  sw3[i,2]-sw3[i,16]
  sw3$weight.adj2[i] <- n
} #####adjustment for hand pollinated weight



m3s<-lme(weight.adj2~Proportion.native*Average.number.of.flowers, 
         random=~1|Date/Latitude, 
         data=sw3)
summary(m3s)
plot(m3s)

##############################################################################
#####secondary model: limiting to exlcude trials that are close to one another


sw3<-sw2[sw2$Trial!=2,]
sw3<-sw3[sw3$Trial!=8,]
sw3<-sw3[sw3$Trial!=9,]
nrow(sw2);nrow(sw3)


m4s<-lme(weight.adj~Proportion.native*Average.number.of.flowers, 
         random=~1|Date/Latitude, 
         data=sw3)
summary(m4s)

