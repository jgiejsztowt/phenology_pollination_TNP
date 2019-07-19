###Script1_plantspecificcontrols_seed weight
#set up space
rm(list=ls())
setwd("C:/Users/Giejszju/Desktop/pollination")

#import data and packages
sw<-read.csv("./Data/seed weights.csv")
library(nlme)


#cutting down into treatments
e<-sw[sw$Treatment=="E",]
n<-sw[sw$Treatment=="N",]



############################################################################################################################
#########################looking at factors affecting seed weight in h######################################################
############################################################################################################################
h<-sw[sw$Treatment=="H",]


h1<-lm(log(weight) ~ Flower.number.on.shrub.at.pollination*Ht..mm., data=h)
summary(h1)

h2<-update(h1, .~.-Flower.number.on.shrub.at.pollination:Ht..mm.)
summary(h2)

h3<-update(h2, .~.-Flower.number.on.shrub.at.pollination)
summary(h3) ####only height is determinative of seed weight.
#final model of landscape effects on seed weight include coefficient from here 
par(mfrow=c(2,2)); plot(h3)

par(mfrow=c(1,1)); plot(log(h$weight)~h$Ht..mm.)
abline(h3)

