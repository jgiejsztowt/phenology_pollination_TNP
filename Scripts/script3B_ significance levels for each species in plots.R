#script3B_ significance levels for each species in plots
rm(list=ls())

setwd("C:/Users/Giejszju/Desktop/pollination")


#read in data and libraries
ff2017 <-read.csv("./Data/2016_2017_phenology_first flower.csv")
ff2016 <- read.csv("./Data/2015_2016_phenology_first flower.csv")
fa <- read.csv("./Data/2016_2017_phenology_abundance_peak.csv")

###########################################################
###first flowering responses 2016-2017#####################
###########################################################
ff<-ff2017

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
fflim<-fflim[fflim$Species!="",]
fflim$Species<-factor(fflim$Species)

warmed<-fflim[fflim$Warmed=="W",]
ambient<-fflim[fflim$Warmed=="A",]

shapiro.test(fflim[fflim$Species=="CALVUL",5])
shapiro.test(fflim[fflim$Species=="CELGLA",5])
shapiro.test(fflim[fflim$Species=="CELGRA",5])
shapiro.test(fflim[fflim$Species=="CELSPE",5])
shapiro.test(fflim[fflim$Species=="DRAREC",5])
shapiro.test(fflim[fflim$Species=="DRASUB",5])
shapiro.test(fflim[fflim$Species=="DROFRA",5])
shapiro.test(fflim[fflim$Species=="EPAALP",5])
shapiro.test(fflim[fflim$Species=="EUPCUN",5])
shapiro.test(fflim[fflim$Species=="OZOLEP",5])
shapiro.test(fflim[fflim$Species=="PENPUM",5])
shapiro.test(fflim[fflim$Species=="PIMHIR",5])
shapiro.test(fflim[fflim$Species=="WAHPYG",5])

ttestCV<-wilcox.test(warmed[warmed$Species=="CALVUL",5],ambient[ambient$Species=="CALVUL",5])
ttestCGL<-wilcox.test(warmed[warmed$Species=="CELGLA",5],ambient[ambient$Species=="CELGLA",5])
ttestCGR<-wilcox.test(warmed[warmed$Species=="CELGRA",5],ambient[ambient$Species=="CELGRA",5])
ttestCS<-wilcox.test(warmed[warmed$Species=="CELSPE",5],ambient[ambient$Species=="CELSPE",5])
ttestDR<-wilcox.test(warmed[warmed$Species=="DRAREC",5],ambient[ambient$Species=="DRAREC",5])
ttestDS<-wilcox.test(warmed[warmed$Species=="DRASUB",5],ambient[ambient$Species=="DRASUB",5])
ttestDF<-wilcox.test(warmed[warmed$Species=="DROFRA",5],ambient[ambient$Species=="DROFRA",5])
ttestEP<-wilcox.test(warmed[warmed$Species=="EPAALP",5],ambient[ambient$Species=="EPAALP",5])
ttestEC<-wilcox.test(warmed[warmed$Species=="EUPCUN",5],ambient[ambient$Species=="EUPCUN",5])
ttestOV<-wilcox.test(warmed[warmed$Species=="OZOLEP",5],ambient[ambient$Species=="OZOLEP",5])
ttestPP<-t.test(warmed[warmed$Species=="PENPUM",5],ambient[ambient$Species=="PENPUM",5])
ttestPM<-wilcox.test(warmed[warmed$Species=="PIMHIR",5],ambient[ambient$Species=="PIMHIR",5])
ttestWP<-wilcox.test(warmed[warmed$Species=="WAHPYG",5],ambient[ambient$Species=="WAHPYG",5])


ttestCV
ttestCGL
ttestCGR
ttestCS
ttestDR
ttestDS
ttestDF
ttestEP
ttestEC
ttestOV
ttestPP
ttestPM
ttestWP


###########################################################
###first flowering responses 2015-2016#####################
###########################################################
ff<-ff2016

fflim<-ff[ff$Species!="copche",]
fflim<-fflim[fflim$Species!="copper",]
fflim<-fflim[fflim$Species!="drofra",]
fflim<-fflim[fflim$Species!="hyprad",]
fflim<-fflim[fflim$Species!="lepsco",]
fflim<-fflim[fflim$Species!="leufra",]### ones and twos
fflim<-fflim[fflim$Species!="pracol",]
fflim<-fflim[fflim$Species!="raoalb",]
fflim$Species<-factor(fflim$Species)


shapiro.test(fflim[fflim$Species=="calvul",5])
shapiro.test(fflim[fflim$Species=="celgla",5])
shapiro.test(fflim[fflim$Species=="celgra",5])
shapiro.test(fflim[fflim$Species=="celspe",5])
shapiro.test(fflim[fflim$Species=="chipal",5])
shapiro.test(fflim[fflim$Species=="drarec",5])
shapiro.test(fflim[fflim$Species=="drasub",5])
shapiro.test(fflim[fflim$Species=="epaalp",5])
shapiro.test(fflim[fflim$Species=="eupcun",5])
shapiro.test(fflim[fflim$Species=="hebtet",5])
shapiro.test(fflim[fflim$Species=="ozolep",5])
shapiro.test(fflim[fflim$Species=="penpum",5])
shapiro.test(fflim[fflim$Species=="wahpyg",5])

warmed<-fflim[fflim$Warm=="W",]
ambient<-fflim[fflim$Warm=="A",]                          
                        
wilcox.test(warmed[warmed$Species=="calvul",5],ambient[ambient$Species=="calvul",5])
wilcox.test(warmed[warmed$Species=="celgla",5],ambient[ambient$Species=="celgla",5])
t.test(warmed[warmed$Species=="celgra",5],ambient[ambient$Species=="celgra",5])
t.test(warmed[warmed$Species=="celspe",5],ambient[ambient$Species=="celspe",5])
wilcox.test(warmed[warmed$Species=="chipal",5],ambient[ambient$Species=="chipal",5])
t.test(warmed[warmed$Species=="drarec",5],ambient[ambient$Species=="drarec",5])
wilcox.test(warmed[warmed$Species=="drasub",5],ambient[ambient$Species=="drasub",5])
wilcox.test(warmed[warmed$Species=="epaalp",5],ambient[ambient$Species=="epaalp",5])
t.test(warmed[warmed$Species=="eupcun",5],ambient[ambient$Species=="eupcun",5])
wilcox.test(warmed[warmed$Species=="hebtet",5],ambient[ambient$Species=="hebtet",5])
wilcox.test(warmed[warmed$Species=="ozolep",5],ambient[ambient$Species=="ozolep",5])
wilcox.test(warmed[warmed$Species=="penpum",5],ambient[ambient$Species=="penpum",5])
wilcox.test(warmed[warmed$Species=="wahpyg",5],ambient[ambient$Species=="wahpyg",5])


#####################################################################################
#######################ABUNDANCE#####################################################
#####################################################################################

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

warmed<-fflim[fflim$Warmed=="W",]
ambient<-fflim[fflim$Warmed=="A",]

shapiro.test(fflim[fflim$Species=="CALVUL",5])
shapiro.test(fflim[fflim$Species=="CELGLA",5])
shapiro.test(fflim[fflim$Species=="CELGRA",5])
shapiro.test(fflim[fflim$Species=="CELSPE",5])
shapiro.test(fflim[fflim$Species=="DRAREC",5])
shapiro.test(fflim[fflim$Species=="DRASUB",5])
shapiro.test(fflim[fflim$Species=="DROFRA",5])
shapiro.test(fflim[fflim$Species=="EPAALP",5])
shapiro.test(fflim[fflim$Species=="EUPCUN",5])
shapiro.test(fflim[fflim$Species=="OZOLEP",5])
shapiro.test(fflim[fflim$Species=="PENPUM",5])
shapiro.test(fflim[fflim$Species=="PIMHIR",5])
shapiro.test(fflim[fflim$Species=="WAHPYG",5])

wilcox.test(warmed[warmed$Species=="CALVUL",5],ambient[ambient$Species=="CALVUL",5])
wilcox.test(warmed[warmed$Species=="CELGLA",5],ambient[ambient$Species=="CELGLA",5])
wilcox.test(warmed[warmed$Species=="CELGRA",5],ambient[ambient$Species=="CELGRA",5])
wilcox.test(warmed[warmed$Species=="CELSPE",5],ambient[ambient$Species=="CELSPE",5])
wilcox.test(warmed[warmed$Species=="DRAREC",5],ambient[ambient$Species=="DRAREC",5])
wilcox.test(warmed[warmed$Species=="DRASUB",5],ambient[ambient$Species=="DRASUB",5])
wilcox.test(warmed[warmed$Species=="DROFRA",5],ambient[ambient$Species=="DROFRA",5])
wilcox.test(warmed[warmed$Species=="EPAALP",5],ambient[ambient$Species=="EPAALP",5])
wilcox.test(warmed[warmed$Species=="EUPCUN",5],ambient[ambient$Species=="EUPCUN",5])
wilcox.test(warmed[warmed$Species=="OZOLEP",5],ambient[ambient$Species=="OZOLEP",5])
wilcox.test(warmed[warmed$Species=="PENPUM",5],ambient[ambient$Species=="PENPUM",5])
wilcox.test(warmed[warmed$Species=="PIMHIR",5],ambient[ambient$Species=="PIMHIR",5])
wilcox.test(warmed[warmed$Species=="WAHPYG",5],ambient[ambient$Species=="WAHPYG",5])
