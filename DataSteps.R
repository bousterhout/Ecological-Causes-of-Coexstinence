########################
# Data processing
# 12.19.17
# BHO and MS
########################

source('Functions.R')
packages <- c('reshape2', 'plyr', 'dplyr', 'ggplot2')
package.check(packages)

#Read in data from experiment
start <- read.csv("Data/ExperimentalTreatments.csv")
start$Cage <- as.factor(start$Cage)
colnames(start)[6:8] <- c("ENTR.Abun.Init", "ENVE.Abun.Init", "ENEX.Abun.Init")
start[,c(9:10)] <- list(NULL)


final <- read.csv("Data/FinalSize.csv")
final$Cage <- as.factor(final$Cage)
final$OWPL <- as.numeric(final$OWPL)
colnames(final)[7] <-"I_F"
final[,8]<-NULL

#Aggregate measurement data
init.hw <- subset (final, I_F == "I")
final.hw <-subset(final, I_F == "F")

summary <- init.hw %>%
  group_by(Lake, Species) %>%
summarise(avg = mean(HW), sd = sd(HW))

summary$lakesp <- paste(summary$Lake, summary$Species, sep="_")

#Quick summary plot of average HW by lake and species
ggplot(summary) +
  geom_point(aes(x = lakesp, y = avg, color = Lake)) +
  geom_errorbar(aes(x = lakesp, ymax = avg + sd, ymin = avg - sd, colour = Lake), width = 0)+
  scale_x_discrete(labels = summary$Species)+
  xlab("Species")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Calculate the mean lnHW and lnOWPL for each Lake, Cage, Species

init.hw$lnHW <- log(init.hw$HW)
init.hw$lnOWPL <- log(init.hw$OWPL)
init.hw.mean <- aggregate(init.hw[c("lnHW", "lnOWPL")], by =init.hw[c('Lake', 'Species')], FUN = 'mean')
init.hw.mean[,4] <- NULL

final.hw$lnHW <- log(final.hw$HW)
final.hw$lnOWPL <- log(final.hw$OWPL)
final.hw.mean <- aggregate(final.hw[c("lnHW", "lnOWPL")], by =final.hw[c('Lake', 'Cage', 'Species')], FUN = 'mean')
final.hw.mean[,5] <- NULL

final.count <- aggregate(final.hw["lnHW"], by =final.hw[c('Lake', 'Cage', 'Species')], FUN = 'length')
colnames(final.count)[4] <-"Count"

final<-plyr::join(final.hw.mean, final.count, by=c('Lake','Cage','Species'), type='left')

df.l <-plyr::join(final, init.hw.mean, by=c('Lake', 'Species'), type='left')
colnames(df.l)[c(4,6)] <- c("Final.lnHw", "Init.lnHW")

df.l$lnHW.dif <- df.l$Final.lnHw - df.l$Init.lnHW
df.w<-dcast(df.l, Lake + Cage ~ Species, value.var="lnHW.dif")
df.w.count <-dcast(df.l, Lake + Cage ~ Species, value.var="Count")
df.w<-plyr::join(df.w,df.w.count, by=c('Lake','Cage'), type='left')
colnames(df.w)[6:8] <- c("EX.ct", "TR.ct", "VE.ct")

df.w$Sp1 <- ifelse(is.na(df.w$ENEX) | is.na(df.w$ENVE), "ENTR", "ENEX")
df.w$Sp2 <- ifelse(is.na(df.w$ENEX), "ENVE", 
                            ifelse(is.na(df.w$ENTR), "ENVE", "ENEX"))

df.w$Sp1.lnHW.dif <- ifelse(df.w$Sp1 == "ENEX", df.w$ENEX, df.w$ENTR)
df.w$Sp2.lnHW.dif <- ifelse(df.w$Sp2 == "ENEX", df.w$ENEX, df.w$ENVE)

df.w$Sp1.CT <- ifelse(df.w$Sp1 == "ENEX", df.w$EX.ct, df.w$TR.ct)
df.w$Sp2.CT <- ifelse(df.w$Sp2 == "ENEX", df.w$EX.ct, df.w$VE.ct)

df.w[,c(3:8)] <-list(NULL)



df.w$Sp.Combo <- paste(df.w$Sp1, df.w$Sp2, sep="_")

#Add in days and initial densities

cage <- read.csv("Data/CageData.csv")
cage[,c(2:3,9,10)] <- list(NULL)

cage$Sp1 <- ifelse(cage$ENEX ==0 | cage$ENVE==0, "ENTR", "ENEX")
cage$Sp2 <- ifelse(cage$ENEX==0, "ENVE", 
                   ifelse(cage$ENTR==0, "ENVE", "ENEX"))

cage$Sp1.Init.CT <- ifelse(cage$Sp1 == "ENEX", cage$ENEX.Abun, cage$ENTR.Abun)
cage$Sp2.Init.CT <- ifelse(cage$Sp2 == "ENEX", cage$ENEX.Abun, cage$ENVE.Abun)

cage[,c(4:6)]<-list(NULL)


df <-plyr::join(df.w,cage, by=c('Lake', "Cage", "Sp1", "Sp2"), type='left')

df$Hi <-ifelse(df$Sp1.Init.CT==15, df$Sp1, df$Sp2)
df$Lo <-ifelse(df$Sp1.Init.CT==5, df$Sp1, df$Sp2)

df$HiLo <-as.factor(paste(df$Hi, "Hi", df$Lo, "Lo", sep="_"))

rm(list = ls()[!(ls() %in% c('df'))])


#Growth: [mean (ln(head width in the second sample) – mean(ln(head width in the first sample)] / (no. days between samples)
df$GrowthSp1 <- df$Sp1.lnHW.dif/df$Days
df$GrowthSp2 <- df$Sp2.lnHW.dif/df$Days

#Mortality  [ln(Final.Ct) - ln(Init.Ct)] / no. days between samples
df$Sp1.lnInit.CT <- log(df$Sp1.Init.CT) 
df$Sp2.lnInit.CT <- log(df$Sp2.Init.CT)
df$Sp1.lnCT <- log(df$Sp1.CT) 
df$Sp2.lnCT <- log(df$Sp2.CT)

df$MortalitySp1.ln <- (df$Sp1.lnCT- df$Sp1.lnInit.CT)/df$Days 
df$MortalitySp2.ln <- (df$Sp2.lnCT- df$Sp2.lnInit.CT)/df$Days 

df[,c(5:8)]<-list(NULL)

df.w<-df

df.w$Growth.Hi <-ifelse(df.w$Hi == df.w$Sp1, df.w$GrowthSp1, df.w$GrowthSp2)
df.w$Growth.Lo <-ifelse(df.w$Lo == df.w$Sp1, df.w$GrowthSp1, df.w$GrowthSp2)

df.w$Mortality.Hi <-ifelse(df.w$Hi == df.w$Sp1, df.w$MortalitySp1.ln, df.w$MortalitySp2.ln)
df.w$Mortality.Lo <-ifelse(df.w$Lo == df.w$Sp1, df.w$MortalitySp1.ln, df.w$MortalitySp2.ln)

df.w$Hi <- as.factor(df.w$Hi)
df.w$Lo <- as.factor(df.w$Lo)


df.w$Rel.Abun.Exp <- ifelse(df.w$Sp1.Init.CT ==15 & df.w$Sp2.Init.CT == 5, "HL", "LH")
df.w$Rel.Abun.Exp <- as.factor(df.w$Rel.Abun.Exp)
rel.abun<-read.csv('Data/RelativeAbundance.csv')
df.w$Rel.Abun.Hi <- ifelse(df.w$Lake == 'Charleston' & df.w$Hi == "ENEX", rel.abun[1,3],
                           ifelse(df.w$Lake == 'Charleston' & df.w$Hi == "ENTR", rel.abun[2,3],
                                  ifelse(df.w$Lake == 'Charleston' & df.w$Hi == "ENVE", rel.abun[3,3],
                                         ifelse(df.w$Lake == 'Engineer' & df.w$Hi == "ENTR", rel.abun[5,3],
                                                ifelse(df.w$Lake == 'Engineer' & df.w$Hi == "ENVE", rel.abun[6,3],
                                                       ifelse(df.w$Lake == 'Engineer' & df.w$Hi == "ENEX", rel.abun[4,3],
                                                              ifelse(df.w$Lake == 'Fayetteville' & df.w$Hi == "ENEX", rel.abun[7,3], 
                                                                     ifelse(df.w$Lake == 'Fayetteville' & df.w$Hi == "ENTR", rel.abun[8,3],
                                                                            ifelse(df.w$Lake == 'Fayetteville' & df.w$Hi == "ENVE", rel.abun[9,3],
                                                                                   ifelse(df.w$Lake == 'Lincoln' & df.w$Hi == "ENTR", rel.abun[11,3],
                                                                                   ifelse(df.w$Lake == 'Lincoln' & df.w$Hi == "ENVE", rel.abun[12,3],
                                                                                   ifelse(df.w$Lake == 'Lincoln' & df.w$Hi == "ENEX", rel.abun[10,3],
                                                                                   ifelse(df.w$Lake == 'Wedington' & df.w$Hi == "ENEX", rel.abun[13,3],
                                                                                   ifelse(df.w$Lake == 'Wedington' & df.w$Hi == "ENTR", rel.abun[14,3], rel.abun[15,3]))))))))))))))

df.w$Rel.Abun.Lo <- ifelse(df.w$Lake == 'Charleston' & df.w$Lo == "ENEX", rel.abun[1,3],
                           ifelse(df.w$Lake == 'Charleston' & df.w$Lo == "ENTR", rel.abun[2,3],
                                  ifelse(df.w$Lake == 'Charleston' & df.w$Lo == "ENVE", rel.abun[3,3],
                                         ifelse(df.w$Lake == 'Engineer' & df.w$Lo == "ENTR", rel.abun[5,3],
                                                ifelse(df.w$Lake == 'Engineer' & df.w$Lo == "ENVE", rel.abun[6,3],
                                                       ifelse(df.w$Lake == 'Engineer' & df.w$Lo == "ENEX", rel.abun[4,3],
                                                              ifelse(df.w$Lake == 'Fayetteville' & df.w$Lo == "ENEX", rel.abun[7,3], 
                                                                     ifelse(df.w$Lake == 'Fayetteville' & df.w$Lo == "ENTR", rel.abun[8,3],
                                                                            ifelse(df.w$Lake == 'Fayetteville' & df.w$Lo == "ENVE", rel.abun[9,3],
                                                                                   ifelse(df.w$Lake == 'Lincoln' & df.w$Lo == "ENTR", rel.abun[11,3],
                                                                                          ifelse(df.w$Lake == 'Lincoln' & df.w$Lo == "ENVE", rel.abun[12,3],
                                                                                                 ifelse(df.w$Lake == 'Lincoln' & df.w$Lo == "ENEX", rel.abun[10,3],
                                                                                                        ifelse(df.w$Lake == 'Wedington' & df.w$Lo == "ENEX", rel.abun[13,3],
                                                                                                               ifelse(df.w$Lake == 'Wedington' & df.w$Lo == "ENTR", rel.abun[14,3], rel.abun[15,3]))))))))))))))


df.w$Rel.Abun.Sp1 <- ifelse(df.w$Sp1 == df.w$Hi, df.w$Rel.Abun.Hi, df.w$Rel.Abun.Lo)
df.w$Rel.Abun.Sp2 <- ifelse(df.w$Sp2 == df.w$Hi, df.w$Rel.Abun.Hi, df.w$Rel.Abun.Lo)

############################
#Long format

start <- read.csv("Data/ExperimentalTreatments.csv")
start$Cage <- as.factor(start$Cage)
colnames(start)[6:8] <- c("ENTR.Abun.Init", "ENVE.Abun.Init", "ENEX.Abun.Init")
start[,c(9)] <- NULL
start[,c(10)] <- NULL


final <- read.csv("Data/FinalSize.csv")
final$Cage <- as.factor(final$Cage)
final$OWPL <- as.numeric(final$OWPL)
colnames(final)[7] <-"I_F"
final[,8]<-NULL

#Aggregate measurement data

#1) Subset out initial / final

init.hw <- subset (final, I_F == "I")
final.hw <-subset(final, I_F == "F")

#2) Mean log(HW) and log(OWPL_ for each Lake, Cage, Species

init.hw$lnHW <- log(init.hw$HW)
init.hw$lnOWPL <- log(init.hw$OWPL)
init.hw.mean <- aggregate(init.hw[c("lnHW", "lnOWPL")], by =init.hw[c('Lake', 'Species')], FUN = 'mean')
init.hw.mean[,4] <- NULL

final.hw$lnHW <- log(final.hw$HW)
final.hw$lnOWPL <- log(final.hw$OWPL)
final.hw.mean <- aggregate(final.hw[c("lnHW", "lnOWPL")], by =final.hw[c('Lake', 'Cage', 'Species')], FUN = 'mean')
final.hw.mean[,5] <- NULL

final.count <- aggregate(final.hw["lnHW"], by =final.hw[c('Lake', 'Cage', 'Species')], FUN = 'length')
colnames(final.count)[4] <-"Count"

final<-plyr::join(final.hw.mean, final.count, by=c('Lake','Cage','Species'), type='left')

df.l <-plyr::join(final, init.hw.mean, by=c('Lake', 'Species'), type='left')
colnames(df.l)[c(4,6)] <- c("Final.lnHw", "Init.lnHW")

cage <- read.csv("Data/CageData.csv")
cage[,c(2:3,9,10)] <- list(NULL)



cage$Sp1 <- ifelse(cage$ENEX ==0 | cage$ENVE==0, "ENTR", "ENEX")
cage$Sp2 <- ifelse(cage$ENEX==0, "ENVE", 
                   ifelse(cage$ENTR==0, "ENVE", "ENEX"))

cage$Sp1.Init.CT <- ifelse(cage$Sp1 == "ENEX", cage$ENEX.Abun, cage$ENTR.Abun)
cage$Sp2.Init.CT <- ifelse(cage$Sp2 == "ENEX", cage$ENEX.Abun, cage$ENVE.Abun)

cage[,c(4:6)]<- list(NULL)


df.l <-plyr::join(df.l,cage, by=c('Lake', "Cage"), type='left')
df.l$Init.CT <- ifelse(df.l$Species == df.l$Sp1, df.l$Sp1.Init.CT, df.l$Sp2.Init.CT)

df.l$Growth <- (df.l$Final.lnHw - df.l$Init.lnHW)/df.l$Days
df.l$Mortality <- (log(df.l$Init.CT) - log(df.l$Count))/df.l$Days
df.l$OtherSp <- ifelse(df.l$Species == df.l$Sp1, df.l$Sp2, df.l$Sp1)
df.l[,c(4,6:11)]<- list(NULL)

colnames(df.l)[5] <- "Init.Density"
colnames(df.l)[4] <- "Final.Density"

rel.abun<-read.csv('Data/RelativeAbundance.csv')

df.l <- plyr::join(df.l, rel.abun, by = c('Lake','Species'), type ='left')

rm(list = ls()[!(ls() %in% c('df.w',"df.l"))])

