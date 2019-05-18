######################
# Stabilizing Effects
# Experiment Analysis
# January 10 2018
# BHO
######################

library(plyr)
library(lmerTest)
library(car)
library(ggplot2)
library(reshape2)
library(ggpubr)

#setwd('/Users/adamsiepielski/Dropbox/stabilizingeffects')
source('DataSteps.R')

#df.l$Init.Density <- as.factor(df.l$Init.Density)
df.l$OtherSp <- as.factor(df.l$OtherSp)

########################################################################
# What if separate species combo's out into different MANOVA analyses??
########################################################################

#Lake
df.w$Sp.Combo <- as.factor(df.w$Sp.Combo)
tv <- subset(df.w, Sp.Combo == "ENTR_ENVE")

tv.manova <- manova(cbind(GrowthSp1, GrowthSp2) ~ Lake * Rel.Abun.Exp, data=tv)
summary(tv.manova, type = III, test = "Wilks")
summary.aov(tv.manova)


ev <- subset(df.w, Sp.Combo == "ENEX_ENVE")
ev.manova <- manova(cbind(GrowthSp1, GrowthSp2) ~ Lake * Rel.Abun.Exp, data=ev)
summary(ev.manova, type = III, test = "Wilks")
summary.aov(ev.manova)

te <- subset(df.w, Sp.Combo == "ENTR_ENEX")
te.manova <- manova(cbind(GrowthSp1, GrowthSp2) ~ Lake * Rel.Abun.Exp, data=te)
summary(te.manova, type = III, test = "Wilks")
summary.aov(te.manova)

################################################
#Does species gorwth rates differ across lakes?
################################################

sp.comp <- (aov(df.l$Growth ~ df.l$Species))
TukeyHSD(sp.comp)

species_comparisons <- list( c("ENEX", "ENTR"),
                         c("ENEX", "ENVE"),
                         c("ENVE", "ENTR"))

growth.species.comparison<- 
  ggerrorplot(data = df.l, x = "Species", y = "Growth",
              desc_stat = "mean_sd", color = "black",
              add = "jitter", add.params = list(color = "grey70")) + 
  stat_compare_means(comparisons = species_comparisons) +
  #scale_x_discrete(name = "Wetland age (years)", 
  #                 labels = c("2-3", "4-5", "6-7", "Reference"))+
  theme_classic()
  
  ##################################################
  # Comparison of individual sloper of each species 
  ##################################################
  
  ##############################################
  # Single sp regression (Levine Nature paper)
  ##############################################
  
  species <- c("ENTR", 'ENVE', 'ENEX')
  lakes <- c("Charleston", "Engineer", "Fayetteville", "Lincoln", "Wedington")
  
  sp.df <- data.frame(Species = as.character(),
                      OtherSp = as.character(),
                      Lake = as.character(),
                      Slope = as.numeric(),
                      p = as.numeric(),
                      r2 = as.numeric(),
                      Lower.CI = as.numeric(),
                      Upper.CI = as.numeric(),
                      stringsAsFactors = FALSE)
  
  for(i in 1:length(species)) {
    sub <- subset(df.l, Species == species[i])
    other.sp.list <- as.list(plyr::count(sub$OtherSp)[1])
    
    for(j in 1:length(other.sp.list[[1]])) {
      for(l in 1:length(lakes)){
        row <- nrow(sp.df) + 1
        df.temp <- subset(sub, OtherSp == other.sp.list[[1]][j] & Lake == lakes[l])
        model <- lm(Growth ~ Init.Density, data=df.temp)
        ci <- confint(model)
        sp.df[row, "Species"] <- as.character(species[i])
        sp.df[row, "OtherSp"] <- as.character(other.sp.list[[1]][j])
        sp.df[row, "Lake"] <- as.character(lakes[l])
        sp.df[row, "Slope"] <- summary(model)$coefficient[2,1]
        sp.df[row, "p"] <- summary(model)$coefficient[2,4]
        sp.df[row, 'r2'] <- summary(model)$adj.r.squared
        sp.df[row, 'Lower.CI'] <- ci[2,1]
        sp.df[row, 'Upper.CI'] <- ci[2,2]
      }
    }
  }

  sp.df.g <-sp.df
  ##################
  # Env covariates
  ##################
  #df.l$Init.Density <- as.factor(df.l$Init.Density)
  
  a<-ddply(df.l, c("Lake", "Species", "Init.Density","OtherSp", "RelativeAbun"), summarise,
           N      = length(Growth),
           Growth = mean(Growth),
           sd     = sd(Growth),
           se     = sd / sqrt(N))
  
  a.wide <- dcast(a, Lake + Species + OtherSp + N + RelativeAbun~ Init.Density, value.var="Growth")
  
  colnames(a.wide)[6:7] <- c("Lo", "Hi")
  a.wide$Dif <- log2(a.wide$Lo/a.wide$Hi) ###log ratio of differences migh make better sense, Since the absolute value per se is not key, Ratehr its how much better
  a.wide$X <- paste(a.wide$Species, a.wide$OtherSp, sep=" ")
  
  growth.dif<-a.wide
  
  env<-read.csv('Data/Env_covariates.csv')
  colnames(env)[1] <- "Lake"
  growth.dif <- join(growth.dif, env, by='Lake')

  #Abiotic -- 3 way interaction for salinity and conductivity; chl-a species x chl.a
  salinity <- lmer(Dif ~ Species * OtherSp * scale(salinity) + (1|Lake),
                   data = growth.dif)
  Anova(salinity)
  summary(salinity)
  cond <- lmer(Dif ~ Species * OtherSp * scale(cond) + (1|Lake),
               data = growth.dif)
  Anova(cond)
  summary(cond)
  
  pH <- lmer(Dif ~ Species * OtherSp * scale(pH) + (1|Lake),
             data = growth.dif)
  Anova(pH)
  summary(pH)
  
  chl <- lmer(Dif ~ Species * OtherSp * scale(chl.a) + (1|Lake),
              data = growth.dif)
  Anova(chl)
  summary(chl)
  
  #Biotic -- trend for species x prey and competitor density
  fish <- lmer(Dif ~ Species * OtherSp * scale(Fish.Densitym2) + (1|Lake),
               data = growth.dif)
  Anova(fish)
  summary(fish)
  
  veg <- lmer(Dif ~ Species * OtherSp * scale(Shoot.Countm2) + (1|Lake),
              data = growth.dif)
  Anova(veg)
  summary(veg)
  
  prey <- lmer(Dif ~ Species * OtherSp * scale(Prey.CPU) + (1|Lake),
               data = growth.dif)
  Anova(prey)
  summary(prey)
  
  competitor <- lmer(Dif ~ Species * OtherSp * scale(Competitorm2) + (1|Lake),
                     data = growth.dif)
  summary(competitor)
  Anova(competitor)  
 