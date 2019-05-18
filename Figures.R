library(ggplot2)
library(plyr)
library(reshape2)
library(cowplot)
library(grid)

source('DataSteps.R')
source('Analysis_Growth.R')
source('Analysis_Mortality.R')

 df.l$OtherSp <- as.factor(df.l$OtherSp)

#df.l <- df.l%>% arrange(Lake, Species, OtherSp, Init.Density)

#############################
#Species pair graphs by lake
#############################

#Growth

all.sum<-ddply(df.l, c("Lake", "Species", "OtherSp", "Init.Density"), summarise,
               N      = length(Growth),
               mean   = mean(Growth),
               sd     = sd(Growth),
               se     = sd / sqrt(N))

all.sum$Color <- ifelse(all.sum$Species == "ENEX", '#2b83ba',
                        ifelse(all.sum$Species == "ENTR", '#fdae61', '#d7191c'))

all.sum$Line <- ifelse(all.sum$OtherSp == "ENTR", 'solid',
                       ifelse(all.sum$OtherSp == "ENEX", 'dotted', 'longdash'))

all.sum$Symbol <- ifelse(all.sum$OtherSp == "ENTR", 15,
                         ifelse(all.sum$OtherSp == "ENEX", 17, 19))

ex.tr.g <- subset(all.sum, Species == "ENTR" & OtherSp == "ENEX" | Species == "ENEX" & OtherSp == "ENTR" )
ex.ve.g <- subset(all.sum, Species == "ENVE" & OtherSp == "ENEX" | Species == "ENEX" & OtherSp == "ENVE" )
tr.ve.g <- subset(all.sum, Species == "ENVE" & OtherSp == "ENTR" | Species == "ENTR" & OtherSp == "ENVE" )

extr.graph <-
  ggplot(data = ex.tr.g)+
  geom_line(aes(x = as.numeric(Init.Density), y = mean, 
                linetype = as.factor(Line), color = as.factor(Color))) +
  geom_point(aes(x = as.numeric(Init.Density), y = mean, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = as.numeric(Init.Density), ymin = mean - se, ymax = mean + se), width = 0) +
  scale_colour_manual(values=c('#2b83ba','#fdae61'))+
  scale_linetype_manual(values=c('dotted','solid'))+
  scale_shape_manual(values = c(15,17))+
  scale_x_continuous(breaks = c(5,15), limits = c(2,18), labels = c(0.25, 0.75)) +
  scale_y_continuous(breaks = c(0.003, 0.006, 0.009)) +
  xlab('Relative abundance')+
  ylab("Growth")+
  theme(legend.position="none")

extr.graph.g <-
  extr.graph + facet_grid(. ~ Lake)

exve.graph <-
  ggplot(data = ex.ve.g)+
  geom_line(aes(x = as.numeric(Init.Density), y = mean, linetype = as.factor(Line), color = as.factor(Color))) +
  geom_point(aes(x = as.numeric(Init.Density), y = mean, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = as.numeric(Init.Density), ymin = mean - se, ymax = mean + se), width = 0) +
  scale_colour_manual(values=c('#2b83ba','#d7191c'))+
  scale_linetype_manual(values=c('dotted','longdash'))+
  scale_shape_manual(values = c(17,19))+
  scale_x_continuous(breaks = c(5,15), limits = c(2,18), labels = c(0.25, 0.75)) +
  scale_y_continuous(breaks = c(0.003, 0.006, 0.009)) +
  xlab('Relative abundance')+
  ylab("Growth")+
  theme(legend.position="none")

exve.graph.g <-
  exve.graph + facet_grid(. ~ Lake)
# tr.sum$se <- ifelse(tr.sum$se > 1, 0 , tr.sum$se)

trve.graph <-
  ggplot(data = tr.ve.g)+
  geom_line(aes(x = as.numeric(Init.Density), y = mean, linetype = as.factor(Line), color = as.factor(Color))) +
  geom_point(aes(x = as.numeric(Init.Density), y = mean, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = as.numeric(Init.Density), ymin = mean - se, ymax = mean + se), width = 0) +
  scale_colour_manual(values=c('#d7191c','#fdae61'))+
  scale_linetype_manual(values=c('longdash','solid'))+
  scale_shape_manual(values = c(15,19))+
  scale_x_continuous(breaks = c(5,15), limits = c(2,18), labels = c(0.25, 0.75)) +
  scale_y_continuous(breaks = c(0.003, 0.006, 0.009)) +
  xlab('Relative abundance')+
  #ylab(expression(paste(italic("E. exsulans"), "growth")))+
  theme(legend.position="none")

trve.graph.g <-
  trve.graph + facet_grid(. ~ Lake)

#Mortality
all.sum<-ddply(df.l, c("Lake", "Species", "OtherSp", "Init.Density"), summarise,
               N      = length(Mortality),
               mean   = mean(Mortality),
               sd     = sd(Mortality),
               se     = sd / sqrt(N))

all.sum$Color <- ifelse(all.sum$Species == "ENEX", '#2b83ba',
                        ifelse(all.sum$Species == "ENTR", '#fdae61', '#d7191c'))

all.sum$Line <- ifelse(all.sum$OtherSp == "ENTR", 'solid',
                       ifelse(all.sum$OtherSp == "ENEX", 'dotted', 'longdash'))

all.sum$Symbol <- ifelse(all.sum$OtherSp == "ENTR", 15,
                         ifelse(all.sum$OtherSp == "ENEX", 17, 19))

ex.tr.m <- subset(all.sum, Species == "ENTR" & OtherSp == "ENEX" | Species == "ENEX" & OtherSp == "ENTR" )
ex.ve.m <- subset(all.sum, Species == "ENVE" & OtherSp == "ENEX" | Species == "ENEX" & OtherSp == "ENVE" )
tr.ve.m <- subset(all.sum, Species == "ENVE" & OtherSp == "ENTR" | Species == "ENTR" & OtherSp == "ENVE" )

extr.graph <-
  ggplot(data = ex.tr.m)+
  geom_line(aes(x = as.numeric(Init.Density), y = mean, 
            linetype = as.factor(Line), color = as.factor(Color))) +
  geom_point(aes(x = as.numeric(Init.Density), y = mean, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = as.numeric(Init.Density), ymin = mean - se, ymax = mean + se), width = 0) +
  scale_colour_manual(values=c('#2b83ba','#fdae61'))+
  scale_linetype_manual(values=c('dotted','solid'))+
  scale_shape_manual(values = c(15,17))+
  scale_x_continuous(breaks = c(5,15), limits = c(2,18), labels = c(0.25, 0.75)) +
  #scale_y_continuous(breaks = c(0.00, 0.015, 0.03)) +
  xlab('Relative abundance')+
  ylab("Mortality")+
  theme(legend.position="none")

extr.graph.m <-
extr.graph + facet_grid(. ~ Lake)

exve.graph <-
  ggplot(data = ex.ve.m)+
  geom_line(aes(x = as.numeric(Init.Density), y = mean, linetype = as.factor(Line), color = as.factor(Color))) +
  geom_point(aes(x = as.numeric(Init.Density), y = mean, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = as.numeric(Init.Density), ymin = mean - se, ymax = mean + se), width = 0) +
  scale_colour_manual(values=c('#2b83ba','#d7191c'))+
  scale_linetype_manual(values=c('dotted','longdash'))+
  scale_shape_manual(values = c(17,19))+
  scale_x_continuous(breaks = c(5,15), limits = c(2,18), labels = c(0.25, 0.75)) +
 # scale_y_continuous(breaks = c(0.003, 0.006, 0.009)) +
  xlab('Relative abundance')+
  ylab("Mortality")+
  theme(legend.position="none")

exve.graph.m <-
  exve.graph + facet_grid(. ~ Lake)
# tr.sum$se <- ifelse(tr.sum$se > 1, 0 , tr.sum$se)

trve.graph <-
  ggplot(data = tr.ve.m)+
  geom_line(aes(x = as.numeric(Init.Density), y = mean, linetype = as.factor(Line), color = as.factor(Color))) +
  geom_point(aes(x = as.numeric(Init.Density), y = mean, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = as.numeric(Init.Density), ymin = mean - se, ymax = mean + se), width = 0) +
  scale_colour_manual(values=c('#d7191c','#fdae61'))+
  scale_linetype_manual(values=c('longdash','solid'))+
  scale_shape_manual(values = c(15,19))+
  scale_x_continuous(breaks = c(5,15), limits = c(2,18), labels = c(0.25, 0.75)) +
 # scale_y_continuous(breaks = c(0.003, 0.006, 0.009)) +
  xlab('Relative abundance')+
  ylab("Mortality")+
  theme(legend.position="none")

trve.graph.m <-
  trve.graph + facet_grid(. ~ Lake)


pdf("Figures/Intra_vs_Inter_FacetLake_All.pdf", width=7.48, height=12)
plot_grid(extr.graph.g, extr.graph.m,
          exve.graph.g, exve.graph.m,
          trve.graph.g, trve.graph.m,
          ncol = 1, labels = c('A)', '', 'B)', '', 'C)',''))
dev.off()

##############################
# Slope + CI's
##############################

ex.tr <- subset(sp.df.g, Species == "ENTR" & OtherSp == "ENEX" | Species == "ENEX" & OtherSp == "ENTR" )
ex.ve <- subset(sp.df.g, Species == "ENVE" & OtherSp == "ENEX" | Species == "ENEX" & OtherSp == "ENVE" )
tr.ve <- subset(sp.df.g, Species == "ENVE" & OtherSp == "ENTR" | Species == "ENTR" & OtherSp == "ENVE" )

extr.graph.g <-
  ggplot(data = ex.tr)+
  geom_point(aes(x = Lake, y = Slope)) + #, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = Lake, ymin = Lower.CI, ymax = Upper.CI), width = 0) +
  geom_abline(slope = 0, intercept = 0, lty = "dashed")+
  coord_cartesian(ylim = c(-0.0006, 0.0008))+
  scale_y_continuous(breaks = c(-0.0006, 0, 0.0006), labels = c(-6,0,6)) +
  xlab('Lake')+
  ylab(expression('Growth\nslope (10'^-4*')'))+
  facet_grid(Species ~ .) + 
  theme(legend.position="none",
        axis.text.x=element_text(angle=30,hjust=1))

exve.graph.g <-
  ggplot(data = ex.ve)+
  geom_point(aes(x = Lake, y = Slope)) + #, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = Lake, ymin = Lower.CI, ymax = Upper.CI), width = 0) +
  geom_abline(slope = 0, intercept = 0, lty = "dashed")+
  coord_cartesian(ylim = c(-0.0006, 0.0008))+
  scale_y_continuous(breaks = c(-0.0006, 0, 0.0006), labels = c(-6,0,6)) +
  xlab('Lake')+
  ylab(expression('Growth\nslope (10'^-4*')'))+
  facet_grid(Species ~ .) + 
  theme(legend.position="none",
        axis.text.x=element_text(angle=30,hjust=1))

trve.graph.g <-
  ggplot(data = tr.ve)+
  geom_point(aes(x = Lake, y = Slope)) + #, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = Lake, ymin = Lower.CI, ymax = Upper.CI), width = 0) +
  geom_abline(slope = 0, intercept = 0, lty = "dashed")+
  coord_cartesian(ylim = c(-0.0006, 0.0008))+
  scale_y_continuous(breaks = c(-0.0006, 0, 0.0006), labels = c(-6,0,6)) +
  xlab('Lake')+
  ylab(expression('Growth\nslope (10'^-4*')'))+
  facet_grid(Species ~ .) + 
  theme(legend.position="none",
        axis.text.x=element_text(angle=30,hjust=1))


ex.tr <- subset(sp.df.m, Species == "ENTR" & OtherSp == "ENEX" | Species == "ENEX" & OtherSp == "ENTR" )
ex.ve <- subset(sp.df.m, Species == "ENVE" & OtherSp == "ENEX" | Species == "ENEX" & OtherSp == "ENVE" )
tr.ve <- subset(sp.df.m, Species == "ENVE" & OtherSp == "ENTR" | Species == "ENTR" & OtherSp == "ENVE" )

extr.graph.m <-
  ggplot(data = ex.tr)+
  geom_point(aes(x = Lake, y = Slope)) + #, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = Lake, ymin = Lower.CI, ymax = Upper.CI), width = 0) +
  geom_abline(slope = 0, intercept = 0, lty = "dashed")+
  coord_cartesian(ylim = c(-0.004, 0.004))+
  scale_y_continuous(breaks = c(-0.004, 0, 0.004), labels = c(-4,0,4)) +
  xlab('Lake')+
  ylab(expression('Mortality\nslope (10'^-3*')'))+
  facet_grid(Species ~ .) + 
  theme(legend.position="none",
        axis.text.x=element_text(angle=30,hjust=1))

exve.graph.m <-
  ggplot(data = ex.ve)+
  geom_point(aes(x = Lake, y = Slope)) + #, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = Lake, ymin = Lower.CI, ymax = Upper.CI), width = 0) +
  geom_abline(slope = 0, intercept = 0, lty = "dashed")+
  coord_cartesian(ylim = c(-0.004, 0.004))+
  scale_y_continuous(breaks = c(-0.004, 0, 0.004), labels = c(-4,0,4)) +
  xlab('Lake')+
  ylab(expression('Mortality\nslope (10'^-3*')'))+  
  facet_grid(Species ~ .) + 
  theme(legend.position="none",
        axis.text.x=element_text(angle=30,hjust=1))

trve.graph.m <-
  ggplot(data = tr.ve)+
  geom_point(aes(x = Lake, y = Slope)) + #, shape = as.factor(Symbol))) +
  geom_errorbar(aes(x = Lake, ymin = Lower.CI, ymax = Upper.CI), width = 0) +
  geom_abline(slope = 0, intercept = 0, lty = "dashed")+
  coord_cartesian(ylim = c(-0.004, 0.004))+
  scale_y_continuous(breaks = c(-0.004, 0, 0.004), labels = c(-4,0,4)) +
  xlab('Lake')+
  ylab(expression('Mortality\nslope (10'^-3*')'))+  
  facet_grid(Species ~ .) + 
  theme(legend.position="none",
        axis.text.x=element_text(angle=30,hjust=1))

pdf("Figures/Intra_vs_Inter_SlopeCI.pdf", width=7.48, height=18)
plot_grid(extr.graph.g, extr.graph.m,
          exve.graph.g, exve.graph.m,
          trve.graph.g, trve.graph.m,
          ncol = 1, labels = c('A)', '', 'B)', '', 'C)',''))
dev.off()

############################
# Environmental Covariation
############################

#Growth

a<-ddply(df.l, c("Lake", "Species", "Init.Density","OtherSp"), summarise,
         N      = length(Growth),
         mean = mean(Growth),
         sd     = sd(Growth),
         se     = sd / sqrt(N),
         var    = sd*sd)

a.wide.mean <- dcast(a, Lake + Species + OtherSp + N ~ Init.Density, value.var="mean")
a.wide.se <- dcast(a, Lake + Species + OtherSp + N ~ Init.Density, value.var="se")
a.wide.var <- dcast(a, Lake + Species + OtherSp + N ~ Init.Density, value.var="var")
colnames(a.wide.se)[5:6] <- c("Lo.SE", "Hi.SE")
colnames(a.wide.mean)[5:6] <- c("Lo.Mean", "Hi.Mean")
colnames(a.wide.var)[5:6] <- c("Lo.Var", "Hi.Var")

a.wide <- join(a.wide.mean, a.wide.var)
a.wide <- join(a.wide, a.wide.se)

a.wide$SED <- sqrt((a.wide$Lo.Var/a.wide$N) + (a.wide$Hi.Var/a.wide$N))
a.wide$X <- paste(a.wide$Species, a.wide$OtherSp, sep=" ")
a.wide$Dif <- (a.wide$Hi.Mean - a.wide$Lo.Mean)/10 ##divide by 10 to make it the slope
a.wide$SED.min <- a.wide$Dif - a.wide$SED
a.wide$SED.max <- a.wide$Dif + a.wide$SED

env<-read.csv('Data/Env_covariates.csv')
colnames(env)[1] <- "Lake"
growth.dif <- join(a.wide, env, by='Lake')

fish.plot <- ggplot(data=growth.dif, aes(x = Fish.Densitym2, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  # geom_segment(aes(x = 0, xend = 10, y=0, yend = 0), color = "black", linetype = 3) +
  ylab("Growth slope\nrare vs common")+
  xlab("Fish density")+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  theme(legend.position="none")

salinity.plot <- ggplot(data=growth.dif, aes(x = salinity, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  ylab("Growth slope\nrare vs common")+
  xlab("Salinity")+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  theme(legend.position="none")

Veg.plot <- ggplot(data=growth.dif, aes(x = Shoot.Countm2, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  ylab("Growth slope\nrare vs common")+
  xlab("Macrophyte density")+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  theme(legend.position="none")

prey.plot <- ggplot(data=growth.dif, aes(x = Prey.CPU, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  ylab("Growth slope\nrare vs common")+
  xlab("Prey density")+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  theme(legend.position="none")

comp.plot <- ggplot(data=growth.dif, aes(x = Competitorm2, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  # geom_segment(aes(x = 0, xend = 210, y=0, yend = 0), color = "black", linetype = 3) +
  ylab("Growth slope\nrare vs common")+
  xlab("Competitor density")+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  theme(legend.position="none")

cond.plot <- 
  ggplot(data=growth.dif, aes(x = cond, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F)+  
  scale_x_continuous(breaks = seq(70, 170, 30))+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  ylab("Growth slope\nrare vs common")+
  xlab("Conductivity")+
  theme(legend.position="none")

pH.plot <- 
  ggplot(data=growth.dif, aes(x = pH, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F)+
  scale_x_continuous(breaks = seq(7.7,8.3,0.2))+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  ylab("Growth slope\nrare vs common")+
  xlab("pH")+
  theme(legend.position="none")

chl.plot <- 
  ggplot(data=growth.dif, aes(x = chl.a, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F)+
  # scale_x_continuous(breaks = seq(7.7,8.3,0.2))+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  ylab("Growth slope\nrare vs common")+
  xlab("chl-a")+
  theme(legend.position="none")

g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(pH.plot) 
grid.draw(legend) 

pdf("Figures/StabilizingEffects_lnGrowth_EnvCovar6.pdf", width=7.48, height=10)
plot_grid(fish.plot, Veg.plot, prey.plot, comp.plot, cond.plot,
          salinity.plot, pH.plot, chl.plot,ncol = 2)
dev.off()

#Mortality

a<-ddply(df.l, c("Lake", "Species", "Init.Density","OtherSp"), summarise,
         N      = length(Mortality),
         mean = mean(Mortality),
         sd     = sd(Mortality),
         se     = sd / sqrt(N),
         var    = sd*sd)

a.wide.mean <- dcast(a, Lake + Species + OtherSp + N ~ Init.Density, value.var="mean")
a.wide.se <- dcast(a, Lake + Species + OtherSp + N ~ Init.Density, value.var="se")
a.wide.var <- dcast(a, Lake + Species + OtherSp + N ~ Init.Density, value.var="var")
colnames(a.wide.se)[5:6] <- c("Lo.SE", "Hi.SE")
colnames(a.wide.mean)[5:6] <- c("Lo.Mean", "Hi.Mean")
colnames(a.wide.var)[5:6] <- c("Lo.Var", "Hi.Var")

a.wide <- join(a.wide.mean, a.wide.var)
a.wide <- join(a.wide, a.wide.se)

a.wide$SED <- sqrt((a.wide$Lo.Var/a.wide$N) + (a.wide$Hi.Var/a.wide$N))
a.wide$X <- paste(a.wide$Species, a.wide$OtherSp, sep=" ")
a.wide$Dif <- (a.wide$Hi.Mean - a.wide$Lo.Mean)/10 ##divide by 10 to make it the slope
a.wide$SED.min <- a.wide$Dif - a.wide$SED
a.wide$SED.max <- a.wide$Dif + a.wide$SED

env<-read.csv('Data/Env_covariates.csv')
colnames(env)[1] <- "Lake"
Mortality.dif <- join(a.wide, env, by='Lake')

fish.plot <- ggplot(data=Mortality.dif, aes(x = Fish.Densitym2, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  # geom_segment(aes(x = 0, xend = 10, y=0, yend = 0), color = "black", linetype = 3) +
  ylab("Mortality slope\nrare vs common")+
  xlab("Fish density")+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  theme(legend.position="none")

salinity.plot <- ggplot(data=Mortality.dif, aes(x = salinity, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  ylab("Mortality slope\nrare vs common")+
  xlab("Salinity")+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  theme(legend.position="none")

Veg.plot <- ggplot(data=Mortality.dif, aes(x = Shoot.Countm2, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  ylab("Mortality slope\nrare vs common")+
  xlab("Macrophyte density")+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  theme(legend.position="none")

prey.plot <- ggplot(data=Mortality.dif, aes(x = Prey.CPU, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  ylab("Mortality slope\nrare vs common")+
  xlab("Prey density")+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  theme(legend.position="none")

comp.plot <- ggplot(data=Mortality.dif, aes(x = Competitorm2, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F) +
  # geom_segment(aes(x = 0, xend = 210, y=0, yend = 0), color = "black", linetype = 3) +
  ylab("Mortality slope\nrare vs common")+
  xlab("Competitor density")+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  theme(legend.position="none")

cond.plot <- 
  ggplot(data=Mortality.dif, aes(x = cond, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F)+  
  scale_x_continuous(breaks = seq(70, 170, 30))+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  ylab("Mortality slope\nrare vs common")+
  xlab("Conductivity")+
  theme(legend.position="none")

pH.plot <- 
  ggplot(data=Mortality.dif, aes(x = pH, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F)+
  scale_x_continuous(breaks = seq(7.7,8.3,0.2))+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  ylab("Mortality slope\nrare vs common")+
  xlab("pH")+
  theme(legend.position="none")

chl.plot <- 
  ggplot(data=Mortality.dif, aes(x = chl.a, y = Dif, color=X, linetype=X)) +
  geom_point() + 
  geom_smooth(method='lm', se=F)+
  # scale_x_continuous(breaks = seq(7.7,8.3,0.2))+
  scale_colour_manual(values=c('#2b83ba', '#2b83ba', '#fdae61', '#fdae61', '#d7191c', '#d7191c'))+
  scale_linetype_manual(values=c('solid','longdash','dotted','longdash','dotted','solid'))+
  ylab("Mortality slope\nrare vs common")+
  xlab("chl-a")+
  theme(legend.position="none")

g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

legend <- g_legend(pH.plot) 
grid.draw(legend) 

pdf("Figures/StabilizingEffects_lnMortality_EnvCovar6.pdf", width=7.48, height=10)
plot_grid(fish.plot, Veg.plot, prey.plot, comp.plot, cond.plot,
          salinity.plot, pH.plot, chl.plot,ncol = 2)
dev.off()

##################
# Spider plots
##################

library(fmsb)

###############
# Create DF Growth
###############

env<-read.csv('Data/Env_covariates.csv')
colnames(env)[1] <- "Lake"
env[,c(5,6,12)] <-NULL

sp.df.g <- join(sp.df.g, env, by = 'Lake')

sp.df.enex.entr <- subset(sp.df.g, Species == "ENEX" & OtherSp == "ENTR")
sp.df.enex.enve <- subset(sp.df.g, Species == "ENEX" & OtherSp == "ENVE")
sp.df.entr.enex <- subset(sp.df.g, Species == "ENTR" & OtherSp == "ENEX")
sp.df.entr.enve <- subset(sp.df.g, Species == "ENTR" & OtherSp == "ENVE")
sp.df.enve.enex <- subset(sp.df.g, Species == "ENVE" & OtherSp == "ENEX")
sp.df.enve.entr <- subset(sp.df.g, Species == "ENVE" & OtherSp == "ENTR")

spider.df <- data.frame(Fish = as.numeric(),
                        Prey = as.numeric(),
                        Macrophyte = as.numeric(),
                        Competitor = as.numeric(),
                        Conductivity = as.numeric(),
                        Salinity = as.numeric(),
                        pH = as.numeric(),
                        chl.a = as.numeric())

spider.df[3,1]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$Fish.Densitym2)
spider.df[3,2]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$Prey.CPU)
spider.df[3,3]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$Shoot.Countm2)
spider.df[3,4]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$Competitorm2)
spider.df[3,5]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$cond)
spider.df[3,6]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$salinity)
spider.df[3,7]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$pH)
spider.df[3,8]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$chl.a)

spider.df[4,1]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$Fish.Densitym2)
spider.df[4,2]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$Prey.CPU)
spider.df[4,3]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$Shoot.Countm2)
spider.df[4,4]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$Competitorm2)
spider.df[4,5]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$cond)
spider.df[4,6]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$salinity)
spider.df[4,7]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$pH)
spider.df[4,8]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$chl.a)

spider.df[5,1]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$Fish.Densitym2)
spider.df[5,2]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$Prey.CPU)
spider.df[5,3]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$Shoot.Countm2)
spider.df[5,4]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$Competitorm2)
spider.df[5,5]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$cond)
spider.df[5,6]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$salinity)
spider.df[5,7]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$pH)
spider.df[5,8]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$chl.a)

spider.df[6,1]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$Fish.Densitym2)
spider.df[6,2]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$Prey.CPU)
spider.df[6,3]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$Shoot.Countm2)
spider.df[6,4]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$Competitorm2)
spider.df[6,5]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$cond)
spider.df[6,6]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$salinity)
spider.df[6,7]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$pH)
spider.df[6,8]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$chl.a)

spider.df[7,1]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$Fish.Densitym2)
spider.df[7,2]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$Prey.CPU)
spider.df[7,3]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$Shoot.Countm2)
spider.df[7,4]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$Competitorm2)
spider.df[7,5]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$cond)
spider.df[7,6]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$salinity)
spider.df[7,7]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$pH)
spider.df[7,8]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$chl.a)

spider.df[8,1]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$Fish.Densitym2)
spider.df[8,2]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$Prey.CPU)
spider.df[8,3]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$Shoot.Countm2)
spider.df[8,4]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$Competitorm2)
spider.df[8,5]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$cond)
spider.df[8,6]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$salinity)
spider.df[8,7]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$pH)
spider.df[8,8]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$chl.a)

rownames(spider.df) <- c('Max','Min','EX_TR', 'EX_VE', 'TR_EX', 'TR_VE',
                         'VE_EX', 'VE_TR')

colnames(spider.df)[8] <- "chl-a"

spider.df[1:2,1:8] <- c(1 , -1)
spider.df.g <- spider.df

#Radar chart
radarchart(spider.df.g[1:4,])

# Plot 2: Same plot with custom features
colors_border=c('#2b83ba', '#2b83ba')#, '#de2d26')

# colors_in=c(rgb(0,0,1,0.4), rgb(1,0,0,0.4)) #, rgb(0.7,0.5,0.1,0.4)

# pdf("Figures/StabilizingEffects_SlopeEnvCor_SpiderPlot.pdf", width=3.74, height=9)
# par(mfrow=c(3,1))

#ENEX
#Open and save EPS first in AI
#Seem to lose some resolution if go straight to Photoshop

setEPS()
postscript("Figures/StabilizingEffects_SpiderPlot_ENEX_growth.eps", 
           onefile = F)
radarchart( spider.df.g[c(1:4),]  , axistype=0 , seg = 8,
            #custom polygon
            pcol=colors_border , plwd=3 , plty=c("solid","longdash"),
            #custom the grid
            cglcol="grey50", cglty=1,
            axislabcol="black",
            caxislabels=seq(-1, 1,0.25), cglwd=1,
            #custom labels
            vlcex=0.8
)
legend(x=1, y=1, legend = c(expression(italic('E. traviatum')),  
                            expression(italic('E. vesperum'))), 
       bty = "n", lwd=3, lty = c('solid','longdash'),
       col=colors_border ,
       text.col = "black", cex=1.2, pt.cex=3)
dev.off()

#ENTR
postscript("Figures/StabilizingEffects_SpiderPlot_ENTR_growth.eps", 
           onefile = F)
radarchart( spider.df.g[c(1,2,5,6),]  , axistype=0 , seg = 8,
            #custom polygon
            pcol=c("#ff850f",'#ff850f') , plwd=3 , plty=c("dotted","longdash"),
            #custom the grid
            cglcol="grey50", cglty=1,
            axislabcol="black",
            caxislabels=seq(-1, 1,0.25), cglwd=1,
            #custom labels
            vlcex=0.8
)
legend(x=1, y=1, legend = c(expression(italic('E. exsulans')),  
                            expression(italic('E. vesperum'))), 
       bty = "n", lwd=3, lty = c('dotted','longdash'),
       col=c("#fdae61",'#fdae61') ,
       text.col = "black", cex=1.2, pt.cex=3)
dev.off()

#ENVE
postscript("Figures/StabilizingEffects_SpiderPlot_ENVE_growth.eps", 
           onefile = F)
radarchart( spider.df.g[c(1,2,7,8),]  , axistype=0 , seg = 8,
            #custom polygon
            pcol=c('#d7191c','#d7191c') , plwd=3 , plty=c("dotted","solid"),
            #custom the grid
            cglcol="grey50", cglty=1,
            axislabcol="black",
            caxislabels=seq(-1, 1,0.25), cglwd=1,
            #custom labels
            vlcex=0.8,
            bg= NA
)
legend(x=1, y=1, legend = c(expression(italic('E. exsulans')),  
                            expression(italic('E. traviatum'))), 
       bty = "n", lwd=3, lty = c('dotted','solid'),
       col=c('#d7191c','#d7191c'),
       text.col = "black", cex=1.2, pt.cex=3)
dev.off()

#Mortality

sp.df.m <- join(sp.df.m, env, by = 'Lake')

sp.df.enex.entr <- subset(sp.df.m, Species == "ENEX" & OtherSp == "ENTR")
sp.df.enex.enve <- subset(sp.df.m, Species == "ENEX" & OtherSp == "ENVE")
sp.df.entr.enex <- subset(sp.df.m, Species == "ENTR" & OtherSp == "ENEX")
sp.df.entr.enve <- subset(sp.df.m, Species == "ENTR" & OtherSp == "ENVE")
sp.df.enve.enex <- subset(sp.df.m, Species == "ENVE" & OtherSp == "ENEX")
sp.df.enve.entr <- subset(sp.df.m, Species == "ENVE" & OtherSp == "ENTR")

spider.df <- data.frame(Fish = as.numeric(),
                        Prey = as.numeric(),
                        Macrophyte = as.numeric(),
                        Competitor = as.numeric(),
                        Conductivity = as.numeric(),
                        Salinity = as.numeric(),
                        pH = as.numeric(),
                        chl.a = as.numeric())

spider.df[3,1]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$Fish.Densitym2)
spider.df[3,2]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$Prey.CPU)
spider.df[3,3]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$Shoot.Countm2)
spider.df[3,4]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$Competitorm2)
spider.df[3,5]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$cond)
spider.df[3,6]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$salinity)
spider.df[3,7]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$pH)
spider.df[3,8]<-cor(sp.df.enex.entr$Slope, sp.df.enex.entr$chl.a)

spider.df[4,1]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$Fish.Densitym2)
spider.df[4,2]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$Prey.CPU)
spider.df[4,3]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$Shoot.Countm2)
spider.df[4,4]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$Competitorm2)
spider.df[4,5]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$cond)
spider.df[4,6]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$salinity)
spider.df[4,7]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$pH)
spider.df[4,8]<-cor(sp.df.enex.enve$Slope, sp.df.enex.enve$chl.a)

spider.df[5,1]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$Fish.Densitym2)
spider.df[5,2]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$Prey.CPU)
spider.df[5,3]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$Shoot.Countm2)
spider.df[5,4]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$Competitorm2)
spider.df[5,5]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$cond)
spider.df[5,6]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$salinity)
spider.df[5,7]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$pH)
spider.df[5,8]<-cor(sp.df.entr.enex$Slope, sp.df.entr.enex$chl.a)

spider.df[6,1]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$Fish.Densitym2)
spider.df[6,2]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$Prey.CPU)
spider.df[6,3]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$Shoot.Countm2)
spider.df[6,4]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$Competitorm2)
spider.df[6,5]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$cond)
spider.df[6,6]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$salinity)
spider.df[6,7]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$pH)
spider.df[6,8]<-cor(sp.df.entr.enve$Slope, sp.df.entr.enve$chl.a)

spider.df[7,1]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$Fish.Densitym2)
spider.df[7,2]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$Prey.CPU)
spider.df[7,3]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$Shoot.Countm2)
spider.df[7,4]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$Competitorm2)
spider.df[7,5]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$cond)
spider.df[7,6]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$salinity)
spider.df[7,7]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$pH)
spider.df[7,8]<-cor(sp.df.enve.enex$Slope, sp.df.enve.enex$chl.a)

spider.df[8,1]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$Fish.Densitym2)
spider.df[8,2]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$Prey.CPU)
spider.df[8,3]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$Shoot.Countm2)
spider.df[8,4]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$Competitorm2)
spider.df[8,5]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$cond)
spider.df[8,6]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$salinity)
spider.df[8,7]<-cor(sp.df.enve.entr$Slope, sp.df.enve.entr$pH)
spider.df[8,8]<-cor.test(sp.df.enve.entr$Slope, sp.df.enve.entr$chl.a)

rownames(spider.df) <- c('Max','Min','EX_TR', 'EX_VE', 'TR_EX', 'TR_VE',
                         'VE_EX', 'VE_TR')

colnames(spider.df)[8] <- "chl-a"

spider.df[1:2,1:8] <- c(1 , -1)
spider.df.m <- spider.df

#Radar chart
radarchart(spider.df.m[1:4,])

# Plot 2: Same plot with custom features
colors_border=c('#2b83ba', '#2b83ba')#, '#de2d26')

# colors_in=c(rgb(0,0,1,0.4), rgb(1,0,0,0.4)) #, rgb(0.7,0.5,0.1,0.4)

# pdf("Figures/StabilizingEffects_SlopeEnvCor_SpiderPlot.pdf", width=3.74, height=9)
# par(mfrow=c(3,1))

#ENEX
#Open and save EPS first in AI
#Seem to lose some resolution if go straight to Photoshop

setEPS()
postscript("Figures/StabilizingEffects_SpiderPlot_ENEX_mortality.eps", 
           onefile = F)
radarchart( spider.df.m[c(1:4),]  , axistype=0 , seg = 8,
            #custom polygon
            pcol=colors_border , plwd=3 , plty=c("solid","longdash"),
            #custom the grid
            cglcol="grey50", cglty=1,
            axislabcol="black",
            caxislabels=seq(-1, 1,0.25), cglwd=1,
            #custom labels
            vlcex=0.8
)
legend(x=1, y=1, legend = c(expression(italic('E. traviatum')),  
                            expression(italic('E. vesperum'))), 
       bty = "n", lwd=3, lty = c('solid','longdash'),
       col=colors_border ,
       text.col = "black", cex=1.2, pt.cex=3)
dev.off()

#ENTR
postscript("Figures/StabilizingEffects_SpiderPlot_ENTR_mortality.eps", 
           onefile = F)
radarchart( spider.df.m[c(1,2,5,6),]  , axistype=0 , seg = 8,
            #custom polygon
            pcol=c("#ff850f",'#ff850f') , plwd=3 , plty=c("dotted","longdash"),
            #custom the grid
            cglcol="grey50", cglty=1,
            axislabcol="black",
            caxislabels=seq(-1, 1,0.25), cglwd=1,
            #custom labels
            vlcex=0.8
)
legend(x=1, y=1, legend = c(expression(italic('E. exsulans')),  
                            expression(italic('E. vesperum'))), 
       bty = "n", lwd=3, lty = c('dotted','longdash'),
       col=c("#fdae61",'#fdae61') ,
       text.col = "black", cex=1.2, pt.cex=3)
dev.off()

#ENVE
postscript("Figures/StabilizingEffects_SpiderPlot_ENVE_mortality.eps", 
           onefile = F)
radarchart( spider.df.m[c(1,2,7,8),]  , axistype=0 , seg = 8,
            #custom polygon
            pcol=c('#d7191c','#d7191c') , plwd=3 , plty=c("dotted","solid"),
            #custom the grid
            cglcol="grey50", cglty=1,
            axislabcol="black",
            caxislabels=seq(-1, 1,0.25), cglwd=1,
            #custom labels
            vlcex=0.8,
            bg= NA
)
legend(x=1, y=1, legend = c(expression(italic('E. exsulans')),  
                            expression(italic('E. traviatum'))), 
       bty = "n", lwd=3, lty = c('dotted','solid'),
       col=c('#d7191c','#d7191c'),
       text.col = "black", cex=1.2, pt.cex=3)
dev.off()

#############################################
#Comparison of density dependent growth rates 
#############################################

past.gr <- read.csv('Data/Past.growth.rates.csv')

library(cowplot)
library(ggpubr)

gg.test <- 
  ggplot(data = past.gr, aes(x = density.no.m2, y = growth.rate, color = Species))+
  geom_jitter(aes(shape = State, color = Species), width = 10, height = 0.0001) +
  #scale_color_manual(values = c('#e66101','#fdb863','#b2abd2','#5e3c99'))+
  geom_smooth(method = 'lm', fullrange = TRUE)



past.growth.plot.data <- ggplot_build(gg.test)
# str(past.growth.plot.data)
# head(gg_data$data[[2]])
gg2 <- past.growth.plot.data$data[[2]]


past.gr.graph 
  ggplot(data = gg2, aes(x = x, y = y, color = colour))+
  geom_jitter(aes(shape = State, color = Species), width = 10, height = 0.0001) +
  scale_color_manual(values = c('#e66101','#fdb863','#b2abd2','#5e3c99'))+
  geom_smooth(method = 'lm', fullrange = TRUE, se = FALSE) +
  scale_shape_manual(values = c(15:17,8)) +
  xlab(expression(Density~(m^{2})))+
  ylab(expression(Per~capita~growth~(day^{-1}))) +
  theme_classic()+
  theme(legend.position = "none")
#  theme(legend.text = element_text(rep(face = "plain", 4), rep(face = "italic", 4)))


pdf("Figures/PastGrowth2.pdf", width=4, height=4)
past.gr.graph
dev.off()
