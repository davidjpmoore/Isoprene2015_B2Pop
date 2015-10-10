#01_Acianalysis
#Author: Dave Moore
#Date: 09/19/2015
#Purpose: Carry out statistical analysis of ACI curves Monson et al 2015 Isoprene in Poplar



library(devtools)
library (plantecophys)
library(dplyr)
library(ggplot2)
library(grid) #required for 'unit'
#Load data
#Amberly's data from B2

#loading dat_Iso1.Rda - isoprene unit conversions completed using 00_ReadIsoprene_UnitConversions
load ("dat_Iso_01.Rda")
#to make this from scratch just run: 00_ReadIsoprene_UnitConverions.R 


load("IsopreneACI_coef_byTRT.Rda")
load("IsopreneACI_coef_byTRT01.Rda")
#to make these from scratch just run: 020_Process ACI curves.R 

#ANOVA and statistical tests

library(lme4)
library(reshape2)
Vcmax_ANOVA <- aov( Vcmax ~ Genotype + Tleaf + Genotype:Tleaf, data=IsopreneACI_coef_byTRT)
summary(Vcmax_ANOVA)

Jmax_ANOVA <- aov( Jmax ~ Genotype + Tleaf + Genotype:Tleaf, data=IsopreneACI_coef_byTRT)

summary(Jmax_ANOVA)

Vcmax_ANOVA01<-lme(Vcmax~Genotype|Tleaf,data=IsopreneACI_coef_byTRT)
summary(Vcmax_ANOVA)




melted <- melt(IsopreneACI_coef_byTRT, id.vars=c("Genotype", "Tleaf"))
anova(lm(IsopreneACI_coef_byTRT$Vcmax ~ IsopreneACI_coef_byTRT$Genotype * IsopreneACI_coef_byTRT$Tleaf), IsopreneACI_coef_byTRT)

plot(IsopreneACI_coef_byTRT$Genotype, IsopreneACI_coef_byTRT$Vcmax)

plot(IsopreneACI_coef_byTRT$Genotype, IsopreneACI_coef_byTRT$Jmax)

plot(IsopreneACI_coef_byTRT$Tref[IsopreneACI_coef_byTRT$Vcmax>0], IsopreneACI_coef_byTRT$Vcmax[IsopreneACI_coef_byTRT$Vcmax>0])


#####PLOTS

VcmaxbyGenotype <- ggplot(IsopreneACI_coef_byTRT, aes(x=Genotype, y=Vcmax))
# 
VcmaxbyGenotype + aes(shape = factor(Tleaf)) + scale_shape(solid = FALSE, name ="Leaf Temperature") +
  geom_boxplot(lwd=1) +
  geom_point(aes( shape = factor(Tleaf)), size = 5, position = "jitter") +
    theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))+
  ylab("Vcmax")+
  xlab("Genotype") 

JmaxbyGenotype <- ggplot(IsopreneACI_coef_byTRT, aes(x=Genotype, y=Jmax))
# 

JmaxbyGenotype + aes(shape = factor(Tleaf)) + scale_shape(solid = FALSE, name ="Leaf Temperature") +
  geom_boxplot(lwd=1.25) +
  geom_point(aes( shape = factor(Tleaf)), size = 5, position = "jitter") +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))+
  ylab("Jmax")+
  xlab("Genotype") 




