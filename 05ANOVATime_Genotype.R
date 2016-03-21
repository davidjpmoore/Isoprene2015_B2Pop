#Author: Dave Moore
#date 10/12/2015
#purpose: Estimate time & genotype effects on photosyntyhesis and isoprene

#loading dat_Iso1.Rda - isoprene unit conversions completed using 00_ReadIsoprene_UnitConversions
library(devtools)
library (plantecophys)
library(dplyr)
library(ggplot2)
library(grid) #required for 'unit'

data_Iso_02_Monson = read.csv("./data/FluxDataFiltered_AllDataForRuss_dat_Iso_01.csv")

load ("dat_Iso_01.Rda")

# create R date from date

target <- c("180-372", "49-177")

#Fig2Analysis01 = dat_Iso_01 %>%
  
Fig2Analysis01 = data_Iso_02_Monson %>%
  
  mutate(Genotype=line, Tleaf = Tref, MeasDate=date) %>%
  mutate(rDate = as.Date(MeasDate, "%m/%d/%Y")) %>%
  mutate(month=format(rDate, "%m")) %>%
  mutate(week=format(rDate, "%U")) %>%
  filter(Genotype %in% target) %>%
  filter(X == 1) %>%
  group_by(Genotype, Tleaf, MeasDate) %>%
  select(-date,-line, -Tref)

# Extract measurements at 400 ppm 

Fig2Analysis02 = Fig2Analysis01  %>%
  filter(CO2 == 400)

unique(Fig2Analysis02$Genotype)
  
Anet_week.lm = lm(Anet ~ week, data=Fig2Analysis02)
Iso_week.lm = lm(Iso_nmol_p_umol_p_sec_p_m2 ~ week, data=Fig2Analysis02)
summary(Anet_week.lm)
summary(Iso_week.lm)

#Plot photosynthesis
  plot (Fig2Analysis02$rDate, Fig2Analysis02$Anet)
  #Plot isoprene  
  plot (Fig2Analysis02$rDate, Fig2Analysis02$Iso_nmol_p_umol_p_sec_p_m2)

  
#carry out ANOVA with genotype and month AND TEMPERATURE 

  Fig2AnalysisANOVA_anet <- aov(Anet ~ Genotype  + ## Diet effect 
                                  Tleaf + #temperature effect
                                  week + #month effect
                                  #Error(Subject/Diet) + ## nesting of Subject in Diet 
                                  Tleaf*week +
                                  Tleaf*Genotype + # interaction of temperature * Genotype
                                  week*Genotype + ## interaction of month and genotype
                                  Tleaf*Genotype*week, ## three way interaction
                                data = Fig2Analysis02)

  summary(Fig2AnalysisANOVA_anet)
  
  aggregate(Anet~Genotype,data = Fig2Analysis02, FUN=length)
  drop1(Fig2AnalysisANOVA_anet,~.,test="F") # type III SS and F Tests
  
  Fig2AnalysisANOVA_Iso <- aov(Iso_nmol_p_umol_p_sec_p_m2 ~ Genotype  + ## Diet effect 
                             Tleaf +
                                 week +
                               
                             #Error(Subject/Diet) + ## nesting of Subject in Diet 
                               Tleaf*week+
                               Tleaf*Genotype +
                               week*Genotype + ## interaction of Time and Diet 
                                Tleaf*Genotype*week, 
                                
                           data = Fig2Analysis02)
  
  drop1(Fig2AnalysisANOVA_Iso,~.,test="F") # type III SS and F Tests
  
  #######SANS genotype effect because the design does not support the contrast (only one genotype was measured in Oct)
  
  
  #carry out ANOVA with genotype and month AND TEMPERATURE 
  
  Fig2AnalysisANOVA_anetSG <- aov(Anet ~ Tleaf + #temperature effect
                                  week + #month effect
                                  #Error(Subject/Diet) + ## nesting of Subject in Diet 
                                  Tleaf*week,
                                data = Fig2Analysis02)
  
  summary(Fig2AnalysisANOVA_anetSG)

  drop1(Fig2AnalysisANOVA_anetSG,~.,test="F") # type III SS and F Tests
  Fig2AnalysisANOVA_IsoSG <- aov(Iso_nmol_p_umol_p_sec_p_m2 ~ Tleaf +
                                 week +
                                  Tleaf*week, 
                               data = Fig2Analysis02)
  drop1(Fig2AnalysisANOVA_IsoSG,~.,test="F") # type III SS and F Tests
#PLOTS
  
  AnetbyGenotypeTemptime <- ggplot(Fig2Analysis02, aes(x=week, y=Anet))
  # 
  AnetbyGenotypeTemptime + aes(shape = factor(Tleaf)) + scale_shape(solid = FALSE, name ="Leaf Temperature") +
    geom_boxplot(lwd=1) +
    geom_point(aes( shape = factor(Tleaf)), size = 5, position = "jitter") +
    theme_classic() +
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=22,face="bold")) + 
    theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
    theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
    theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))+
    ylab("Anet")+
    xlab("Week") 
  
  
  
  IsobyGenotypeTemptime <- ggplot(Fig2Analysis02, aes(x=week, y=Iso_nmol_p_umol_p_sec_p_m2))
  # 
  IsobyGenotypeTemptime + aes(shape = factor(Tleaf)) + scale_shape(solid = FALSE, name ="Leaf Temperature") +
    geom_boxplot(lwd=1) +
    geom_point(aes( shape = factor(Tleaf)), size = 5, position = "jitter") +
    theme_classic() +
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=22,face="bold")) + 
    theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
    theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
    theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))+
    ylab("Iso_nmol_p_umol_p_sec_p_m2")+
    xlab("Week") 
  
  
  
  AnetbyIso_byWeek <- ggplot(Fig2Analysis02, aes(x=Iso_nmol_p_umol_p_sec_p_m2, y=Anet))
  # 
  AnetbyIso_byWeek + aes(color= factor(week), name ="week") +
    geom_point(aes( shape = factor(Tleaf)), size = 5) +
   scale_shape(solid = FALSE, name ="Temperature") +
    theme_classic() +
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=22,face="bold")) + 
    theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
    theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
    theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))+
    ylab("Anet")+
    xlab("Isoprene") 
  