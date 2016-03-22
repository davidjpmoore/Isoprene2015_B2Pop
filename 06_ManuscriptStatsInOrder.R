#Manuscript stats IN ORDER of appearance in paper version 12/30/2015

library(devtools)
library (plantecophys)
library(dplyr)
library(ggplot2)
library(grid) #required for 'unit'

# Line 184
# Photosynthesis rates were not significantly lower when measured at a leaf temperature of 35 C, and compared to 25 C across the entire growing season (P > 0.05), though the range of values at 35 C tended to be slightly lower (15 – 24 mol m-2 s-1) (Fig. 2B). 
#loading dat_Iso1.Rda - isoprene unit conversions completed using 00_ReadIsoprene_UnitConversions
# load ("dat_Iso_01.Rda")
# write.csv(dat_Iso_01, file = "AllDataForRuss_dat_Iso_01.csv")
# 
# load data used in generating Fig 2
dat_Iso_01_flagged=read.csv(file="./data/Flagged_dat_Iso_01.csv", head=TRUE )

# 
# aggregate(Anet~line,data = dat_Iso_01_flagged, FUN=length)
target <- c("180-372", "49-177")
# create R date from date

#Assign correct date format - filter unstable licor observations
Fig2Analysis01 = dat_Iso_01_flagged %>%
  mutate(Genotype=line, Tleaf = Tref, MeasDate=date) %>%
  mutate(rDate = as.Date(MeasDate, format="%m/%d/%Y")) %>%
  mutate(month=format(rDate, "%m")) %>%
  mutate(week=format(rDate, "%U")) %>%
  group_by(Genotype, Tleaf, MeasDate) %>%
 filter(GOOD==1) %>%
  select(-date,-line, -Tref)

unique(Fig2Analysis01$rDate)

# Extract measurements at 400 ppm 
GenotypeComparison =Fig2Analysis01 %>%
  filter(Genotype %in% target) %>%
  filter(CO2 == 400) 

# Photosynthesis rates were not significantly 
# lower when measured at a leaf temperature of 35 C, 
# and compared to 25 C across the entire growing season (P > 0.05), 

t.test(GenotypeComparison$Anet ~ GenotypeComparison$Tleaf)


t.test(GenotypeComparison$Anet ~ GenotypeComparison$Genotype)


# Leaf isoprene emission rates were significantly greater at 35 C,
# compared to 25 C, at all observation times during the growing season 
# (P < 0.05; Fig. 2).

t.test(GenotypeComparison$Iso_nmol_p_umol_p_sec_p_m2 ~ GenotypeComparison$Tleaf)
t.test(GenotypeComparison$Iso_nmol_p_umol_p_sec_p_m2 ~ GenotypeComparison$Genotype)

# 
# plot(GenotypeComparison$Genotype,GenotypeComparison$Iso_nmol_p_umol_p_sec_p_m2)
# plot(GenotypeComparison$month, GenotypeComparison$Iso_nmol_p_umol_p_sec_p_m2)
# plot(GenotypeComparison$month, GenotypeComparison$Anet)



     
DateEffectComparison_anet <- aov(Anet ~  month  + ## Genotype effect 
                                 Genotype +
                                   Tleaf + #temperature effect
                                 Tleaf*month, # interaction of temperature * Genotype
                                 
                                 data = GenotypeComparison)
summary(DateEffectComparison_anet)

aggregate(Anet~Genotype,data = GenotypeComparison, FUN=mean)

DateEffectComparison_Iso <- aov(Iso_nmol_p_umol_p_sec_p_m2 ~ month  + ## Genotype effect  
                                  Genotype +
                                 Tleaf + #temperature effect
                               
                                  Tleaf*month, # interaction of temperature * Genotype
                               data = GenotypeComparison)

  summary(DateEffectComparison_Iso)
  aggregate(Iso_nmol_p_umol_p_sec_p_m2~Genotype,data = GenotypeComparison, FUN=mean)


  
# Fig2Analysis02 = Fig2Analysis01  %>%
#   filter(Genotype %in% target) %>%
#   filter(CO2 == 400) 
# 
# 
# Anet49177only = Fig2Analysis02 %>%
#   filter(Genotype =="49-177") 
# 
# # #Plot photosynthesis
# # plot (Fig2Analysis02$rDate, Fig2Analysis02$Anet)
# 
# 
# 
# # #Plot isoprene  
# # plot (Fig2Analysis02$rDate, Fig2Analysis02$Iso_nmol_p_umol_p_sec_p_m2)
# 
# 
# #carry out ANOVA with genotype and month AND TEMPERATURE 
# replications(Anet ~ Tleaf*week, data=Anet49177only)
# Fig2AnalysisANOVA_anet <- aov(Anet ~ Genotype  + ## Genotype effect 
#                                 Tleaf + #temperature effect
#                                 week + #month effect
#                                 
#                                 Tleaf*week + #effect of time
#                                 Tleaf*Genotype + # interaction of temperature * Genotype
#                                 week*Genotype + ## interaction of week and genotype
#                                 Tleaf*Genotype*week, ## three way interaction
#                               data = Fig2Analysis02)
# 
# summary(Fig2AnalysisANOVA_anet)
# aggregate(Anet~Tleaf,data = Fig2Analysis02, FUN=range)
# 
# 
# #reduced ANOVA
# Anet49177onlyANOVA_anet <- aov(Anet ~ Tleaf + #temperature effect
#                                 week + #week effect
#                                 Tleaf*week, #Interaction
#                               data = Anet49177only)
# 
# summary(Anet49177onlyANOVA_anet)
# 
# 
# 
# # Line 186
# # Leaf isoprene emission rates were significantly greater at 35 C, compared to 25 C, at all observation times during the growing season (P < 0.05; Fig. 2). 
# 
# Fig2AnalysisANOVA_ISO <- aov(Iso_nmol_p_umol_p_sec_p_m2 ~  
#                                 Tleaf + #temperature effect
#                                 week + #month effect
#                                 
#                                 Tleaf*week, #effect of time
#                               data = Fig2Analysis02)
# 
# summary(Fig2AnalysisANOVA_ISO)
# 
# Fig2Analysis02_25 = Fig2Analysis02 %>%
#   filter(Tleaf ==25) 
# 
# Fig2Analysis02_35 = Fig2Analysis02 %>%
#   filter(Tleaf ==35) 
# 
# range25_byWeek= aggregate(Iso_nmol_p_umol_p_sec_p_m2~week,data = Fig2Analysis02_25, FUN=range)
# 
# range35_byWeek= aggregate(Iso_nmol_p_umol_p_sec_p_m2~week,data = Fig2Analysis02, FUN=range)
# 
# 
# 
# 
# GenoWeeks =  c(22,23,25,28,29)
# 
# TwoGenotypes = Fig2Analysis02 %>%
#   filter(week %in% GenoWeeks)
# 
# aggregate(Iso_nmol_p_umol_p_sec_p_m2~Genotype,data = TwoGenotypes, FUN=mean)
# 
# 
# # #Line 191
# # The values for Is did not differ significantly among the three genetic lines at any observed phase of the growing season (P > 0.05),
# 
# 
# # #Line 193
# # The temperature sensitivity of Is varied significantly, depending on date during the growing season (P < 0.05). 
# 
# 
# 
# 
# # Statistical analyses
# # Dave, please add this section. I have indicated with yellow highlighter in the Results section where statistical significance values were added.
# 
# 
# #Plots
# 
# ##########################
# # Figure 2 Photosynthesis
# ##########################
# 
# Fig2A_Photo <- ggplot(Anet49177only, aes(x=rDate, y=Anet ))
# 
# Fig2A_Photo +  geom_point(aes(colour = factor(Tleaf)), size = 8) + #different colors for different Leaf Temperatures
#   theme_classic() + #apply classic theme (gets rid of grey background)
#   theme(axis.text=element_text(size=20),
#         axis.title=element_text(size=22,face="bold")) + 
#   theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
#   theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
#   theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))+ #provide negative value for tick length = they face inwards
#   ylab("A net (umol/m2/sec)")+
#   xlab("date") 
# 
# 
# #####################
# # Figure 2 Isoprene
# #####################
# 
# 
Fig2_Iso <- ggplot(GenotypeComparison, aes(x=rDate, y=Iso_nmol_p_umol_p_sec_p_m2 ))
Fig2_Iso + aes(shape = factor(Genotype)) + #different symbols for different Genotypes
  
  geom_point(aes(colour = factor(Tleaf)), size = 8) + #different colors for different Leaf Temperatures
  theme_classic() + #apply classic theme (gets rid of grey background)
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))+ #provide negative value for tick length = they face inwards
  ylab("Isoprene (nmol/m2/sec)")+
  xlab("date") 
