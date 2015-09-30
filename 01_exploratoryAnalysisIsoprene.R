#Date: 09/05/2015
#author: Dave Moore
#purpose: explore isoprene data from 2014 B2 Poplar Plantation

#Libraries
library (plantecophys)
library(dplyr)
library(ggplot2)
library(grid) #required for 'unit'

load ("dat_Iso_01.Rda")

#check unique dates and lines
unique(dat_Iso_01$date) 
unique(dat_Iso_01$line)

# #group by ACIgroups
# Junkaci_002 = Junkaci_001 %>%
#   group_by(ACIgroups)
dat_Iso_02 = dat_Iso_01 %>%
mutate(LongACIgroups=as.factor(paste(line, date, Tref, sep="")), Genotype=line, MeasDate=date, CO2S=CO2, Ci=Ci, Tleaf=Tref, Photo=Anet)  %>%
  select(-date,-line) %>%
group_by(LongACIgroups)

dat_Iso_02norm= dat_Iso_02 %>% group_by(Genotype, MeasDate, Tleaf) %>%
  filter(Photo<50) %>%
  mutate(Iso.norm = Iso_nmol_p_umol_p_sec_p_m2 / max(Iso_nmol_p_umol_p_sec_p_m2),
         Photo.norm = Photo / max(Photo)) 

plot (dat_Iso_02norm$Ci, dat_Iso_02norm$Iso.norm)
summary(dat_Iso_02)
library(grid) #required for 'unit'
#Isoprene Plots
IsoCi <- ggplot(dat_Iso_02norm, aes(x=Ci, y=Iso.norm ))

IsoCi + aes(shape = factor(Genotype)) + #different symbols for different Genotypes

  geom_point(aes(colour = factor(Tleaf)), size = 8) + #different colors for different Leaf Temperatures
  theme_classic() + #apply classic theme (gets rid of grey background)
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))+ #provide negative value for tick length = they face inwards
  ylab("Isoprene (nmol/m2/sec)")+
  xlab("Ci") 


filter(junk1, date==dateloop[i], Anet < 50)


