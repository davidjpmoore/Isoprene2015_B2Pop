#01_Acianalysis
#Author: Dave Moore
#Date: 09/19/2015
#Purpose: Carry out ACI analysis for Monson et al 2015 Isoprene in Poplar

# Using fitaci [library (plantecophys)] to estimate Vcmax and Jmax
#
# Note: you need to specify the dataframe and the variables that correspond to ALEAF, Tleaf, Ci and PPFD
# Note: I haven't worked out how to exclude outliers in any sensible way 
#    but PECAN:Photosynthesis has this function built in - perhaps I can use them together?

# download the source code for the package: https://cran.r-project.org/web/packages/plantecophys/index.html
#install.packages('PATH/plantecophys_0.6-3.zip', repos = NULL, type="source")
#Manual: https://cran.r-project.org/web/packages/plantecophys/plantecophys.pdf 

# updated R 09/14/2015
# updated Rstudio 09/14/2015
# updated dplyr 09/14/2015
# updated plantecophys to developer version 0.6.6 on 09/19/2015
# library(devtools)
# install_bitbucket("remkoduursma/plantecophys")

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

##########################################
#Use filter, mutate and select in one step
##########################################

# For the simple case - create a dataframe that is a subset of dat_Iso_01.Rda containing a single
# A/ci curve - I picked the line and data at random from a list of available lines and dates.
#
Junkaci_001 = dat_Iso_01   %>% #piping command for filter
  #restrict to a single Aci curve filter out any Anet values that are too big
  filter(Anet < 50) %>% #piping command for mutate
  #create new variables (rename them) to be consistent with fitacis
  #Grouping by genetic line, measurement date and reference temperature
  mutate(ACIgroups=as.factor(paste(line, date, Tref, sep="")), PARi=1800, dateMeas=date, CO2S=CO2, Ci=Ci, Tleaf=Tref, Photo=Anet, Patm=91)  %>%
  
  #select only these variables
  select(line,dateMeas,ACIgroups, CO2S,Ci,Tleaf,Photo, PARi, Patm)  

#group by ACIgroups
Junkaci_002 = Junkaci_001 %>%
  group_by(ACIgroups)

CheckACI_dplyr= fitacis(Junkaci_002, "ACIgroups")

# save(Junkaci_002, file="output/IsopreneACIs_Amberly_grouped.csv")
# 
# Junkaci_002 %>% write.csv(.,file = "output/IsopreneACIs_Amberly_grouped.csv")

#hacked groups into simple catagories outside R = A1, B1...etc
#removed outlier points where LI6400 was not stable & caused failure of fit
IsopreneACIs_outlyrsRmoved=read.csv("data/IsopreneACIs_Amberly_hackedgroups.csv")
IsopreneACI_fitsbycurve= fitacis(IsopreneACIs_outlyrsRmoved, "ACIgroups")

IsopreneACI_coef <- coef(IsopreneACI_fitsbycurve)

unique(IsopreneACIs_outlyrsRmoved$ACIgroups)

#select variables that define treatments
Trts_IsopreneACI00 = IsopreneACIs_outlyrsRmoved %>%
  select(line, dateMeas, ACIgroups, Tleaf)

#reduce to unique definition rows
Trts_IsopreneACI = Trts_IsopreneACI00 %>%
  distinct(ACIgroups)

#merge data frames to regain the Genotype and Temperature
IsopreneACI_coef_byTRT01 = inner_join(Trts_IsopreneACI,IsopreneACI_coef, by="ACIgroups" )

IsopreneACI_coef_byTRT = IsopreneACI_coef_byTRT01  %>%
  mutate(Genotype=line, MeasDate=dateMeas)   %>%
  group_by(Genotype, Tleaf)   %>%
  select(-dateMeas,-line)
  
  

#ANOVA and statistical tests

Vcmax_ANOVA <- aov( Vcmax ~ Genotype + Tleaf + Genotype:Tleaf, data=IsopreneACI_coef_byTRT)
summary(Vcmax_ANOVA)

Jmax_ANOVA <- aov( Jmax ~ Genotype + Tleaf + Genotype:Tleaf, data=IsopreneACI_coef_byTRT)

summary(Jmax_ANOVA)

Vcmax_ANOVA01<-lme(Vcmax~Genotype|Tleaf,data=IsopreneACI_coef_byTRT)
summary(Vcmax_ANOVA)

library(lme4)

library(reshape2)
melted <- melt(IsopreneACI_coef_byTRT, id.vars=c("Genotype", "Tleaf"))
anova(lm(IsopreneACI_coef_byTRT$Vcmax ~ IsopreneACI_coef_byTRT$Genotype * IsopreneACI_coef_byTRT$Tleaf), IsopreneACI_coef_byTRT)

plot(IsopreneACI_coef_byTRT$Genotype, IsopreneACI_coef_byTRT$Vcmax)
plot(IsopreneACI_coef_byTRT$Tref[IsopreneACI_coef_byTRT$Vcmax>0], IsopreneACI_coef_byTRT$Vcmax[IsopreneACI_coef_byTRT$Vcmax>0])


#####PLOTS

VcmaxbyGenotype <- ggplot(IsopreneACI_coef_byTRT, aes(x=Genotype, y=Vcmax))
# 
VcmaxbyGenotype + aes(shape = factor(Tleaf)) +
  geom_boxplot(outlier.colour = "black") +
  geom_point(aes( colour = factor(Tleaf)), size = 3, position = "jitter") +
    theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Vcmax")+
  xlab("Genotype") 

JmaxbyGenotype <- ggplot(IsopreneACI_coef_byTRT, aes(x=Genotype, y=Jmax))
# 
VcmaxbyGenotype + aes(shape = factor(Tleaf)) +
  geom_boxplot(outlier.colour = "black") +
  geom_point(aes( colour = factor(Tleaf)), size = 3, position = "jitter") +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Jmax")+
  xlab("Genotype") 


qplot(Genotype, hwy, data = IsopreneACI_coef_byTRT, geom = c("boxplot", "jitter"))
