#00_testingPlantEcoPhys
#Author: Dave Moore
#Date: 09/05/2015
#Purpose: Test the Plantecophys package - a stand alone package to model common leaf gas exchange measurements


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
# updated plantecophys to developer version 0.6.6
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


CheckACI= fitacis(Junkaci_002, "ACIgroups")
##################################################################
# With Developer version 0.6.6                                   #
# DOES NOT FUNCTION:                                             #
#Error: data_frames can only contain 1d atomic vectors and lists #
##################################################################


#######################################
# WORKS! With Developer version 0.6.6 #
#######################################

###########################################################################
#check grouping functioned OK and check for stay points in Amberly's data #
###########################################################################

#Aci plots
#
# Plotting the data used to fit this curve BY GROUP
#
ACi <- ggplot(Junkaci_002, aes(x=Ci, y=Photo))
# 
ACi + aes(shape = factor(Tleaf)) +
  ggtitle(paste("Group", ACIgroups, sep=" "))+
  geom_point(aes(colour = factor(Tleaf)), size = 8) +
  #geom_point(aes(group = ACIgroups))+
  facet_wrap(~ ACIgroups) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Assimilation (umol/m2/sec)")+
  xlab("Ci") 
