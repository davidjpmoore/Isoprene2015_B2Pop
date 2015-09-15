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
Junkaci02 = dat_Iso_01   %>% #piping command for filter
  #restrict to a single Aci curve filter out any Anet values that are too big
  filter(line=="49-177", date=="7/20/2014", Tref==25, Anet < 50) %>% #piping command for select
  #select only the Anet, gs, Ci variables
  mutate(PARi=1800, dateMeas=date, CO2S=CO2, Ci=Ci, Tleaf=Tref, Photo=Anet)  %>%
  select(line,dateMeas,CO2S,Ci,Tleaf,Photo, PARi)    

CheckACI= fitaci(Junkaci02)
#######################################
# WORKS! With Developer version 0.6.6 #
#######################################

########################
#check data is not funky
########################

#Aci plots
#
# Plotting the data used to fit this curve
#
ACi <- ggplot(Junkaci02, aes(x=Ci, y=Photo))
# 
line_label <- Junkaci02$line[1]
date_label <- Junkaci02$date[1]
# 
ACi + aes(shape = factor(Tleaf)) +
  ggtitle(paste("Line",line_label,"Date ",date_label,sep=" "))+
  geom_point(aes(colour = factor(Tleaf)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Assimilation (umol/m2/sec)")+
  xlab("Ci") 


##################################
#Use Mutate and Filter THEN filter
##################################

acidata2_N=dat_Iso_01  %>% 
  mutate(line=as.factor(line),dateMeas=as.factor(date),CO2S=CO2,Ci=Ci,Tleaf=Tref,Photo=Anet) %>% 
  select(line,dateMeas,CO2S,Ci,Tleaf,Photo)

acidataN = acidata2_N   %>% #piping command for filter
  #restrict to a single Aci curve filter out any Anet values that are too big
  filter(line=="49-177", dateMeas=="7/20/2014", Tleaf==25, Photo < 50) %>% #piping command for select
  #select only the Anet, gs, Ci variables
  mutate(PARi=1800)  %>%
  select(line,dateMeas,CO2S,Ci,Tleaf,Photo, PARi) 

testACI2_N=fitaci(acidataN)

#######################################
# WORKS! With Developer version 0.6.6 #
#######################################

#dplyr is doing something odd and I can't work out what 

#Sanity check 
#Download acidata from Remko Duursma's website
load("data/acidata1.rda")
testACI=fitaci(acidata1)

# Works just fine .... e.g.
# 
# Result of fitaci.
# 
# Data and predictions:
#   Ci      Ameas     Amodel         Ac        Aj      Rd VPD    Tleaf PPFD Patm
# 1    72.81690 -0.6656991 -0.7314450  0.6051437  1.233113 1.33653 1.5 33.36515 1800  101
# 2    89.33801  0.6089389  0.5060348  1.8427683  3.513934 1.33653 1.5 33.34065 1800  101
# 3   119.73218  2.4030110  2.7087383  4.0458383  6.918011 1.33653 1.5 33.31123 1800  101
# 4   163.84422  5.5908708  5.7507500  7.0887122 10.595420 1.33653 1.5 33.29358 1800  101


#Pulling out each column into it's own variable (as I'm trying to do above with dplyr)
line=dat_Iso_01$line
dateMeas=dat_Iso_01$date
CO2S=dat_Iso_01$CO2
Ci=dat_Iso_01$Ci
Tleaf=dat_Iso_01$Tref
Photo=dat_Iso_01$Anet
#
#

acidata2=data.frame(line,dateMeas,CO2S,Ci,Tleaf,Photo)

acidata3 = acidata2   %>% #piping command for filter
  #restrict to a single Aci curve filter out any Anet values that are too big
  filter(line=="49-177", dateMeas=="7/20/2014", Tleaf==25, Photo < 50) %>% #piping command for select
  #select only the Anet, gs, Ci variables
  mutate(PARi=1800)  %>%
  select(line,dateMeas,CO2S,Ci,Tleaf,Photo, PARi) 

testACI3=fitaci(acidata3)
###################
#works just fine! #
###################

# Result of fitaci.
# 
# Data and predictions:
#   Ci Ameas     Amodel         Ac        Aj        Rd VPD Tleaf PPFD Patm
# 5   47.1   0.2  0.3015783  0.5612759  1.186320 0.2596472 1.5    25 1800  101
# 4   77.7   3.8  4.0742509  4.3344489  7.744315 0.2596472 1.5    25 1800  101
# 3  136.0  10.5 10.5058314 10.7680780 15.224080 0.2596472 1.5    25 1800  101
# 2  190.0  15.9 15.7166237 15.9838859 19.328122 0.2596472 1.5    25 1800  101
# 1  245.0  20.3 20.4014331 20.6901496 22.129572 0.2596472 1.5    25 1800  101

plot(testACI3$df$Amodel, testACI3$df$Ameas)

#####################################################################################################
# In release 09/14/2015 two files don't provide the same result from fitaci                         #
# Using developer version the local data frame and regular data frame give same result with no error#
#####################################################################################################

# 
# One possible explanation is that acidataN is a "local data frame" , while acidata3 and acidata1 (example set)
# are both just dataframes
# 
# See this thread:
# http://stackoverflow.com/questions/29084380/what-is-the-meaning-of-the-local-data-frame-message-from-dplyrprint-tbl-df


# acidata3
# 
# line  dateMeas CO2S    Ci Tleaf Photo PARi
# 1  49-177 7/20/2014  400 245.0    25  20.3 1800
# 2  49-177 7/20/2014  300 190.0    25  15.9 1800
# 3  49-177 7/20/2014  200 136.0    25  10.5 1800
# 4  49-177 7/20/2014  100  77.7    25   3.8 1800
# 5  49-177 7/20/2014   50  47.1    25   0.2 1800
# 6  49-177 7/20/2014  400 284.0    25  23.2 1800
# 7  49-177 7/20/2014  600 449.0    25  28.9 1800
# 8  49-177 7/20/2014  800 627.0    25  30.2 1800
# 9  49-177 7/20/2014 1000 804.0    25  30.3 1800
# 10 49-177 7/20/2014 1200 982.0    25  29.7 1800

#This works with fitaci



# acidataN
# 
# Source: local data frame [10 x 7]
# 
# line  dateMeas CO2S    Ci Tleaf Photo PARi
# 1  49-177 7/20/2014  400 245.0    25  20.3 1800
# 2  49-177 7/20/2014  300 190.0    25  15.9 1800
# 3  49-177 7/20/2014  200 136.0    25  10.5 1800
# 4  49-177 7/20/2014  100  77.7    25   3.8 1800
# 5  49-177 7/20/2014   50  47.1    25   0.2 1800
# 6  49-177 7/20/2014  400 284.0    25  23.2 1800
# 7  49-177 7/20/2014  600 449.0    25  28.9 1800
# 8  49-177 7/20/2014  800 627.0    25  30.2 1800
# 9  49-177 7/20/2014 1000 804.0    25  30.3 1800
# 10 49-177 7/20/2014 1200 982.0    25  29.7 1800


#This does NOT work with fitaci
