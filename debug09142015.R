#00_testingPlantEcoPhys
#Author: Dave Moore
#Date: 09/05/2015
#Purpose: Test the Plantecophys package - a stand alone package to model common leaf gas exchange measurements


# download the source code for the package: https://cran.r-project.org/web/packages/plantecophys/index.html
#install.packages('PATH/plantecophys_0.6-3.zip', repos = NULL, type="source")
#Manual: https://cran.r-project.org/web/packages/plantecophys/plantecophys.pdf 

library (plantecophys)
library(dplyr)
library(ggplot2)
#Load data
#Amberly's data from B2

#loading dat_Iso1.Rda - isoprene unit conversions completed using 00_ReadIsoprene_UnitConversions
load ("dat_Iso_01.Rda")
#
# For the simple case - create a dataframe that is a subset of dat_Iso_01.Rda containing a single
# A/ci curve - I picked the line and data at random from a list of available lines and dates.
#
Junkaci02 = dat_Iso_01   %>% #piping command for filter
  #restrict to a single Aci curve filter out any Anet values that are too big
  filter(line=="49-177", date=="7/20/2014", Tref==25, Anet < 50) %>% #piping command for select
  #select only the Anet, gs, Ci variables
  mutate(PARi=1800)  %>%
  select(Anet,gs, Ci, Tref, PARi, line, date)    

#Aci plots
#
# Plotting the data used to fit this curve
#
ACi <- ggplot(Junkaci02, aes(x=Ci, y=Anet))

line_label <- Junkaci02$line[1]
date_label <- Junkaci02$date[1]

ACi + aes(shape = factor(Tref)) +
  ggtitle(paste("Line",line_label,"Date ",date_label,sep=" "))+
  geom_point(aes(colour = factor(Tref)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Assimilation (umol/m2/sec)")+
  xlab("Ci") 
#
#

#
# Using fitaci [library (plantecophys)] to estimate Vcmax and Jmax
#
# Note: you need to specify the dataframe and the variables that correspond to ALEAF, Tleaf, Ci and PPFD
# Note: I haven't worked out how to exclude outliers but PECAN:Photosynthesis has this function built in
#
load("data/acidata1.rda")

testACI=fitaci(acidata1)

line=as.list(dat_Iso_01$line)
dateMeas=as.list(dat_Iso_01$date)
CO2S=dat_Iso_01$CO2
Ci=dat_Iso_01$Ci
Tleaf=dat_Iso_01$Tref
Photo=dat_Iso_01$Anet

acidata2=data.frame(line,dateMeas,CO2S,Ci,Tleaf,Photo)

acidata3 = acidata2   %>% #piping command for filter
  #restrict to a single Aci curve filter out any Anet values that are too big
  filter(line=="49-177", dateMeas=="7/20/2014"), Tleaf==25, Photo < 50) %>% #piping command for select
  #select only the Anet, gs, Ci variables
  mutate(PARi=1800)  %>%
  select(CO2S,Ci,Tleaf,Photo) 


testACI2=fitaci(acidata2)


testACI

CheckACI= fitaci(Junkaci02, varnames = list(ALEAF = "Anet", Tleaf = "Tref", Ci = "Ci", PPFD="PARi"), Tcorrect = TRUE, citransition = NULL,
                 quiet = FALSE, startValgrid = FALSE, algorithm = "default", useRd = FALSE )

plot(CheckACI$df$Amodel, CheckACI$df$Ameas)

# 
# ACi_fit <- ggplot(CheckACI$df, aes(x=Ci, y=Amodel))
# 
# line_label <- Junkaci02$line[1]
# date_label <- Junkaci02$date[1]
# 
# ACi_fit + geom_point(colour="grey90", size = 2.5) +
#   theme_classic() +
#   theme(axis.text=element_text(size=20),
#         axis.title=element_text(size=22,face="bold")) + 
#   theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
#   theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
#   ylab("Modelled Assimilation (umol/m2/sec)")+
#   xlab("Ci") 