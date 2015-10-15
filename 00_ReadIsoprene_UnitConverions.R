#00_testingPlantEcoPhys
#Author: Dave Moore
#Date: 09/05/2015
#Purpose: Read in Amberly's data and revise the unit conversions as per Russ's note


# download the source code for the package: https://cran.r-project.org/web/packages/plantecophys/index.html
#install.packages('C:/Users/dmoore1/Downloads/plantecophys_0.6-3.zip', repos = NULL, type="source")
#Manual: https://cran.r-project.org/web/packages/plantecophys/plantecophys.pdf 

library (plantecophys)
library(dplyr)
library(ggplot2)
library(grid) #required for 'unit'
#Load data
#Amberly's data from B2

B2Physiologymaster = tbl_df( read.table("./data/Neice_IsopreneAci2014.csv", na.strings=c('NA'), stringsAsFactors=FALSE, head=TRUE, sep=","))

summary(B2Physiologymaster)
unique(B2Physiologymaster$date)
#
# Note from Russ Monson
#
# 1. Amberly's isoprene calibration curves are in units of ppb, which converts straight through to nmol/mol.
# 2. Divide nmol/mol by 10^6 to get to nmol/umol.
# 3. Multiply by 500 umol s-1 to account for LiCor flow rate of air.
# 4. Divide by 6.2 cm^2 and multiply by 10,000 to get per unit leaf area.

#This should provide numbers between 15 and 50 nmol m-2 s-1, which is typical of isoprene emission rates. 


Rho_air25 = 1183.9 #mg/L
Rho_air35 = 1145.5 #mg/L


# 1. Amberly's isoprene calibration curves are in units of ppb, which converts straight through to nmol/mol.
#read in as : B2Physiologymaster$Standard.curve.equation 

#rename:
Iso_nmol_p_mol =B2Physiologymaster$Standard.curve.equation # Isoprene (nmol/mol)

# 2. Divide nmol/mol by 10^6 to get to nmol/umol.
Iso_nmol_p_umol = Iso_nmol_p_mol / 10^6 #Isoprene (nmol/umol air)

# 3. Multiply by 500 umol s-1 to account for LiCor flow rate of air.
Iso_nmol_p_umol_p_sec = Iso_nmol_p_umol*500 #Isoprene (nmol/umol air/sec) 

# 4. Divide by 6.2 cm^2 and multiply by 10,000 to get per unit leaf area.

Iso_nmol_p_umol_p_sec_p_m2 =  Iso_nmol_p_umol_p_sec/6.2*10000 #Isoprene (nmol/umol air/sec/m2)

#renaming variables
B2Physiologymaster$Anet = B2Physiologymaster$photosynthesis
B2Physiologymaster$gs = B2Physiologymaster$conductance

#Note there is an error in Amberly's spreadsheet 
#6/25/2014  	tree:D12	line: 180-372 - one of the Ci values is 8000
#point has been replaced with NA

#pull relevant columns to new data frame
dat_Iso_01 = select(B2Physiologymaster, date, tree, line, Tref, CO2, Anet, gs, Ci, CPS)
dat_Iso_01$Iso_nmol_p_mol = Iso_nmol_p_mol # Isoprene (nmol/mol)
dat_Iso_01$Iso_nmol_p_umol_p_sec_p_m2 = Iso_nmol_p_umol_p_sec_p_m2 


#Save data to Rdata file
save(dat_Iso_01, file="dat_Iso_01.Rda")




