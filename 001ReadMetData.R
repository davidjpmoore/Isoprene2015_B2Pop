#Purpose: to read in met data and carry out basic analysis
#Author Dave Moore
#Date: 03/12/2015

#B2 Met tower data should be stored on iPlant

#HEADER
# TIMESTAMP  RECORD	AirTC_Avg	RH	WS_ms_Avg	WindDir	Rain_mm_Tot	PAR_Den_Avg	PAR_Tot_Tot	PAR_Den
# TS	RN	Deg C	%	meters/second	degrees	mm	umol/s/m^2	mmol/m^2	umol/s/m^2
# Avg	Smp	Avg	Smp	Tot	Avg	Tot	Smp

#NOTE: from Ian Shiach: Time zone is Arizona (Mountain Standard). (3/9/2015), the time was one hour fast (the 11:45AM point was really 10:45AM). Time was reset to Arizona (Mountain Standard) on 3/9/2015.

#READ MET DATA FROM CSV FILE
Met_dat = read.csv("data//B2PlantationMetData092013_032015.csv",skip=5,header=FALSE, na.strings=c('-9999','-6999'), stringsAsFactors=FALSE)

#READ MET DATA HEADER FROM CSV FILE
Met_header = read.csv("data//B2PlantationMetData092013_032015.csv",skip=2,nrows=1,header=FALSE, na.strings=c('-9999','-6999'), stringsAsFactors=FALSE)
#create LIST from header data frame
MetCols = as.list(Met_header)

#READ MET DATA UNITS FROM CSV FILE
Met_units  = read.csv("data//B2PlantationMetData092013_032015.csv",skip=3,nrows=2,header=FALSE, na.strings=c('-9999','-6999'), stringsAsFactors=FALSE)

#apply column headers to Met_dat
colnames(Met_dat) <- MetCols

#apply column headers to Met_units
colnames(Met_units) <- MetCols

#strip out the the leading text of each element of TIMESTEP
datestamp = gsub( " .*$", "", Met_dat$TIMESTAMP )
#apply date format to the text
Met_dat$rDate <- as.Date(datestamp,"%m/%d/%Y")

#apply date format to TIMESTAMP directly | this works I haven't tried extracting the time of day from it yet
Met_dat$rDateT <- as.Date(Met_dat$TIMESTAMP, "%m/%d/%Y %H:%M")

# Plots - these are VERY BIG files
# library(ggplot2)
# p <- ggplot(Met_dat, aes(rDateT, AirTC_Avg))
# p + geom_point(colour = "red", size = 3)

