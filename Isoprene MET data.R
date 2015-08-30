#Purpose: Read in MET data from B2 tower and calculare sums by month 
#Paper: Isoprene paper led by Russ Monson
#Author Dave Moore
#Date: 08/02/2015

#HEADER
# TIMESTAMP  RECORD  AirTC_Avg	RH	WS_ms_Avg	WindDir	Rain_mm_Tot	PAR_Den_Avg	PAR_Tot_Tot	PAR_Den
# TS	RN	Deg C	%	meters/second	degrees	mm	umol/s/m^2	mmol/m^2	umol/s/m^2
# Avg	Smp	Avg	Smp	Tot	Avg	Tot	Smp

#NOTE: from Ian Shiach: Time zone is Arizona (Mountain Standard). (3/9/2015), the time was one hour fast (the 11:45AM point was really 10:45AM). Time was reset to Arizona (Mountain Standard) on 3/9/2015.

#READ MET DATA FROM CSV FILE
#NOTE: MET DATA is on iPlant
Met_dat = read.csv("data//B2PlantationMetData092013_032015.csv",skip=5,header=FALSE, na.strings=c('-9999','-6999'), stringsAsFactors=FALSE)

#READ MET DATA HEADER FROM CSV FILE
#NOTE: MET DATA is on iPlant
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
 library(ggplot2)
# p <- ggplot(Met_dat, aes(rDateT, AirTC_Avg))
# p + geom_point(colour = "red", size = 3)


#pull year into its own variable
Met_dat$Year =format(Met_dat$rDate, "%Y") 

#pull Month into it's own variable
Met_dat$Month =format(Met_dat$rDate, "%m") 

#pull Month into it's own variable
Met_dat$Week =format(Met_dat$rDate, "%W") 

#pull day into it's own variable
Met_dat$Day =format(Met_dat$rDate, "%d") 

#create subset of data for 2014
met2014 <- subset(Met_dat, Met_dat$Year>2013 | Met_dat$Year<2015) 
#junk - check sums are correct
#junk1 = met2014$PAR_Den_Avg*60*5/1000
#plot(junk1,met2014$PAR_Tot_Tot)

#met2014$PAR_Tot_Tot is the sum of met2014$PAR_Den_Avg
#met2014$PAR_Tot_Tot units: mmol/m^2 per timestep (5 minutes)

#counter

#monthly numbers
Monthly_day=(tapply(met2014$Day, met2014$Month, max))
Monthly_count=as.numeric(Monthly_day)
Monthly_month=(tapply(met2014$Month, met2014$Month, max))

Monthly_PAR_mmol_perMonth=(tapply(met2014$PAR_Tot_Tot, met2014$Month, sum))
Monthly_PAR_mmol_perDay=Monthly_PAR_mmol_perMonth/Monthly_count

Monthly_RH=(tapply(met2014$RH, met2014$Month, mean))
Monthly_Rain_mm_tot=(tapply(met2014$Rain_mm_Tot, met2014$Month, sum))
Monthly_AirTC_Avg=(tapply(met2014$AirTC_Avg, met2014$Month, mean))

plot(Monthly_PAR_mmol_perMonth)

#weekly numbers

Weekly_count=7
Weekly_month = (tapply(met2014$Month, met2014$Week, max))
Weekly_PAR_mmol_perWeek=(tapply(met2014$PAR_Tot_Tot, met2014$Week, sum))

Weekly_PAR_mmol_perDay=Weekly_PAR_mmol_perWeek/Weekly_count


Weekly_RH=(tapply(met2014$RH, met2014$Week, mean))
Weekly_Rain_mm_tot=(tapply(met2014$Rain_mm_Tot, met2014$Week, sum))
Weekly_AirTC_Avg=(tapply(met2014$AirTC_Avg, met2014$Week, mean))

plot(Weekly_month,Weekly_PAR_mmol_perWeek)
plot(Weekly_month,Weekly_PAR_mmol_perDay)

plot(Weekly_month,Weekly_AirTC_Avg)
plot(Weekly_month,Weekly_Rain_mm_tot)
plot(Weekly_month,Weekly_RH)


forRussWeekly = data.frame(Weekly_month,Weekly_AirTC_Avg,Weekly_PAR_mmol_perDay,Weekly_PAR_mmol_perWeek,Weekly_Rain_mm_tot,Weekly_RH)
write.csv(forRussWeekly, file = "forRussWeekly.csv")

#install.packages("ggplot2")
library(ggplot2)


#you can find more examples for using ggplot2 at the following locations
# http://ggplot2.org/ 

#first DEFINE the plot you want to see *NOTE instead of using ENZdat$pH as x you define the dataframe FIRST and then the x and y axes
a <- ggplot(Weekly_met, aes(x=month, y=PAR_mmol_perWeek))

#make a plot with big red symbols
a + geom_point(colour = "red", size = 6)

#Show the site differences with different shapes and sizes
a + aes(shape = factor(SITE)) +
  geom_point(aes(colour = factor(SITE)), size = 4) +
  geom_point(colour="grey90", size = 1.5)



plot(met2014$PAR_Den_Avg)
plot(sumPAR)

