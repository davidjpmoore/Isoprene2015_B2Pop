#Date: 09/05/2015
#author: Dave Moore
#purpose: explore isoprene data from 2014 B2 Poplar Plantation

load ("dat_Iso_01.Rda")

#check unique dates and lines
unique(dat_Iso_01$date) 
unique(dat_Iso_01$line)

#filter single line to check it
filter(dat_Iso_01, line=="49-177")

#Assign data to new DF called Junk1
junk1=filter(dat_Iso_01, line=="49-177")

#Assign exactly the same data to a new DF called Junkx using dplyr 
Junkx <- dat_Iso_01 %>%
  filter(line=="49-177")  #NOTE filter(condition1, condition2..) for AND operators.

#check unique dates in junk1
unique(junk1$date)

#apply second filter to remove outlier Anet numbers
junk2=filter(junk1, date=="7/20/2014", Anet < 50)

#apply second filter to remove outlier Anet numbers | same thing using dplyr commands
Junky <- Junkx %>%
  filter(date=="7/20/2014", Anet < 50)  #NOTE filter(condition1, condition2..) for AND operators.

library(grid) #required for 'unit'
#Isoprene Plots
IsoCi <- ggplot(junk2, aes(x=Ci, y=Iso_nmol_p_umol_p_sec_p_m2))
#set up labels to keep track of what we are plotting
line_label <- junk2$line[1]
date_label <- junk2$date[1]

IsoCi + aes(shape = factor(Tref)) + #different symbols for different Tref's
  ggtitle(paste("Line",line_label,"Date ",date_label,sep=" "))+ #apply title 
  geom_point(aes(colour = factor(Tref)), size = 8) + #different colors for different Tref's
  theme_classic() + #apply classic theme (gets rid of grey background)
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))+ #provide negative value for tick length = they face inwards
  ylab("Isoprene (nmol/m2/sec)")+
  xlab("Ci") 


filter(junk1, date==dateloop[i], Anet < 50)


