#Date: 09/05/2015
#author: Dave Moore
#purpose: explore isoprene data from 2014 B2 Poplar Plantation

load ("dat_Iso_01.Rda")




#plot
IsoByCi <- ggplot(dat_Iso_01, aes(x=Ci, y=Iso_nmol_p_umol_p_sec_p_m2))
IsoByCi + aes(shape = factor(Tref)) +
  geom_point(aes(colour = factor(Tref)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Isoprene Emisssion Rate (nmol/umol air/sec/m2)")+
  xlab("Ci") +
  geom_smooth(method=lm,se=FALSE)




IsoByCi_A <- ggplot(B2Physiologymaster, aes(x=Ci, y=Iso_mol_pm2_psec))
IsoByCi_A + aes(shape = factor(Tref)) +
  geom_point(aes(colour = factor(Tref)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Amberly's Isoprene Emisssion Rate (nmol/umol air/sec/m2)")+
  xlab("Ci") +
  geom_smooth(method=lm,se=FALSE)
