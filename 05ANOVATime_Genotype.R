#Author: Dave Moore
#date 10/12/2015
#purpose: Estimate time & genotype effects on photosyntyhesis and isoprene

#loading dat_Iso1.Rda - isoprene unit conversions completed using 00_ReadIsoprene_UnitConversions
load ("dat_Iso_01.Rda")


# create R date from date

Fig2Analysis01 = dat_Iso_01 %>%
  mutate(Genotype=line, Tleaf = Tref, MeasDate=date) %>%
  mutate(rDate = as.Date(MeasDate, "%m/%d/%Y")) %>%
  mutate(month=format(rDate, "%m")) %>%
  group_by(Genotype, Tleaf, MeasDate) %>%
  select(-date,-line, -Tref)

# Extract measurements at 400 ppm 

Fig2Analysis02 = Fig2Analysis01  %>%
  filter(CO2 == 400)
  
#Plot photosynthesis
  plot (Fig2Analysis02$rDate, Fig2Analysis02$Anet)
  #Plot isoprene  
  plot (Fig2Analysis02$rDate, Fig2Analysis02$Iso_nmol_p_umol_p_sec_p_m2)

  
#carry out ANOVA with genotype and month as factors for 25 degress and 35 Degrees

  Fig2AnalysisANOVA <- aov(Anet ~ Genotype  + ## Diet effect 
                             month +
                       #Error(Subject/Diet) + ## nesting of Subject in Diet 
                         month*Genotype, ## interaction of Time and Diet 
                         data = Fig2Analysis02)

#carry out ANOVA with genotype, temperature and month as factors