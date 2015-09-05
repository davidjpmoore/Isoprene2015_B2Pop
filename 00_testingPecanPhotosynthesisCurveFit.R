##Author: Dave Moore
##Date: 09/02/2015
##Purpose: to test PEcAn.photosynthesis as a method to analyze A/ci curves with T corrections and TPU limitation

#installing PEcAn.photosynthesis as a stand alone
if (!require("PEcAn.photosynthesis",character.only = TRUE))
{
  library(devtools)
  install_github("PecanProject/pecan/modules/photosynthesis") 
}

#note you need rjags installed in R and also JAGS (stand alone application) installed on your computer
#note you will also need Rtools installed.

#note you will get an error 

library(PEcAn.photosynthesis)

## Get list of LI-COR 6400 file names (ASCII not xls)

#this command looks for files *starting with* 'flux-course-' within the folder "extdata" - the files adhere to 
# a naming convention that let's the code pull out whether the file is an A/ci curve or an aq curve
#
filenames <- system.file("extdata", paste0("flux-course-",rep(1:6,each=2),c("aci","aq")), package = "PEcAn.photosynthesis")


D:\Dropbox\01.Projects\Disturbance.projectsCO\Bark Beetles\03.Data_and_notes\03.FEFNWTFLUXESsoil respiration master data

## Load files to a list
master = lapply(filenames, read.Licor)

# # The code below performs a set of interactive QA/QC checks on the LI-COR data that's been loaded. 
# Because it's interactive it isn't run when this vignette is knit.

# # If you want to get a feel for how the code works you'll 
# want to run it first on just one file, rather than looping over all the files

#this runs licor QC on a file - you click on outliers to remove them
master[[1]] <- Licor.QC(master[[1]])

#same process for all files
for(i in 1:length(master)){
  master[[i]] = Licor.QC(master[[i]])
}

#after the QC process combine the files into one data frame

dat<-do.call("rbind", master)
 

## if QC was done, remove both unchecked points and those that fail QC
if("QC" %in% colnames(dat)){
  dat = dat[-which(dat$QC < 1),]  
} else{
  QC = rep(1,nrow(dat))
  dat = cbind(dat,QC)
}
