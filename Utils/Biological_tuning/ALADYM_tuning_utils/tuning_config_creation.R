# This function creates the different config files that are needed to
# automatize simulations over different combinations of tr and F_range

# Input parameters:
#           - F_at_age: year x age matrix
#           - M: column vectors (as in BEMTOOL input)*
#           - F_range: age grouping (list)
#           - I/O paths
#           - species_folder
#           - tr: tr values (vector)
#           - spe: species
#           - cfg: initial configuration file**
#           - M_different: are M_male and M_female different? (T/F)

# Output parameters:
#           - M files starting from correct tr value


# *make sure that the M files start at age 0!
#**please insert NAN to values that need to be NA (eg Discard type)



library(dplyr)
library(stringr)
library(tidyr)

tuning_config <- function(input_path, output_path, species_folder, F_range, tr, spe, cfg_file, M_different, M_male=NULL, M_female=NULL, M_file=NULL){

if (M_different){
  
  if (is.null(M_male) || is.null(M_female)){
    message(sprintf("Check that M_male or M_female are not NULL"))
    return(NA)
  }
  
  # read M files
  Mm_vector <- read.table(paste(input_path, "\\", species_folder, "\\", M_male, sep=""), sep=";", header=TRUE)
  Mf_vector <- read.table(paste(input_path, "\\", species_folder, "\\", M_female, sep=""), sep=";", header=TRUE)

} else{
  
  if (is.null(M_file)){
    message(sprintf("Check that M_file is not NULL"))
    return(NA)
  }
  
  # read M file
  M_vector <- read.table(paste(input_path, "\\", species_folder, "\\", M_file, sep=""), sep=";", header=TRUE)

}

if ((F_range != "None")[1]){
  
# store sequence of F_range per tr (all combos defined in DF_runs)
data = c()

for (i in F_range){
  data2 = rep(paste(i[1], "-", i[2], sep=""),length(tr)) 
  
  data = c(data,data2)
  
}

# define all tr - F_range combos
DF_runs=data.frame(run=seq(length(tr)*length(F_range)),tr=rep(tr,length(F_range)),age_range=data)
                   
for (r in DF_runs$run){
  
  # read configuration
  cfg=read.table(paste(input_path, "\\", cfg_file, sep=""), sep=";", header=TRUE)
  cfg[is.na(cfg)] = ""
  cfg[cfg=='NAN'] = "NA"

  # change tr
  cfg[cfg$Parameters == "Tr",]$Distribution <- DF_runs[r,]$tr
  
  # change Z file path accordingly
  old_Zpath = str_split(cfg[cfg$Parameters == "Z file",]$Distribution, pattern='.csv')[[1]][1]
  new_Zpath = paste(old_Zpath, "_", DF_runs[r,]$tr, "_", DF_runs$age_range[r], ".csv", sep="")
 
  cfg[cfg$Parameters == "Z file",]$Distribution = new_Zpath
  
  # change M files path accordingly
  old_Mmpath = str_split(cfg[cfg$Parameters == "M file - Male",]$Distribution, pattern='.csv')[[1]][1]
  new_Mmpath = paste(old_Mmpath,DF_runs[r,]$tr,".csv", sep="")
    
  cfg[cfg$Parameters == "M file - Male",]$Distribution = new_Mmpath
  
    
  old_Mfpath = str_split(cfg[cfg$Parameters == "M file - Female",]$Distribution, pattern='.csv')[[1]][1]
  new_Mfpath = paste(old_Mfpath,DF_runs[r,]$tr,".csv", sep="")
    
  cfg[cfg$Parameters == "M file - Female",]$Distribution = new_Mfpath

  # save new config file
  write.table(cfg, file=paste(output_path, "\\config_run_", DF_runs[r,]$tr, "_", DF_runs[r,]$age_range, ".csv",sep=""), sep=";", row.names = FALSE)
  
}} else{
  # only use tr as Frange is None
  DF_runs=data.frame(run=seq(length(tr)),tr=tr)
  
  for (r in DF_runs$run){
    
    # read configuration
    cfg=read.table(paste(input_path, "\\", cfg_file, sep=""), sep=";", header=TRUE)
    cfg[is.na(cfg)] = ""
    cfg[cfg=='NAN'] = "NA"
    
    # change tr
    cfg[cfg$Parameters == "Tr",]$Distribution <- DF_runs[r,]$tr
    
    # change Z file path accordingly
    old_Zpath = str_split(cfg[cfg$Parameters == "Z file",]$Distribution, pattern='.csv')[[1]][1]
    new_Zpath = paste(old_Zpath, "_", DF_runs[r,]$tr, "_ass.csv", sep="")
    
    cfg[cfg$Parameters == "Z file",]$Distribution = new_Zpath
    
    ## change M files path accordingly
    
    # males
    old_Mmpath = str_split(cfg[cfg$Parameters == "M file - Male",]$Distribution, pattern='.csv')[[1]][1]
    new_Mmpath = paste(old_Mmpath,DF_runs[r,]$tr,".csv", sep="")
    
    cfg[cfg$Parameters == "M file - Male",]$Distribution = new_Mmpath
    
    # females
    old_Mfpath = str_split(cfg[cfg$Parameters == "M file - Female",]$Distribution, pattern='.csv')[[1]][1]
    new_Mfpath = paste(old_Mfpath,DF_runs[r,]$tr,".csv", sep="")
    
    cfg[cfg$Parameters == "M file - Female",]$Distribution = new_Mfpath
    
    # save new config file
    write.table(cfg, file=paste(output_path, "\\config_run_", DF_runs[r,]$tr, "_ass.csv",sep=""), sep=";", row.names = FALSE)
    
  }
  
}

}

