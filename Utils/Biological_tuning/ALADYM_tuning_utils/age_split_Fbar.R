# This function produces monthly total fishing mortality Z for males and females
# for different age aggregations of F specified in F_range.

# Input parameters:
#           - F_at_age_M: year x age matrix
#           - F_at_age_F: year x age matrix
#           - M: column vectors (as in BEMTOOL input)*
#           - F_range: age grouping (list)
#           - I/O paths
#           - species_folder
#           - tr: tr values (vector)
#           - ts: time series (vector)
#           - spe: species
#           - M_different: are M_male and M_female different? (T/F)
#           - Fbar_assessment: available if F_at_age is not

# Output parameters:
#           - total fishing mortality matrices Z
#             calculated for F_range

# *make sure that the M files start at age 0!

# This code assumes different F_at_age per sex but one single Fbar file


age_split_Fbar <- function(input_path, output_path, species_folder, F_range, tr, ts, spe, Fbar_assessment = "None", F_at_age_M, F_at_age_F, M_different, M_male=NULL, M_female=NULL, M_file=NULL){

  # F_at_age matrix
  if ((F_at_age_M != "None") & (F_at_age_F != "None")){
  F_file_M <- read.table(paste(input_path, "\\", species_folder, "\\", F_at_age_M, sep=""), sep=";", header=T, check.names = FALSE)
  Fish_mort_M <- F_file_M[F_file_M[,1] %in% ts,]
  
  F_file_F <- read.table(paste(input_path, "\\", species_folder, "\\", F_at_age_F, sep=""), sep=";", header=T, check.names = FALSE)
  Fish_mort_F <- F_file_F[F_file_F[,1] %in% ts,]
  }
  
  
  
  # Fbar file
  if (Fbar_assessment != "None"){
    Fbar_ass = read.table(paste(input_path, "\\", species_folder, "\\", Fbar_assessment, sep=""), sep=";", header=T, check.names = FALSE)
  }
  
  
  
  # M_file as in BEMTOOL input
  if (M_different){
    
    if (is.null(M_male) || is.null(M_female) || is.null(F_at_age_M) || is.null(F_at_age_F)){
      message(sprintf("Check that M_male, M_female or F_at_age are not NULL"))
      return(NA)
    }
    
    Mm_vector=read.table(paste(input_path, "\\", species_folder, "\\", M_male, sep=""), sep=";", header=T)
    Mf_vector=read.table(paste(input_path, "\\", species_folder, "\\", M_female, sep=""), sep=";", header=T)
    
    # if F_range is a list then the if checks each element in the list
    if ((F_range != "None")[1]){
    # loop over different age-grouping combos
    for (age_split in F_range){
      
      for (tr_ in tr){
      
      # select correct starting and end points
      if (tr_[1] == 0){
        mean_Mm=mean(Mm_vector[,2])
        mean_Mf=mean(Mf_vector[,2])
        
      } else{
        mean_Mm=mean(Mm_vector[-c(1:tr_),2])
        mean_Mf=mean(Mf_vector[-c(1:tr_),2])
      }
      
        
      F_bar_M = data.frame(year = ts, Fbar = rowMeans(Fish_mort_M[, c((age_split[1]+2):(age_split[2]+2))]))
      F_bar_F = data.frame(year = ts, Fbar = rowMeans(Fish_mort_F[, c((age_split[1]+2):(age_split[2]+2))]))
      
      Zm_bar = F_bar_M
      Zf_bar = F_bar_F
      
      Zm_bar[,2] = Zm_bar[,2] + mean_Mm
      Zf_bar[,2] = Zf_bar[,2] + mean_Mf
      
      DF_Zm = data.frame(year=ts,	seed= c(Zm_bar[1,2],rep("",(length(ts)-1))),	Jan=Zm_bar[,2],	Feb=Zm_bar[,2],	Mar=Zm_bar[,2],	Apr=Zm_bar[,2],	May=Zm_bar[,2],	Jun=Zm_bar[,2],	Jul=Zm_bar[,2],	Aug=Zm_bar[,2],	Sep=Zm_bar[,2],	Oct=Zm_bar[,2],	Nov=Zm_bar[,2],	Dec=Zm_bar[,2],sex="M")
      DF_Zf = data.frame(year=ts,	seed= c(Zf_bar[1,2],rep("",(length(ts)-1))),	Jan=Zf_bar[,2],	Feb=Zf_bar[,2],	Mar=Zf_bar[,2],	Apr=Zf_bar[,2],	May=Zf_bar[,2],	Jun=Zf_bar[,2],	Jul=Zf_bar[,2],	Aug=Zf_bar[,2],	Sep=Zf_bar[,2],	Oct=Zf_bar[,2],	Nov=Zf_bar[,2],	Dec=Zf_bar[,2],sex="F")
      
      DF_Z=rbind(DF_Zm,DF_Zf)
      
      write.table(DF_Z,paste(output_path, "\\", species_folder, "\\Zmean_", spe, "_", tr_, "_", age_split[1],"-",age_split[2], ".csv",sep=""), sep=";",row.names=F)
      
    }}} else{
      # check user has inserted Fbar assessment file
      if (Fbar_assessment =="None"){
        message((sprintf("Insert valid Fbar assessment file")))
        return(NA)
      }
      
      Zm_bar = Fbar_ass
      Zf_bar = Fbar_ass
      
      for (tr_ in tr){
        # loop over tr
        if (tr_[1] == 0){
          mean_Mm=mean(Mm_vector[,2])
          mean_Mf=mean(Mf_vector[,2])
          
        } else{
          mean_Mm=mean(Mm_vector[-c(1:tr_),2])
          mean_Mf=mean(Mf_vector[-c(1:tr_),2])
        }
      
      Zm_bar[,2] = Zm_bar[,2] + mean_Mm
      Zf_bar[,2] = Zf_bar[,2] + mean_Mf
      
      DF_Zm = data.frame(year=ts,	seed= c(Zm_bar[1,2],rep("",(length(ts)-1))),	Jan=Zm_bar[,2],	Feb=Zm_bar[,2],	Mar=Zm_bar[,2],	Apr=Zm_bar[,2],	May=Zm_bar[,2],	Jun=Zm_bar[,2],	Jul=Zm_bar[,2],	Aug=Zm_bar[,2],	Sep=Zm_bar[,2],	Oct=Zm_bar[,2],	Nov=Zm_bar[,2],	Dec=Zm_bar[,2],sex="M")
      DF_Zf = data.frame(year=ts,	seed= c(Zf_bar[1,2],rep("",(length(ts)-1))),	Jan=Zf_bar[,2],	Feb=Zf_bar[,2],	Mar=Zf_bar[,2],	Apr=Zf_bar[,2],	May=Zf_bar[,2],	Jun=Zf_bar[,2],	Jul=Zf_bar[,2],	Aug=Zf_bar[,2],	Sep=Zf_bar[,2],	Oct=Zf_bar[,2],	Nov=Zf_bar[,2],	Dec=Zf_bar[,2],sex="F")
      
      DF_Z=rbind(DF_Zm,DF_Zf)
      
      write.table(DF_Z,paste(output_path, "\\", species_folder, "\\Zmean_", spe, "_", tr_, "_ass.csv", sep=""), sep=";", row.names=F)
      
      }
    }
  
    
    
    # if M_different == False
    } else{
    M_vector=read.table(paste(input_path, "\\", M_file, sep=""), sep=";", header=T)
    
    if ((F_range != "None")[1]){
    
    # loop over different age-grouping combos
    for (age_split in F_range){
      for (tr_ in tr){
      
      if (tr_ == 0){
        mean_M=mean(M_vector[,2])
        
      } else{
        mean_M=mean(M_vector[-c(1:tr_),2])
      }
        F_bar_M = data.frame(year = ts, Fbar = rowMeans(Fish_mort_M[, c((age_split[1]+2):(age_split[2]+2))]))
        F_bar_F = data.frame(year = ts, Fbar = rowMeans(Fish_mort_F[, c((age_split[1]+2):(age_split[2]+2))]))
       
        Zm_bar = F_bar_M
        Zf_bar = F_bar_F
        
        Zm_bar[,2] = Zm_bar[,2] + mean_M
        Zf_bar[,2] = Zf_bar[,2] + mean_M
        
        DF_Zm = data.frame(year=ts,	seed= c(Zm_bar[1,2],rep("",(length(ts)-1))),	Jan=Zm_bar[,2],	Feb=Zm_bar[,2],	Mar=Zm_bar[,2],	Apr=Zm_bar[,2],	May=Zm_bar[,2],	Jun=Zm_bar[,2],	Jul=Zm_bar[,2],	Aug=Zm_bar[,2],	Sep=Zm_bar[,2],	Oct=Zm_bar[,2],	Nov=Zm_bar[,2],	Dec=Zm_bar[,2],sex="M")
        DF_Zf = data.frame(year=ts,	seed= c(Zf_bar[1,2],rep("",(length(ts)-1))),	Jan=Zf_bar[,2],	Feb=Zf_bar[,2],	Mar=Zf_bar[,2],	Apr=Zf_bar[,2],	May=Zf_bar[,2],	Jun=Zf_bar[,2],	Jul=Zf_bar[,2],	Aug=Zf_bar[,2],	Sep=Zf_bar[,2],	Oct=Zf_bar[,2],	Nov=Zf_bar[,2],	Dec=Zf_bar[,2],sex="F")
        
        DF_Z=rbind(DF_Zm,DF_Zf)
        
        write.table(DF_Z,paste(output_path, "\\", species_folder, "\\Zmean_", spe, "_", tr_, "_", age_split[1],"-",age_split[2], ".csv",sep=""), sep=";",row.names=F)
      
    }}} else {
      if (Fbar_assessment =="None"){
        message((sprintf("Insert valid Fbar assessment file")))
        return(NA)
      }
      
      Z_bar = F_bar_ass
      
      for (tr_ in tr){
        
        if (tr_ == 0){
          mean_M=mean(M_vector[,2])
          
        } else{
          mean_M=mean(M_vector[-c(1:tr_),2])
        }
      
      Z_bar[,2] = Z_bar[,2] + mean_M
      
      DF_Z=data.frame(year=ts,	seed= c(Z_bar[1,2],rep("",(length(ts)-1))),	Jan=Z_bar[,2],	Feb=Z_bar[,2],	Mar=Z_bar[,2],	Apr=Z_bar[,2],	May=Z_bar[,2],	Jun=Z_bar[,2],	Jul=Z_bar[,2],	Aug=Z_bar[,2],	Sep=Z_bar[,2],	Oct=Z_bar[,2],	Nov=Z_bar[,2],	Dec=Z_bar[,2],sex="M")
      DF_Z2=DF_Z
      DF_Z2$sex="F"
      DF_Z=rbind(DF_Z,DF_Z2)
      
      write.table(DF_Z,paste(output_path, "\\", species_folder, "\\Zmean_", spe, "_", tr_, "_ass.csv", sep=""), sep=";", row.names=F)                  
      
      
    }}} 


}



