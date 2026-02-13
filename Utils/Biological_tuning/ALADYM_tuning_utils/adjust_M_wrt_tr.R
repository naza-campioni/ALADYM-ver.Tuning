# This function returns M files that start from the correct tr value. File names
# are also updated accordingly.

# Input parameters:
#           - F_at_age: year x age matrix
#           - M: column vectors (as in BEMTOOL input)*
#           - F_range: age grouping (list)
#           - I/O paths
#           - species_folder
#           - tr: tr values (vector)
#           - M_different: are M_male and M_female different? (T/F)

# Output parameters:
#           - M files starting from correct tr value


# *make sure that the M files start at age 0!




library(stringr)


adjust_M_wrt_tr <- function(input_path, output_path, species_folder, tr, M_different, M_male=NULL, M_female=NULL, M_file=NULL){

if (M_different){
  
  if (is.null(M_male) || is.null(M_female)){
    message(sprintf("Check that M_male or M_female are not NULL"))
    return(NA)
  }
  
  # read M files
  Mm_vector <- read.table(paste(input_path, "\\", species_folder, "\\", M_male, sep=""), sep=";", header=TRUE)
  Mf_vector <- read.table(paste(input_path, "\\", species_folder, "\\", M_female, sep=""), sep=";", header=TRUE)
  
  for (i in tr){
    
    # change file names
    old_Mpath = str_split(M_male, pattern='.csv')[[1]][1]
    old_Fpath = str_split(M_female, pattern='.csv')[[1]][1]
    
    new_Mpath = paste(output_path, "\\", species_folder, "\\", old_Mpath, i, ".csv", sep="")
    new_Fpath = paste(output_path, "\\", species_folder, "\\", old_Fpath, i, ".csv", sep="")
    
    # save files with new names and correct starting and ending points
    if (i == 0){
      write.table(Mm_vector, file=new_Mpath, sep=";", row.names=FALSE)
      write.table(Mf_vector, file=new_Fpath, sep=";", row.names=FALSE)
    } else{
      write.table(Mm_vector[-c(1:i),], file=new_Mpath, sep=";", row.names=FALSE)
      write.table(Mf_vector[-c(1:i),], file=new_Fpath, sep=";", row.names=FALSE)
    }
    
    
  }
  
} else{
  
  if (is.null(M_file)){
    message(sprintf("Check that M_file is not NULL"))
    return(NA)
  }
  
  # read M file
  M_vector <- read.table(paste(input_path, "\\", M_file, sep=""), sep=";", header=TRUE)
  
  for (i in tr){
    # change file name
    old_path = str_split(M_file, pattern='.csv')[[1]][1]
    
    new_path = paste(output_path, "\\", species_folder, "\\", old_path, i, ".csv", sep="")
    
    # save new file with correct starting point
    write.table(M_vector[i+1:length(M_vector[,2]),], file=new_path, sep=";", row.names=FALSE)
  }
  
}
  
}


