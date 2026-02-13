library(stringr)

# folder <- "C:/INPUT/ALADYM"
# 
# 
# spe_folder <- "DPS17181920"
# spe <- 'DPS'
# 
# configuration <- "aladym_DPS_GSA17_18_19_20.csv"


create_configs <- function(folder, spe_folder, spe, configuration){
  dir_to_create <- paste(folder, "/selectivity_configs_", spe, sep = '')
  
  if (!dir.exists(dir_to_create)) {
    dir.create(dir_to_create, recursive = TRUE)
  }
  
  list_files <- list.files(paste(folder, "/", spe_folder, "/selectivity_combinations", sep=""))
  
  cfg = read.table(paste(folder, "/", configuration, sep = ''), sep = ';', header = TRUE)
  cfg[is.na(cfg)] = ''
  cfg[cfg=='NAN'] = "NA"
  
  old_path <- str_split(cfg[cfg$Parameters == "Selectivity file",]$Distribution,
                        pattern = 'Selectivity')[[1]][1]
  
  for (len in 1:length(list_files)) {
    file_path <- list_files[len]
    new_path <- paste(old_path, "selectivity_combinations\\", file_path, sep = '')
    
    cfg[cfg$Parameters == 'Selectivity file',]$Distribution <- new_path
    
    write.table(cfg,
                file = paste(dir_to_create, "/config_selectivity_", len, ".csv", sep = ''),
                sep = ';',
                row.names = FALSE)
  }
}

