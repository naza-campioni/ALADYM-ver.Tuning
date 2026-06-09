library(hash)

# spe <- 'DPS'
# spe_folder <- 'DPS17181920'
# 
# sel_file <- "Selectivity_DPS17181920_SEL.csv"
# 
# folder <- "C:/INPUT/ALADYM"
# 
# # selectivity parameters
# # sel 1
# p11 <- c(10, 15, 20) #seq(15, 20)
# p12 <- c(5, 6) #seq(3, 7)
# 
# # sel 4
# p41 <- c(12, 25)
# p42 <- c(6, 7)
# p43 <- c(7, 8)
# 
# # per-selectivity-type parameters
# params_sel <- hash()
# params_sel[["1"]] <- list(p11, p12)
# params_sel[['4']] <- list(p41, p42, p43)
# 
# # selectivity map
# selectivity_id <- hash()
# selectivity_id[["1"]] <- list(c("param1", "param2")) #, c(1, 2))
# selectivity_id[["4"]] <- list(c("param1", "param2", "param3")) #, c(1, 2, 3))

create_combinations <- function(folder, params_sel, selectivity_id, spe_folder, sel_file){
  
 
  sel <- read.csv(paste(folder, "/", spe_folder, '/', sel_file, sep = ''),
                  sep = ';',
                  header = TRUE)
  sel[is.na(sel)] = ""
  
  
  dir_to_create <- "selectivity_combinations"
  
  if (!dir.exists(paste(folder, "/", spe_folder, "/", dir_to_create, sep = ''))) {
    dir.create(paste(folder, "/", spe_folder, "/", dir_to_create, sep = ''), recursive = TRUE)
  }
  
  
  fleet_config <- list()
  
  for (type in unique(sel$type)) {
    
    params <- params_sel[[type]]
    
    combo_grid <- expand.grid(params)
    
    colnames(combo_grid) <- selectivity_id[[type]][[1]]
    
    fleet_config[[type]] <- combo_grid
  }
  
  
  # -----------------------------
  # CREATE FILES
  # -----------------------------
  
  cfg_id <- 1
  
  for (type in names(fleet_config)) {
    
    configs <- fleet_config[[type]]
    
    for (i in 1:nrow(configs)) {
      
      # start from original file
      df_cfg <- sel
      
      # rows belonging to current type
      idx <- df_cfg$type == type
      
      # apply parameter values
      for (param_name in colnames(configs)) {
        
        df_cfg[idx, param_name] <- configs[i, param_name]
      }
      
      # output filename
      outfile <- sprintf(
        "selectivity_cfg_%04d.csv",
        cfg_id
      )
      
      path <- paste(
        folder,
        "/",
        spe_folder,
        "/selectivity_combinations/",
        outfile,
        sep = ""
      )
      
      # save
      write.table(
        df_cfg,
        path,
        sep = ';',
        row.names = FALSE,
        quote = FALSE
      )
      
      cfg_id <- cfg_id + 1
    }
  }
  
} 

