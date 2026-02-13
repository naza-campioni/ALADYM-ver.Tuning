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
  
  split_gear <- function(df, name_gear){
    
    return(df[df$fleet_segment == name_gear, ])
    
  }
  
  df_by_gears <- list()
  
  for (names in unique(sel$fleet_segment)) {
    df_by_gears <- append(df_by_gears, list(split_gear(sel, names)))
  }
  
  
  
  # create combo per fleet
  create_combo <- function(df){
    
    sel_type             <- paste(df$sel_type[1])
    names                <- df$fleet_segment[1]
    param_names          <- selectivity_id[[sel_type]][[1]]
    # id                   <- selectivity_id[[sel_type]][[2]]
    params               <- params_sel[[sel_type]] 
    
    combo_grid           <- expand.grid(params)
    colnames(combo_grid) <- param_names
    
    
    return(list(combo_grid, names)) 
  }
  
  # 
  fleet_config <- list()
  fleet_config_name <- list()
  
  for (i in 1:length(df_by_gears)) {
    output            <- create_combo(df_by_gears[[i]])
    fleet_config      <- append(fleet_config, list(output[[1]]))
    fleet_config_name <- append(fleet_config_name, list(output[[2]]))
  }
  
  names(fleet_config) <- fleet_config_name
  
  
  # create unique  param names
  fleet_config <- Map(function(df, nm) {
    names(df) <- paste(nm, names(df), sep = "_")
    df
  }, fleet_config, names(fleet_config))
  
  
  # generate all global configurations
  full_configs <- Reduce(function(x, y) merge(x, y, by = NULL), fleet_config)
  
  
  build_selectivity_config <- function(template, config_row, gear_param_map) {
    
    df <- template
    
    # overwrite parameters for each gear
    for (gear in names(gear_param_map)) {
      
      cols <- gear_param_map[[gear]]             # columns in full_configs belonging to this gear
      params <- config_row[cols]                 # the actual values for this config
      
      # subset of template belonging to this gear
      rows <- df$fleet_segment == gear
      
      # find which param indexes are relevant depending on sel_type
      sel_type <- df$sel_type[rows][1]
      relevant_params <- selectivity_id[[as.character(sel_type)]][[2]]
      
      # assign relevant params
      for (i in seq_along(relevant_params)) {
        param_col <- paste0("param", relevant_params[i])
        df[rows, param_col] <- params[i]
      }
    }
    
    return(df)
  }
  
  gear_param_map <- lapply(names(fleet_config), function(g) {
    grep(paste0("^", g, "_"), names(full_configs), value = TRUE)
  })
  names(gear_param_map) <- names(fleet_config)
  
  template <- sel   # your original full selectivity table
  
  dir_to_create <- "selectivity_combinations"
  
  if (!dir.exists(paste(folder, "/", spe_folder, "/", dir_to_create, sep = ''))) {
    dir.create(paste(folder, "/", spe_folder, "/", dir_to_create, sep = ''), recursive = TRUE)
  }
  
  
  for (i in 1:nrow(full_configs)) {
    
    cfg_row <- full_configs[i, ]
    
    df_cfg <- build_selectivity_config(
      template = template,
      config_row = cfg_row,
      gear_param_map = gear_param_map
    )
    
    outfile <-  sprintf("selectivity_cfg_%04d.csv", i)
    path <- paste(folder, "/", spe_folder, "/selectivity_combinations/", outfile, sep='')
    write.csv2(df_cfg, path, row.names = FALSE)
  }
  
  
  
} 

