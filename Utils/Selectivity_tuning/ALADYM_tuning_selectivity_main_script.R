# =====================================================
# Selectivity tuning pipeline
#
# This script:
# 1) Generates all selectivity parameter combinations across fleets,
#    based on user-defined ranges
# 2) Creates one ALADYM configuration file per selectivity combination
#
# IMPORTANT:
# The original ALADYM configuration file MUST use
# the literal string "NAN" (not NA) for missing values.
# These are automatically converted during processing.
# =====================================================

source("C:\\Users\\Nazareno Campioni\\Desktop\\ALADYM_tuning_selectivity_utils\\create_sel_combinations.R")
source("C:\\Users\\Nazareno Campioni\\Desktop\\ALADYM_tuning_selectivity_utils\\create_sel_configs.R")

library(hash)

# -----------------------------------------------------
# Define selectivity dictionary:
# maps selectivity type -> parameters used
# -----------------------------------------------------
selectivity_id <- hash()
params_sel <- hash()
# selectivity_id[["1"]] <- list(c("param1", "param2"), c(1, 2))
# selectivity_id[["2"]] <- list(c("param1", "param2", "param3"), c(1, 2, 3))
# selectivity_id[["3"]] <- list(c("param1", "param2"), c(1, 2))
# selectivity_id[["4"]] <- list(c("param1", "param2"), c(1, 2))
# selectivity_id[["5"]] <- list(c("param1", "param2", "param3", "param4", "param5"),
#                               c(1, 2, 3, 4, 5))
# selectivity_id[["6"]] <- list(c("param1", "param2", "param3"), c(1, 2, 3))

# this will need to be set according to each species
# eg DTS for HKE has selectivity 2 (3 parameters) whereas DTS for DPS has selectivity
# 1 (2 parameters)

#-----------------------------------
# HKE
#-----------------------------------
selectivity_id[["DTS"]] <- list(c("param1", "param2", "param3"), c(1, 2, 3))
selectivity_id[['DFN']] <- list(c("param1", "param2", "param3"), c(1, 2, 3))
selectivity_id[['HOK']] <- list(c("param1", "param2", "param3"), c(1, 2, 3))
selectivity_id[['PGP']] <- list(c("param1", "param2", "param3"), c(1, 2, 3))

# dts
p_dts_1 <- c(140, 160, 180)
p_dts_2 <- c(10, 30, 50)
p_dts_3 <- c(430, 450, 470)

# dfn
p_dfn_1 <- c(360, 380, 400)
p_dfn_2 <- c(120, 140, 160)
p_dfn_3 <- c(120, 140, 160)

# hok
p_hok_1 <- c(430, 450, 470)
p_hok_2 <- c(80, 100, 120)
p_hok_3 <- c(100, 120, 140)

# pgp
p_pgp_1 <- c(360, 380, 400)
p_pgp_2 <- c(120, 140, 160)
p_pgp_3 <- c(120, 140, 160)

# per-selectivity-type parameters
params_sel[['DTS']] <- list(p_dts_1, p_dts_2, p_dts_3)
params_sel[['DFN']] <- list(p_dfn_1, p_dfn_2, p_dfn_3)
params_sel[['HOK']] <- list(p_hok_1, p_hok_2, p_hok_3)
params_sel[['PGP']] <- list(p_pgp_1, p_pgp_2, p_pgp_3)


#----------------------------
# DPS
#----------------------------
# selectivity_id[["DTS"]] <- list(c("param1", "param2"), c(1, 2))
# 
# # dts
# p_dts_1 <- c(15, 17, 20)
# p_dts_2 <- c(3, 5, 7)
# 
# # per-selectivity-type parameters
# params_sel[['DTS']] <- list(p_dts_1, p_dts_2)

#---------------------------
# NEP
#---------------------------
# selectivity_id[['DTS']] <- list(c("param1", "param2"), c(1, 2))
# selectivity_id[['DFN']] <- list(c("param1", "param2"), c(1, 2))
# 
# # dts
# p_dts_1 <- c(17, 20, 23)
# p_dts_2 <- c(4, 7, 10)
# 
# # dfn
# p_dfn_1 <- c(17, 20, 23)
# p_dfn_2 <- c(4, 6.9, 9)
# 
# # per-selectivity-type parameters
# params_sel[['DTS']] <- list(p_dts_1, p_dts_2)
# params_sel[['DFN']] <- list(p_dfn_1, p_dfn_2)


#-------------------------
# MUT
#-------------------------
# selectivity_id[['DTS']] <- list(c("param1", "param2"), c(1, 2))
# selectivity_id[['DFN']] <- list(c("param1", "param2", "param3"), c(1, 2, 3))
# selectivity_id[['PGP']] <- list(c("param1", "param2"), c(1, 2))
# 
# # dts
# p_dts_1 <- c(80, 100, 120)
# p_dts_2 <- c(8, 10, 12)
# 
# # dfn
# p_dfn_1 <- c(150, 170, 190)
# p_dfn_2 <- c(15, 25, 35)
# p_dfn_3 <- c(20, 40, 60)
# 
# # pgp
# p_pgp_1 <- c(80, 100, 120)
# p_pgp_2 <- c(8, 10, 12)
# 
# # per-selectivity-type parameters
# params_sel[['DTS']] <- list(p_dts_1, p_dts_2)
# params_sel[['DFN']] <- list(p_dfn_1, p_dfn_2, p_dfn_3)
# params_sel[['PGP']] <- list(p_pgp_1, p_pgp_2)



# -----------------------------------------------------
# User inputs:
# - folders
# - species identifiers
# - selectivity parameters to explore
# - base configuration file
# -----------------------------------------------------

# input folder
folder <- "C:/INPUT/ALADYM"

# species details
spe <- 'HKE'
spe_folder <- 'HKE1718'

# original selectivity file
sel_file <- "Selectivity_HKE.csv"

# original configuration file
configuration <- "aladym_HKE_GSA17_18.csv"


# -----------------------------------------------------
# Step 1: create all selectivity combinations
# -----------------------------------------------------
create_combinations(folder, params_sel, selectivity_id, spe_folder, sel_file)


# -----------------------------------------------------
# Step 2: create matching configuration files
# -----------------------------------------------------
create_configs(folder, spe_folder, spe, configuration)

