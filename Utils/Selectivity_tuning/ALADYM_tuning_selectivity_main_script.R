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
selectivity_id[["1"]] <- list(c("param1", "param2"), c(1, 2))
selectivity_id[["2"]] <- list(c("param1", "param2", "param3"), c(1, 2, 3))
selectivity_id[["3"]] <- list(c("param1", "param2"), c(1, 2))
selectivity_id[["4"]] <- list(c("param1", "param2"), c(1, 2))
selectivity_id[["5"]] <- list(c("param1", "param2", "param3", "param4", "param5"),
                              c(1, 2, 3, 4, 5))
selectivity_id[["6"]] <- list(c("param1", "param2", "param3"), c(1, 2, 3))

# -----------------------------------------------------
# Define selectivity parameters combinations per selectivity type
# -----------------------------------------------------
# sel 1
p11 <- c(10, 15, 20) #seq(15, 20)
p12 <- c(5, 6) #seq(3, 7)

# sel 2
p21 <- c(10, 15)
p22 <- c(7, 8)
p23 <- c(8, 10)

# sel 3
p31 <- c(20, 25)
p32 <- c(10, 15)

# sel 4
p41 <- c(12, 25)
p42 <- c(6, 7)
p43 <- c(7, 8)

# sel 5
p51 <- c(12, 25)
p52 <- c(12, 25)
p53 <- c(12, 25)
p54 <- c(12, 25)
p55 <- c(12, 25)

# sel 6
p61 <- c(12, 25)
p62 <- c(10, 20)
p63 <- c(15, 15)


# per-selectivity-type parameters
params_sel <- hash()
params_sel[["1"]] <- list(p11, p12)
params_sel[['2']] <- list(p21, p22, p23)
params_sel[['3']] <- list(p31, p32)
params_sel[['4']] <- list(p41, p42, p43)
params_sel[['5']] <- list(p51, p52, p53, p54, p55)
params_sel[['6']] <- list(p61, p62, p63)


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
sel_file <- "Selectivity_HKE_SEL.csv"

# original configuration file
configuration <- "aladym_HKE_GSA17_18_SEL.csv"


# -----------------------------------------------------
# Step 1: create all selectivity combinations
# -----------------------------------------------------
create_combinations(folder, params_sel, selectivity_id, spe_folder, sel_file)


# -----------------------------------------------------
# Step 2: create matching configuration files
# -----------------------------------------------------
create_configs(folder, spe_folder, spe, configuration)

