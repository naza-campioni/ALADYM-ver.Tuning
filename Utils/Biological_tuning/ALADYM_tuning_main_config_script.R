# This script creates the following files:
#
#          - M files that start from the correct tr value
#          - Z files that take into account different age-grouping criterion
#            and current tr values 
#          - config_files that call the correct M and Z files
#
# Make sure to change the source paths and that:
#
#          1) the original config file lives in C:\INPUT\ALADYM
#          2) M and F files live in C:\INPUT\ALADYM\species_folder
#
# If the file F at age is not available, set, F_at_age_F and F_at_age_M to "None"
# and provide the Fbar_assessment file. In this case, all Zs will be
# calculated using this file.



source("C:\\Users\\Nazareno Campioni\\Desktop\\ALADYM_tuning_utils\\adjust_M_wrt_tr.R")
source("C:\\Users\\Nazareno Campioni\\Desktop\\ALADYM_tuning_utils\\age_split_Fbar.R")
source("C:\\Users\\Nazareno Campioni\\Desktop\\ALADYM_tuning_utils\\tuning_config_creation.R")

#####################################################
# Before beginning, set the following variables

## paths
# input path - main folder where we can find M, F and config_file
ip <- "C:\\INPUT\\ALADYM"

# output path - where you want to save the output (usually C:\INPUT\ALADYM)
op <- "C:\\INPUT\\ALADYM"

# species folder - used for M and F files
species_folder <- "DPS17181920"


# are there different M files for males and females?
M_different <- TRUE


## set the M files names -> make sure these start from age = 0 
# sex-specific files?
M_male <- "M_male_DPS.csv"
M_female <- "M_female_DPS.csv"

# same file?
M_file <- "M_HKE.csv"


## set the F_at_age file name
F_at_age_M <- "None" # "F_at_age_HKE_M.csv"
F_at_age_F <- "None" # "F_at_age_HKE_F.csv"

## set Fbar file name
# Fbar_assessment <- "None"
Fbar_assessment <- "Fbar_assessment_DPS.csv"

## set initial configuration file -> must live in C:\INPUT\ALADYM
## make sure that NA values are specified as NAN (needed for the splitting)
configuration <- "aladym_DPS_GSA17_18_19_20.csv"

## set tr
tr = c(0, 1, 2, 3)

## set F_range over which to calculate Fbar, eg: list(c(0,3),c(1,4))
# if you only have Fbar from assessment, set F_range = None
F_range = "None"

# F_range = list(c(0,1),c(2,3),c(4,5))



## time series
ts = c(2004:2024)

## species
spe = 'DPS'
#####################################################




#####################################################
##### create M files so that they start from the right tr value 


adjust_M_wrt_tr(input_path = ip, output_path = op, species_folder = species_folder,
                 tr = tr, M_different = M_different, M_male = M_male, M_female = M_female)


####################################################
##### create Z files calculated over different age ranges

age_split_Fbar(input_path = ip, output_path = op, species_folder = species_folder,
               F_range = F_range, tr = tr, ts = ts, spe = spe, Fbar_assessment = Fbar_assessment,
               F_at_age_M = F_at_age_M, F_at_age_F = F_at_age_F, M_different = M_different, M_male = M_male,
               M_female = M_female)

###################################################
##### create appropriate config files

tuning_config(input_path = ip, output_path = op, species_folder = species_folder,
              F_range = F_range, tr = tr, spe = spe, cfg_file = configuration,
              M_different = M_different, M_male = M_male, M_female = M_female)

