library(stringr)
library(ggplot2)
cat('Running script:', 'ALADYM_tuning.r', '\n')
# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/ALADYM.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.

AAA_VERSION_NAME <<- "ALADYM version Tuning"
setwd("C:\\Users\\Nazareno Campioni\\ALADYM-ver.Tuning")

# setwd(paste("C:\\AAA_BEMTOOL_ALADYM\\",AAA_VERSION_NAME, sep=""))

if (!exists("phase")) {
rm(list=ls(all=TRUE))
IN_BEMTOOL <<- FALSE
phase <<- ""
SKIP_spe <<- FALSE
}

AAA_VERSION_NAME <<- "ALADYM version Tuning"

# environment variable for tuning
CAL <- new.env()
CAL$combo_tag <- NULL       # eg 0_0-1
CAL$score <- list()
CAL$errors <- data.frame(   # individual errors
  run = character(),
  SSB = numeric(),
  Fbar = numeric(),
  catch = numeric(),
  total = numeric(),
  stringsAsFactors = FALSE
)
# list of config files
list_config <- list.files("C:\\INPUT\\ALADYM", pattern="config")

# list of assessment files
list_assessment <- list.files(getwd(), pattern="assessment")

# load data - they will be used inside the extract function
# and in the final plots
SSB_file <- list_assessment[grepl("ssb", tolower(list_assessment))]
Fbar_file <- list_assessment[grepl("fbar", tolower(list_assessment))]
catch_file <- list_assessment[grepl("landings", tolower(list_assessment))]


SSB_data <- read.csv(paste(getwd(), "/", SSB_file, sep=""), sep=";", header=TRUE)
Fbar_data <- read.csv(paste(getwd(), "/", Fbar_file, sep=""), sep=";", header=TRUE)
catch_data <- read.csv(paste(getwd(), "/", catch_file, sep=""), sep=";", header=TRUE)

# extraction function of Fbar, SSB, total catch
extract_results <- function(pd, index){
  sim_catch <- pd[3:(length(years)+2),length(FLEETSEGMENTS_names) + 2]
  obs_catch <- pd[3:(length(years)+2), (2*length(FLEETSEGMENTS_names) + 3)]
  
  annual_F <- pd[(length(years) + 5):(2*length(years) + 4),(3*length(FLEETSEGMENTS_names) + 5)]
  annual_SSB <- pd[(length(years) + 5):(2*length(years) + 4),(3*length(FLEETSEGMENTS_names) + 6)]
  monthly_SSB <- data.frame(pd[(length(years) + 5):(2*length(years) + 4),(3*length(FLEETSEGMENTS_names) + 7):(3*length(FLEETSEGMENTS_names) + 7 + 11)])
  
  months <- pd[(length(years) + 4),(3*length(FLEETSEGMENTS_names) + 7):(3*length(FLEETSEGMENTS_names) + 7 + 11)]
  
  total_catch_sim <- data.frame(Years=years, Total=sim_catch)
  total_catch_obs <- data.frame(Years=years, Total=obs_catch)
  
  F_annual <- data.frame(Years=years, F=annual_F)
  SSB_annual <- data.frame(Years=years, SSB=annual_SSB)
  colnames(monthly_SSB) <- months
  
  F_assessment <- Fbar_data # read.csv(paste(getwd(), "/Fbar_assessment.csv", sep=""), sep=";", header=TRUE)
  SSB_assessment <- SSB_data # read.csv(paste(getwd(), "/SSB_assessment.csv", sep=""), sep=";", header=TRUE)
  
  rel_err <- list(
    SSB = (as.numeric(SSB_annual$SSB) - as.numeric(SSB_assessment$SSB)) / as.numeric(SSB_assessment$SSB),
    catch = (as.numeric(total_catch_sim$Total) - as.numeric(total_catch_obs$Total)) / as.numeric(total_catch_obs$Total),
    Fbar = (as.numeric(F_annual$F) - as.numeric(F_assessment$F)) / as.numeric(F_assessment$F)
  )
  
  ssb_err   <- mean(rel_err$SSB^2,   na.rm = TRUE)
  catch_err <- mean(rel_err$catch^2, na.rm = TRUE)
  f_err     <- mean(rel_err$Fbar^2,  na.rm = TRUE)
  
  score <- ssb_err + catch_err + f_err
  
  CAL$errors <- rbind(
    CAL$errors,
    data.frame(
      run = CAL$combo_tag,
      SSB = ssb_err,
      Fbar = f_err,
      catch = catch_err,
      total = score,
      stringsAsFactors = FALSE
    )
  )
  
  dir_to_create <- file.path(getwd(), CAL$combo_tag)
  
  if (!dir.exists(dir_to_create)) {
    dir.create(dir_to_create, recursive = TRUE)
  }
  
  write.table(total_catch_sim, paste(getwd(), "/", CAL$combo_tag, "/total_catch_sim.csv", sep=""), sep=";", row.names = FALSE)
  write.table(total_catch_obs, paste(getwd(), "/", CAL$combo_tag, "/total_catch_obs.csv", sep=""), sep=";", row.names = FALSE)
  write.table(F_annual, paste(getwd(), "/", CAL$combo_tag, "/annual_F.csv", sep=""), sep=";", row.names = FALSE)
  write.table(SSB_annual, paste(getwd(), "/", CAL$combo_tag, "/SSB_annual.csv", sep=""), sep=";", row.names = FALSE)
  write.table(monthly_SSB, paste(getwd(), "/", CAL$combo_tag, "/monthly_SSB.csv", sep=""), sep=";", row.names = FALSE)
  
  jpeg(file.path(getwd(), CAL$combo_tag, "SSB_relative_error.jpeg"), width=1300, height=500)
  par(mfrow=c(1,1))
  plot(years, rel_err$SSB, type="l", col="blue",
       main=paste("Relative Error: SSB, config: ", CAL$combo_tag, sep=""),
       ylab="(sim - obs)/obs", xlab="Year")
  abline(h=0, lty=2, col="black")
  dev.off()  
  
  jpeg(file.path(getwd(), CAL$combo_tag, "catch_relative_error.jpeg"), width=1300, height=500)
  par(mfrow=c(1,1))
  plot(years, rel_err$catch, type="l", col="blue",
       main=paste("Relative Error: Catch, config: ", CAL$combo_tag, sep=""),
       ylab="(sim - obs)/obs", xlab="Year")
  abline(h=0, lty=2, col="black")
  dev.off()
  
  jpeg(file.path(getwd(), CAL$combo_tag, "Fbar_relative_error.jpeg"), width=1300, height=500)
  plot(years, rel_err$Fbar, type="l", col="blue",
       main=paste("Relative Error: Fbar, config: ", CAL$combo_tag, sep=""),
       ylab="(sim - obs)/obs", xlab="Year")
  abline(h=0, lty=2, col="black")
  dev.off()  
  
  
  CAL$score <- c(CAL$score, score)
  names(CAL$score)[index] <- CAL$combo_tag
  
}


# extraction function of saved data for global plot
extract_combo_outputs <- function(combo_name, base_dir) {

  folder <- file.path(base_dir, combo_name)
  
  message("Reading combo: ", combo_name)
  message("Folder: ", folder)
  
  ssb_file   <- file.path(folder, "SSB_annual.csv")
  fbar_file  <- file.path(folder, "annual_F.csv")
  catch_file <- file.path(folder, "total_catch_sim.csv")
  
  ssb   <- read.csv(ssb_file, sep=";", header=TRUE)
  fbar  <- read.csv(fbar_file, sep=";", header=TRUE)
  catch <- read.csv(catch_file, sep=";", header=TRUE)
  
  # put everything into long format with combo name
  df <- data.frame(
    Year = ssb$Year,      # assuming same years everywhere
    SSB  = ssb$SSB,
    Fbar = fbar$F,
    Catch = catch$Total,
    Combo = combo_name,
    stringsAsFactors = FALSE
  )
  
  return(df)
}


for (idx in 1:length(list_config)){#

if (!IN_BEMTOOL) {
ALADYM_home <<- getwd()
years <<- c(2016)
years_forecast <<- c(2017)
FLEETSEGMENTS_names <<- NULL
all_years <<- years
INTEGRATED_APPROACH <<- FALSE
SAtool <<- "NONE"
BMT_SCENARIO <<- 0
BMT_HR_CHANGE_FISHMORTALITY <<- 3
GLO <- new.env()
GLO$L_number <- 12
BMT_SPECIES <<- c("")
ALADYM_spe <<- 1
} else {
SKIP_spe <<- FALSE
years_forecast <<- years.forecast
# years <<- years
}

showCompTime <<- F

# if (printOK) cat("\n\n")
print("***************************************************************************", quote=FALSE)
print("Loading ALADYM GUI (Graphical User Interface)...", quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")


print(".......................................... [rungui.r]", quote=F)  
suppressWarnings(source(paste(ALADYM_home, "/src/load_functions.r", sep=""))  )
suppressWarnings(source(paste(ALADYM_home, "/gui/scripts.r", sep="") ) )
suppressWarnings(source(paste(ALADYM_home, "/gui/ini.r", sep="")) )
suppressWarnings(source(paste(ALADYM_home, "/src/paths.r", sep=""))  )
# initialization matrices



Tr <<- 0
if (!IN_BEMTOOL) {
biological.lifeSpanM <<- 1
biological.lifeSpanF <<- 1
} else {
biological.lifeSpanM <<-  as.numeric(as.character(Populations[[ALADYM_spe]]@lifespan$lifespan[[1]]))
biological.lifeSpanF <<- convert_numeric0_to_na(as.numeric(as.character(convert_numeric0_to_na(Populations[[ALADYM_spe]]@lifespan$lifespan[[2]]))))
biological.lifeSpan <<- max(biological.lifeSpanM, biological.lifeSpanF)
}
months_vec_M <<- c(Tr:(biological.lifeSpanM*12))
months_vec_F <<- c(Tr:(biological.lifeSpanF*12))
biological.months_MM <<- length(months_vec_M)
biological.months_MF <<- length(months_vec_F)

main_window <<- gtkWindow(show=FALSE)
 if (!IN_BEMTOOL) {
main_window["title"] <- AAA_VERSION_NAME
} else {
if (phase == "SIMULATION") {
main_window["title"] <- paste("BEMTOOL 2.0 biological simulation - " ,AAA_VERSION_NAME, " [",BMT_SPECIES[ALADYM_spe],"]", sep="" )
} else {
main_window["title"] <- paste("BEMTOOL 2.0 biological forecast - " ,AAA_VERSION_NAME, " [",BMT_SPECIES[ALADYM_spe],"]", sep="")
}
}
main_window$setDefaultSize(1024, 768)
gtkWindowSetResizable(main_window, FALSE)
#gtkWindowSetPosition(main_window, GTK_WIN_POS_CENTER_ALWAYS)

# define TAB
notebook <<- gtkNotebook()
notebook$setTabPos("top")

# 
#  ALADYM in BEMTOOL
# 
 if (IN_BEMTOOL) {
if (phase=="SIMULATION") {

all_years <<- years

suppressWarnings(source(paste(ALADYM_home, "/gui/generaldata.r", sep="") ))					
notebook$appendPage(vboxGeneralData, gtkLabel(str=" GENERAL DATA "))

suppressWarnings(source(paste(ALADYM_home, "/gui/biological.r", sep="") )	)				
notebook$appendPage(vboxBiological, gtkLabel(str=" BIOLOGICAL "))

suppressWarnings(source(paste(ALADYM_home, "/gui/recruitment.r", sep="") ) )					
notebook$appendPage(vboxRecruitment, gtkLabel(str=" RECRUITMENT "))

suppressWarnings(source(paste(ALADYM_home, "/gui/mortality.r", sep="") ) )					
notebook$appendPage(vboxMortality, gtkLabel(str=" MORTALITY "))

# ********************************* FISHERY graphical elements
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery.r", sep="") ) )					
notebook$appendPage(vboxFishery, gtkLabel(str=" FISHERY "))

suppressWarnings( source(paste(ALADYM_home, "/gui/forecast.r", sep="") )	)	
}

if (phase=="FORECAST") {

Tr <<- INP$tr

path_to_save <- paste(casestudy_path, "/Diagnosis/working files/GUIfle_fore.Rdata", sep="")
load(path_to_save)  
FleetList_forecast <<- .GlobalEnv$ALADYM_GUI_fleets_fore[[ALADYM_spe]]

#print(paste("prima di chiamare ALADYM.r carico anno 1 specie", ALADYM_spe))
#print(FleetList_forecast[[1]]@fishingeffort.vector)

##gtkNotebookRemovePage(notebook, gtkNotebookPageNum(notebook, vboxFishery)) 
#gtkComboBoxSetActive(combo_fleetsegments_fore, -1 ) 
#gtkComboBoxSetActive(combo_fleetsegments_fore, 0 )

all_years <<- c(years, years_forecast)
    # ********************************* FISHERY graphical elements
    suppressWarnings(source(paste(ALADYM_home, "/gui/fishery.r", sep="") ))					
    notebook$appendPage(vboxFishery, gtkLabel(str=" SIMULATION "))
    # ********************************* FORECAST graphical elements
   suppressWarnings( source(paste(ALADYM_home, "/gui/forecast.r", sep="") )	)	
   
#  for (item in BMT_FLEETSEGMENTS[associated_fleetsegment_indices]) {
#  combo_fleetsegments_fore$appendText(item)
#    }

gtkComboBoxSetActive(combo_fleetsegments, 0 )
# loadFleetsegmentintoGUI(FleetList_simulation[[1]])
#gtkEntrySetEditable(entryGearName, FALSE) 
  #  gtkComboBoxSetActive(combo_fleetsegments_fore, 0 )
 notebook$appendPage(vboxForecast, gtkLabel(str=" FORECAST "))
} 

# ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
# ???????????????????????????????????????????????????????????????????????????????????????????????????????? ALADYM STAND-ALONE
# ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
} else {

suppressWarnings(source(paste(ALADYM_home, "/gui/generaldata.r", sep="") ))					
notebook$appendPage(vboxGeneralData, gtkLabel(str=" GENERAL DATA "))

suppressWarnings(source(paste(ALADYM_home, "/gui/biological.r", sep="") )	)				
notebook$appendPage(vboxBiological, gtkLabel(str=" BIOLOGICAL "))

suppressWarnings(source(paste(ALADYM_home, "/gui/recruitment.r", sep="") ) )					
notebook$appendPage(vboxRecruitment, gtkLabel(str=" RECRUITMENT "))

suppressWarnings(source(paste(ALADYM_home, "/gui/mortality.r", sep="") ) )					
notebook$appendPage(vboxMortality, gtkLabel(str=" MORTALITY "))

# ********************************* FISHERY graphical elements
suppressWarnings(source(paste(ALADYM_home, "/gui/fishery.r", sep="") ) )					
notebook$appendPage(vboxFishery, gtkLabel(str=" FISHERY "))

suppressWarnings(source(paste(ALADYM_home, "/gui/forecast.r", sep="") )	)	
notebook$appendPage(vboxForecast, gtkLabel(str=" FORECAST "))
 
}  
# disable the forecast box

# ********************************* FORECAST graphical elements - end

vbox <- gtkVBox(homogeneous = FALSE, spacing = 0)
#vbox$packStart(menubar, expand = FALSE, fill = FALSE, padding = 0)

hbox <- gtkHBox(homogeneous = FALSE, spacing = 0)


 # ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
# ???????????????????????????????????????????????????????????????????????????????????????????????????????? ALADYM in BEMTOOL
# ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
if (IN_BEMTOOL) {
if (phase=="SIMULATION") {
hbox$packStart(gtkLabel("After setting all the parameters press the button to go on with BEMTOOL model."), expand = FALSE, fill = FALSE, padding = 5)

btn_browse_BIOfile <- gtkButton()
gtkButtonSetLabel(btn_browse_BIOfile, "Load SIMULATION parameters...")
btn_browse_BIOfile$AddCallback("clicked", select_file_BIO)
hbox$packStart(btn_browse_BIOfile, expand = FALSE, fill = FALSE, padding = 20)

btn_runSimulation <- gtkButton()
gtkButtonSetLabel(btn_runSimulation, "RUN SIMULATION")
btn_runSimulation$AddCallback("clicked", run_simulation)

hbox$packStart(btn_runSimulation, expand = FALSE, fill = FALSE, padding = 20)

btn_skipSimulation <- gtkButton()
gtkButtonSetLabel(btn_skipSimulation, "SKIP >>")
btn_skipSimulation$AddCallback("clicked", skip_simulation)
hbox$packStart(btn_skipSimulation, expand = FALSE, fill = FALSE, padding = 20)

} else {
btn_runScenario <- gtkButton()
gtkButtonSetLabel(btn_runScenario, "RUN FORECAST")
btn_runScenario$AddCallback("clicked", run_forecast)
hbox$packStart(gtkLabel("After setting the selectivity parameters press the button to go on with BEMTOOL scenario."), expand = FALSE, fill = FALSE, padding = 5)
hbox$packStart(btn_runScenario, expand = FALSE, fill = FALSE, padding = 20)

gtkNotebookSetCurrentPage(notebook, 1)
gtkNotebookSetCurrentPage(notebook_forecast,0)

}

# ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
# ???????????????????????????????????????????????????????????????????????????????????????????????????????? ALADYM STAND-ALONE
# ?????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????????
} else {

btn_runSimulation <- gtkButton()
gtkButtonSetLabel(btn_runSimulation, "RUN SIMULATION")
btn_runSimulation$AddCallback("clicked", run_simulation)
hbox$packStart(btn_runSimulation, expand = FALSE, fill = FALSE, padding = 20)

btn_runScenario <- gtkButton()
gtkButtonSetLabel(btn_runScenario, "RUN FORECAST")
btn_runScenario$AddCallback("clicked", run_forecast)
hbox$packStart(btn_runScenario, expand = FALSE, fill = FALSE, padding = 20)


btn_browse_BIOfile <- gtkButton()
gtkButtonSetLabel(btn_browse_BIOfile, "Load SIMULATION parameters...")
btn_browse_BIOfile$AddCallback("clicked", select_file_BIO)
hbox$packStart(btn_browse_BIOfile, expand = FALSE, fill = FALSE, padding = 20)

btn_loadSimulation <- gtkButton()
gtkButtonSetLabel(btn_loadSimulation, "Load SIMULATION workspace...")
btn_loadSimulation$AddCallback("clicked", load_simulation)
hbox$packStart(btn_loadSimulation, expand = FALSE, fill = FALSE, padding = 20)

gtkWidgetSetSensitive(btn_runScenario, FALSE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 1), FALSE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 2), FALSE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 3), FALSE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 4), FALSE)
gtkWidgetSetSensitive(gtkNotebookGetNthPage(notebook, 5), FALSE)

}


vbox$packStart(hbox, FALSE, FALSE, 5)   

vbox$packStart(notebook, expand=FALSE, TRUE)
#vbox$packStart(statusbar, FALSE, FALSE, 0)
main_window$add(vbox)
gtkWindowSetPosition(main_window, as.integer(3))  

 
if (IN_BEMTOOL & phase=="SIMULATION") {
#if (exists("bmt_wnd_sim")) {
#suppressWarnings(bmt_wnd_sim$destroy() )
#}
lockFisheryValues()
} else if (IN_BEMTOOL & phase=="FORECAST") { 
lockFisheryValues()
lockFisheryValues_fore()
}

main_window$show()


cat("\n\n")
print("***************************************************************************", quote=FALSE)
print("ALADYM GUI successfully loaded!", quote=FALSE)
print("***************************************************************************", quote=FALSE)
cat("\n\n")


gtkToggleButtonSetActive(radio_effortdata, TRUE)
gtkToggleButtonSetActive(radio_data, TRUE)


if (!IN_BEMTOOL) {
deactivate_FishingEffort_unused_params()
deactivate_Pproduction_unused_params()
deactivate_FishingM_unused_params()
gtkComboBoxSetActive(combo_discard, 2)
deactivate_Discard_unused_params()
}

if (!IN_BEMTOOL | (IN_BEMTOOL & phase=="SIMULATION")) {
gtkToggleButtonSetActive(radio_Zentry, TRUE)
deactivate_F()
}

AUTOMATIC_CI <<- F

if (IN_BEMTOOL & (phase=="FORECAST") & AUTOMATIC_CI) {
     
    gtkEntrySetText(entry_CI_numb_runs_fore, "500") 
	  gtkToggleButtonSetActive(chkConfidenceIntervals_fore, TRUE)
    	
suppressWarnings(source(paste(ALADYM_home, "/gui/utilities/interconn.bmt/bmt_check_list_fore.r", sep="") ) )
go_on <<- TRUE
#suppressWarnings(source(paste(ALADYM_home, "/gui/utilities/validate_input_fore.r", sep="") ) )

if (go_on) {

# if (exists("ALADYM_GUI_fleets_fore") & (ALADYM_spe == 1) & (!INTEGRATED_APPROACH) ) { rm(ALADYM_GUI_fleets_fore) } 
if (MEY_CALCULATION) {
    suppressWarnings(source(paste(ALADYM_home, "/gui/menubarFun.run_forecast_bmt.MEYcode.r", sep="") ) )
 } else if (!INTEGRATED_APPROACH) {
    suppressWarnings(source(paste(ALADYM_home, "/gui/menubarFun.run_forecast_bmt.NIcode.r", sep="") ) )
 } else {    # integrated approach 
    suppressWarnings(source(paste(ALADYM_home, "/gui/menubarFun.run_forecast_bmt.Icode.r", sep="") ) ) 
 }

}
}



# dialog <- gtkFileChooserDialog("Choose the parameters .CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])

 #length(list_config)
CONFIGURATION_file <<- paste("C:\\INPUT\\ALADYM\\", list_config[idx], sep="")
# CONFIGURATION_file <<- "C:\\INPUT\\ALADYM\\config_run4.csv"
cfg_tag <- str_split(basename(CONFIGURATION_file), "_")[[1]][3:4]
cfg_tag <- gsub(".csv", "", cfg_tag)

CAL$combo_tag <- paste(cfg_tag, collapse="_")

cat("Configuration: ", CAL$combo_tag)

  BIOparameters_table <<- data.frame(read.csv(CONFIGURATION_file, sep=";", na.strings = "") , stringsAsFactors = F )
  BIOmatr <- BIOparameters_table
  suppressWarnings(setBiologicalParams(BIOparameters_table) )
  run_simulation()
  cat(">>>>> Executed configuration ", CAL$combo_tag, "\n")
  extract_results(CAL$summary_table, idx)
  cat(">>>>>> Results extracted \n")
}

sprintf(">>>>>> Best combo is: %s, with score: %f", names(CAL$score)[which.min(unlist(CAL$score))], min(unlist(CAL$score)))

best_score <- sprintf(
  ">>>>>> Best combo is: %s, with score: %f",
  names(CAL$score)[which.min(unlist(CAL$score))],
  min(unlist(CAL$score))
)

writeLines(best_score, paste(getwd(), "/best_combo_result.txt", sep=""))

combo_names <- names(CAL$score)

all_sims <- do.call(rbind, lapply(combo_names, function(x) {
  extract_combo_outputs(x, getwd())
}))



# SSB
p1 <- ggplot(all_sims, aes(x=Year, y=SSB, group=Combo)) +
  geom_line(aes(color = Combo), alpha = 0.3, linewidth = 0.8) +
  geom_point(data=SSB_data,
            aes(x=Year, y=SSB),
            inherit.aes = FALSE,
            color="blue",
            size=2) +
  theme_bw() +
  ggtitle("SSB Sims vs Obs") +
  ylab("SSB") +
  xlab("Year")+
  labs(color = "")

# Fbar
p2 <- ggplot(all_sims, aes(x=Year, y=Fbar, group=Combo)) +
  geom_line(aes(color = Combo), alpha = 0.3, linewidth = 0.8) +
  geom_point(data=Fbar_data,
            aes(x=Year, y=F),
            inherit.aes = FALSE,
            color="blue",
            size=2) +
  theme_bw() +
  ggtitle("F Sims vs Obs") +
  ylab("F") +
  xlab("Year")+
  labs(color = "")

# catch
p3 <- ggplot(all_sims, aes(x=Year, y=Catch, group=Combo)) +
  geom_line(aes(color = Combo), alpha = 0.3, linewidth = 0.8) +
  geom_point(data=catch_data,
            aes(x=Year, y=Catch),
            inherit.aes = FALSE,
            color="blue",
            size=2) +
  theme_bw() +
  ggtitle("Catch Sims vs Obs") +
  ylab("Catch") +
  xlab("Year")+
  labs(color = "")

# best combo
best_combo <- names(which.min(CAL$score))
best_sim <- subset(all_sims, Combo == best_combo)

b1 <- ggplot() +
  geom_line(
    data = best_sim,
    aes(x = Year, y = SSB, color = "Simulation"),
    linewidth = 1.2
  ) +
  geom_point(
    data = SSB_data,
    aes(x = Year, y = SSB, color = "Observed"),
    size = 2
  ) +
  scale_color_manual(values = c("Simulation" = "black", "Observed" = "blue")) +
  theme_bw() +
  ggtitle(paste("SSB – Best combo:", best_combo)) +
  ylab("SSB") +
  xlab("Year") +
  labs(color = "")


b2 <- ggplot() +
  geom_line(
    data = best_sim,
    aes(x = Year, y = Fbar, color = "Simulation"),
    linewidth = 1.2
  ) +
  geom_point(
    data = Fbar_data,
    aes(x = Year, y = F, color = "Observed"),
    size = 2
  ) +
  scale_color_manual(values = c("Simulation" = "black", "Observed" = "blue")) +
  theme_bw() +
  ggtitle(paste("SSB – Best combo:", best_combo)) +
  ylab("F") +
  xlab("Year") +
  labs(color = "")


b3 <- ggplot() +
  geom_line(
    data = best_sim,
    aes(x = Year, y = Catch, color = "Simulation"),
    linewidth = 1.2
  ) +
  geom_point(
    data = catch_data,
    aes(x = Year, y = Catch, color = "Observed"),
    size = 2
  ) +
  scale_color_manual(values = c("Simulation" = "black", "Observed" = "blue")) +
  theme_bw() +
  ggtitle(paste("SSB – Best combo:", best_combo)) +
  ylab("Catch") +
  xlab("Year") +
  labs(color = "")


# save diagnostics (plots + score table)
dir_all_plots <- file.path(getwd(), "Combo_runs_diagnostics")

if (!dir.exists(dir_all_plots)) {
  dir.create(dir_all_plots, recursive = TRUE)
}

ggsave(paste(dir_all_plots, "/SSB_sims_vs_obs.jpeg", sep=""), plot = p1, width = 10, height = 5, units = "in", dpi = 300)
ggsave(paste(dir_all_plots, "/F_sims_vs_obs.jpeg", sep=""), plot = p2, width = 10, height = 5, units = "in", dpi = 300)
ggsave(paste(dir_all_plots, "/Catch_sims_vs_obs.jpeg", sep=""), plot = p3, width = 10, height = 5, units = "in", dpi = 300)

write.table(
  CAL$errors,
  file = paste(dir_all_plots, "/Calibration_errors_summary.csv", sep=""),
  sep = ";",
  row.names = FALSE
)

# plot best combo plots
dir_best_plots <- file.path(getwd(), "Best_combo_diagnostics")

if (!dir.exists(dir_best_plots)) {
  dir.create(dir_best_plots, recursive = TRUE)
}

ggsave(paste(dir_best_plots, "/SSB_sims_vs_obs_best.jpeg", sep=""), plot = b1, width = 10, height = 5, units = "in", dpi = 300)
ggsave(paste(dir_best_plots, "/F_sims_vs_obs_best.jpeg", sep=""), plot = b2, width = 10, height = 5, units = "in", dpi = 300)
ggsave(paste(dir_best_plots, "/Catch_sims_vs_obs_best.jpeg", sep=""), plot = b3, width = 10, height = 5, units = "in", dpi = 300)
