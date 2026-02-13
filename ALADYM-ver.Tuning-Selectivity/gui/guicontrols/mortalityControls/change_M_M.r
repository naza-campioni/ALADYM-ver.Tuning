# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/guicontrols/mortalityControls/change_M_M.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





change_fishingM_M<- function(w) {
# if (printOK) if (printOK)   print('Calling function: change_fishingM_Mw) {')

 n_ages_M  <- convert_numeric0_to_na(as.numeric(as.character(gtkEntryGetText(entryVBF_M_lifespan))))  
 # print(paste("changed life span for males!", n_ages_M) )
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# additional code for BEMTOOL integration
mortality_dataframe_M_M <- get_table("FISHING_MORTALITY_M")

mortality.Fvector.males <<- mortality_dataframe_M_M 

ages_for_FMales <-  colnames(mortality_dataframe_M_M)

  if (length(FleetList_simulation) != 0 ) {

for (fl in 1:length(FLEETSEGMENTS_names)) {
 mortality_dataframe_M_M <-  FleetList_simulation[[fl]]@fishingmortality.M.vector
 
  mortality_dataframe_M_M <- mortality_dataframe_M_M[,which(colnames(mortality_dataframe_M_M) %in% ages_for_FMales)]

FleetList_simulation[[fl]]@fishingmortality.M.vector <<- mortality_dataframe_M_M

}
}

#mortality.FishingMvector.males <<- mortality.FishingMvector.males[,c(1:n_ages_M)]

# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------

#------------------------------------------ load the file
source(paste(ALADYM_home, "/gui/fishery/fishery.fishingmortalityTableM.r", sep=""))


}
