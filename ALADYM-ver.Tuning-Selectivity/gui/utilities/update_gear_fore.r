# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/utilities/update_gear_fore.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





# ---------------------- Create and add gear to the list
update_gear_fore <- function(w) {
# if (printOK) if (printOK)   print('Calling function: update_gear_forew) {')
#print(".......................................... [upadate_gear_fore.r] --> update_gear_fore()")
# new_aldFleetsegment <<- new(Class="aldFleetsegment") 
# new_aldFleetsegment <<- setFleetsegmentfromGUI(new_aldFleetsegment)

index_to_update = -1
selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
index_to_update <- which(FLEETSEGMENTS_names == selected)        
print(paste("Fleetsegment forecast to be updated: ", selected, "[",index_to_update,"]", sep=""))
FleetList_forecast[index_to_update] <<- updateFleetsegment_forefromGUI(FleetList_forecast[[index_to_update]])

# FleetList_simulation <<- c(FleetList_simulation, new_aldFleetsegment)
# FleetList_forecast <<- FleetList_simulation
# return(new_aldFleetsegment@fleetname)
}
