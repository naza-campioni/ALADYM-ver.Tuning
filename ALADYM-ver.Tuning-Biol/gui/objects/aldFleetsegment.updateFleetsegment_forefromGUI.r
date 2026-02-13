# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/objects/aldFleetsegment.updateFleetsegment_forefromGUI.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




#
#
#
#
# Function that save changes to an existing fleet segment (FORECAST) taking all the parameters from the GUI
#           
updateFleetsegment_forefromGUI<-function(object) {
# if (printOK) if (printOK)   print('Calling function: updateFleetsegment_forefromGUIobject) {')
#print(".......................................... [aldFleetsegment.r] --> updateFleetsegment_forefromGUI()", quote=F)
#object@selectivity.file <- selectivity_file_fore
object@selectivity.vector <- get_table("SELECTIVITY_FORE")  
object@production.datatype  <- "" 
#object@production.file <- ""
object@production.vector <- data.frame(NULL)
#object@discard.file <- discard_file_fore
object@discard.vector <- get_table("DISCARD_FORE")

# new_aldForecast@reduction_scenario <<- 
#object@fishingeffort.file <- fishingeffort_file_fore
#object@fishingeffort.vector <- get_table("FISHING_EFFORT_FORE")

#print("Fleet segment for forecast updated!", quote=F)
return(object)
}
