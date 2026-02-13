# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/forecast/fleetTabsFun/reload_fleetsegment_fore_info.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




reload_fleetsegment_fore_info<-function(w) {
# if (printOK) if (printOK)   print('Calling function: reload_fleetsegment_fore_infow) {')
#print(".......................................... [forecast.fleetTabsFun.r] --> reload_fleetsegment_fore_info()", quote=F)
  index_to_load = -1
  selected <- gtkComboBoxGetActiveText(combo_fleetsegments_fore)
  if (!is.null(selected)) {
  
    index_to_load <- which(FLEETSEGMENTS_names == selected )
  loadFleetsegment_foreintoGUI(FleetList_forecast[[index_to_load]])
  } #else  {
#  clear_ForecastGUI(w)
#  }
  # print("END....................................... [forecast.fleetTabsFun.r] --> reload_fleetsegment_fore_info()", quote=F)
}
