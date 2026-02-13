# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/guicontrols/forecastControls/deactivate_FishingEffort_unused_params_fore.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_FishingEffort_unused_params_fore <-function(w) {
# if (printOK) if (printOK)   print('Calling function: deactivate_FishingEffort_unused_params_forew) {')
 gtkWidgetSetSensitive(VESSELS_fore.treeview, TRUE)
  gtkWidgetSetSensitive(DAYS_fore.treeview, TRUE)
  gtkWidgetSetSensitive(GT_fore.treeview, TRUE)
  gtkWidgetSetSensitive(FISHINGEFFORT_fore.treeview, TRUE)


if ( gtkToggleButtonGetActive(radio_effortdata) ) {
gtkWidgetSetSensitive(VESSELS_fore.treeview, TRUE)
  gtkWidgetSetSensitive(DAYS_fore.treeview, TRUE)
  gtkWidgetSetSensitive(GT_fore.treeview, TRUE)
  gtkWidgetSetSensitive(FISHINGEFFORT_fore.treeview, FALSE)
} else if ( gtkToggleButtonGetActive(radio_fishingcoeff) ) {
  gtkWidgetSetSensitive(VESSELS_fore.treeview, FALSE)
  gtkWidgetSetSensitive(DAYS_fore.treeview, FALSE)
  gtkWidgetSetSensitive(GT_fore.treeview, FALSE)
  gtkWidgetSetSensitive(FISHINGEFFORT_fore.treeview, TRUE)
} 

}
