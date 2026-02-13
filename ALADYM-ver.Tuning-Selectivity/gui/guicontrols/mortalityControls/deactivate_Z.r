# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/guicontrols/mortalityControls/deactivate_Z.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




deactivate_Z <-function(w){
# if (printOK) if (printOK)   print('Calling function: deactivate_Zw){')
  gtkWidgetSetSensitive(h_frame_totalmortality, FALSE)
  gtkWidgetSetSensitive(vboxFishingMortality, TRUE)
  gtkWidgetSetSensitive(button_saveall_Fmortalities_by_fleet, TRUE)
    gtkWidgetSetSensitive(button_saveall_Fmortalities_overall, TRUE)
  gtkWidgetSetSensitive(button_load_F_by_fleet, TRUE)
    gtkWidgetSetSensitive(button_load_F_overall, TRUE)
  gtkWidgetSetSensitive(vboxFisherySelectivity, FALSE)
    gtkWidgetSetSensitive(button_load_selectivity, FALSE)
    gtkWidgetSetSensitive(button_saveall_selectivity, FALSE)    
 
   new_aldSimulation@enteringMortality <<- "F" 
    
    if ((!IN_BEMTOOL) | (IN_BEMTOOL & phase=="FORECAST")) {
    gtkWidgetSetSensitive(vboxFisherySelectivity_fore, FALSE)     
    gtkWidgetSetSensitive(button_saveall_selectivity_fore, FALSE)  
     gtkWidgetSetSensitive(button_load_selectivity_fore, FALSE)
    }
    

    
     
}
