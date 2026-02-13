# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/guicontrols/forecastControls/change_chkFMSY.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




change_chkFMSY <-function(w){
# if (printOK) if (printOK)   print('Calling function: change_chkFMSYw){')
          gtkWidgetSetSensitive(lbl_TargetYear, TRUE)
          gtkWidgetSetSensitive(lbl_TargetF, TRUE)
          gtkWidgetSetSensitive(entry_targetF, TRUE)
          gtkWidgetSetSensitive(entry_targetMonth, TRUE)
          gtkWidgetSetSensitive(combo_Scenariotype, TRUE)
          gtkWidgetSetSensitive(vboxFisherySelectivity_fore, TRUE)
          gtkWidgetSetSensitive(vboxFishingFore_effortdata, TRUE)  
      if (gtkToggleButtonGetActive(chkFMSY)) {
          gtkWidgetSetSensitive(lbl_TargetYear, TRUE)
          gtkWidgetSetSensitive(lbl_TargetF, TRUE)
          gtkWidgetSetSensitive(entry_targetF, TRUE)
          gtkWidgetSetSensitive(entry_targetMonth, TRUE)
          gtkWidgetSetSensitive(combo_Scenariotype, TRUE)
          gtkWidgetSetSensitive(vboxFisherySelectivity_fore, FALSE)
          gtkWidgetSetSensitive(vboxFishingFore_effortdata, FALSE)  
      } else {
          gtkWidgetSetSensitive(lbl_TargetYear, FALSE)
          gtkWidgetSetSensitive(lbl_TargetF, FALSE)
          gtkWidgetSetSensitive(entry_targetF, FALSE)
          gtkWidgetSetSensitive(entry_targetMonth, FALSE)
          gtkWidgetSetSensitive(combo_Scenariotype, FALSE)
          gtkWidgetSetSensitive(vboxFisherySelectivity_fore, TRUE)
          gtkWidgetSetSensitive(vboxFishingFore_effortdata, TRUE)  
      } 
}
