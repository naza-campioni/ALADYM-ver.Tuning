# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/guicontrols/fisheryControls/deactivate_Survivability_unused_params.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.





deactivate_Survivability_unused_params <-function(w) {
# if (printOK) if (printOK)   print('Calling function: deactivate_Survivability_unused_paramsw) {')
 select_index = -1
 
gtkWidgetSetSensitive(lbl_option_survivability, TRUE)
gtkWidgetSetSensitive(radio_survivability_ogive, TRUE)
gtkWidgetSetSensitive(radio_survivability_constant, TRUE)
gtkWidgetSetSensitive(lbl_option_survivability_param1_males, TRUE)
gtkWidgetSetSensitive(entry_survivability_param1_males, TRUE)
gtkWidgetSetSensitive(lbl_option_survivability_param2_females, TRUE)
gtkWidgetSetSensitive(entry_survivability_param2_females, TRUE)


selected <- gtkComboBoxGetActiveText(combo_discard_survival_rate)

selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)


if (selected == "YES" ) {

      if (length(select_index_fleet) != 0) {
       FleetList_simulation[[select_index_fleet]]@discard.survivability.calculation <<- "Y"
      }

gtkWidgetSetSensitive(lbl_option_survivability, T)
gtkWidgetSetSensitive(radio_survivability_ogive, T)
gtkWidgetSetSensitive(radio_survivability_constant, T)
gtkWidgetSetSensitive(lbl_option_survivability_param1_males, T)
gtkWidgetSetSensitive(entry_survivability_param1_males, T)
gtkWidgetSetSensitive(lbl_option_survivability_param2_females, T)
gtkWidgetSetSensitive(entry_survivability_param2_females, T)

 selected <- gtkComboBoxGetActiveText(combo_discard_survival_rate)

if (selected == "NO") { 
   
   gtkWidgetSetSensitive(lbl_option_survivability, F)
gtkWidgetSetSensitive(radio_survivability_ogive, F)
gtkWidgetSetSensitive(radio_survivability_constant, F)
gtkWidgetSetSensitive(lbl_option_survivability_param1_males, F)
gtkWidgetSetSensitive(entry_survivability_param1_males, F)
gtkWidgetSetSensitive(lbl_option_survivability_param2_females, F)
gtkWidgetSetSensitive(entry_survivability_param2_females, F)

} else {

    if (  gtkToggleButtonGetActive(radio_survivability_ogive) ) {

gtkLabelSetText(lbl_option_survivability_param1_males, "L50% [mm]")
gtkLabelSetText(lbl_option_survivability_param2_females, "L75%L25% [mm]")

    } else {
gtkLabelSetText(lbl_option_survivability_param1_males, "MALES")
gtkLabelSetText(lbl_option_survivability_param2_females, "FEMALES")

    }
  }

} else {
   
         if (length(select_index_fleet) != 0) {
       FleetList_simulation[[select_index_fleet]]@discard.survivability.calculation <<- "N"
      }
   
   gtkWidgetSetSensitive(lbl_option_survivability, F)
gtkWidgetSetSensitive(radio_survivability_ogive, F)
gtkWidgetSetSensitive(radio_survivability_constant, F)
gtkWidgetSetSensitive(lbl_option_survivability_param1_males, F)
gtkWidgetSetSensitive(entry_survivability_param1_males, F)
gtkWidgetSetSensitive(lbl_option_survivability_param2_females, F)
gtkWidgetSetSensitive(entry_survivability_param2_females, F)

}


deactive_survivability_O_C()


}


# if (printOK)   print('Calling function: deactive_survivability_O_Cw) {')
deactive_survivability_O_C <- function(w) {

selected_fleet <- gtkComboBoxGetActiveText(combo_fleetsegments)
select_index_fleet <- which(FLEETSEGMENTS_names == selected_fleet)


selected <- gtkComboBoxGetActiveText(combo_discard_survival_rate)

if (selected == "NO") { 
   
gtkWidgetSetSensitive(lbl_option_survivability, F)
gtkWidgetSetSensitive(radio_survivability_ogive, F)
gtkWidgetSetSensitive(radio_survivability_constant, F)
gtkWidgetSetSensitive(lbl_option_survivability_param1_males, F)
gtkWidgetSetSensitive(entry_survivability_param1_males, F)
gtkWidgetSetSensitive(lbl_option_survivability_param2_females, F)
gtkWidgetSetSensitive(entry_survivability_param2_females, F)

} else {

    if (  gtkToggleButtonGetActive(radio_survivability_ogive) ) {

gtkLabelSetText(lbl_option_survivability_param1_males, "L50% [mm]")
gtkLabelSetText(lbl_option_survivability_param2_females, "L75%L25% [mm]")

   if (length(select_index_fleet) > 0) {
    FleetList_simulation[[select_index_fleet]]@discard.survivability.datatype <<- "DOS" 
     }

    } else {
gtkLabelSetText(lbl_option_survivability_param1_males, "MALES")
gtkLabelSetText(lbl_option_survivability_param2_females, "FEMALES")

   if (length(select_index_fleet) > 0) {
   FleetList_simulation[[select_index_fleet]]@discard.survivability.datatype <<- "C"
   }
    }
  }
}
