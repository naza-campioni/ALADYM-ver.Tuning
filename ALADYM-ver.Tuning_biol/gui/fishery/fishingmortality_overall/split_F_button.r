# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/fishery/fishingmortality_overall/split_F_button.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.



split_F_button <- function(w) {
# if (printOK) if (printOK)   print('Calling function: split_F_buttonw) {')

wnd <- showMessage("        Splitting F overall...        ")   #  --------------------------------------
      gtkWidgetSetSensitive(main_window, F)
 

       
split_F(new_aldSimulation@Fsplittingtype)      

wnd$destroy()
gtkWidgetSetSensitive(main_window, TRUE)

}    




