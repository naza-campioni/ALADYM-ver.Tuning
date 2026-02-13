# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/forecast/recrFromVector_fore/load_recruitment_fromVector_fore_file.r'), '\n')
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
# ------------------------------------------------------------------------------
# Function for the selection of the file where values of stock-recruitment are 
# saved and the loading of those values in the table
# ------------------------------------------------------------------------------
#
load_recruitment_fromVector_fore_file <- function(w) {
# if (printOK) if (printOK)   print('Calling function: load_recruitment_fromVector_fore_filew) {')
dialog <- gtkFileChooserDialog("Choose a CSV file", main_window, "open", "gtk-cancel", GtkResponseType["cancel"], "gtk-open", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {
extRecruitment_fore_file <<- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Loading recruitment from file...        ")

external_recruitment_matrix_fore <<- read.csv(extRecruitment_fore_file, sep=";")

#if (nrow(CI_external_matrix_fore) != CI_NB_RUNS_FORE) {
#    showError("The number of the recruitment errors is not consistent with the inserted number of runs!")
#} else { 
#------------------------------------------ load the file
#reload_extErrorRecruitment_fore()
#}


wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Recruitment error loaded!        ")


} 
} 
