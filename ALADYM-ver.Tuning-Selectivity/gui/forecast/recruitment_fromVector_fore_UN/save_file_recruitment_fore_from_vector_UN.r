# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/forecast/recruitment_fromVector_fore_UN/save_file_recruitment_fore_from_vector_UN.r'), '\n')
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
#
#
#
#
#
#
# ------------------------------------------------------------------------------
# Function for the saving of the vector
# ------------------------------------------------------------------------------
#
save_file_recruitment_fore_from_vector_UN <- function(w) {
# if (printOK) if (printOK)   print('Calling function: save_file_recruitment_fore_from_vector_UNw) {')
dialog <- gtkFileChooserDialog("Enter a name for the .csv file", main_window, "save", "gtk-cancel", GtkResponseType["cancel"], "gtk-save", GtkResponseType["accept"])
if (dialog$run() == GtkResponseType["accept"]) {

save_path <- dialog$getFilename()

vai <- T
dialog$destroy()
} else {
 vai <- F
dialog$destroy()
}

if (vai) {
gtkWidgetSetSensitive(main_window, FALSE)
wnd <- showMessage("        Saving recruitment_fore_from_vector...        ")

# create pproduction table 
recruitment_table <- data.frame(matrix(nrow=length(years_forecast), ncol=(length(MONTHS)+1) ))
colnames(recruitment_table) <- c("year", MONTHS)
recruitment_table$year <- years_forecast
     
for (i in 1:length(recruitments_fore_from_vector_UN)) {
  for (m in 2:13) {
      recruitment_table[as.character(recruitment_table$year) == as.character(recruitments_fore_from_vector_UN[[i]]$year),m] <-  recruitments_fore_from_vector_UN[[i]][m-1]
  }
} 

write.table(recruitment_table, save_path,  sep=";", na = "",row.names = FALSE)

wnd$destroy()   
gtkWidgetSetSensitive(main_window, TRUE)
wnd <- showMessageOK("        Recruitment saved!        ")


} 


}

