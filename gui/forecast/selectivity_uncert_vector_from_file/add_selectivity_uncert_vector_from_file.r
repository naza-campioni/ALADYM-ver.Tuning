# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/forecast/selectivity_uncert_vector_from_file/add_selectivity_uncert_vector_from_file.r'), '\n')
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
# add elements to the list of stock-recruitment_fore_from_vector values
# ------------------------------------------------------------------------------
#
add_selectivity_uncert_vector_from_file <- function() {
# if (printOK) if (printOK)   print('Calling function: add_selectivity_uncert_vector_from_file) {')
#print("Adding elements to the list...")   
  if (!is.null(selectivity_uncert_vector_from_file_matrix)) {
  for (r in 1:nrow(selectivity_uncert_vector_from_file_matrix)) {
  sr_temp <- as.list(selectivity_uncert_vector_from_file_matrix[r,]) 
  names(sr_temp) <- c("run_N",	"param1",	"param2",	"param3",	"param4",	"param5",	"sel_type",	"fleet")
  selectivity_uncert_vector_from_file_list <<- c(selectivity_uncert_vector_from_file_list, list(sr_temp)) 
  }
   } else {
    zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS_FORE, ncol=8 ))
     colnames(zero_matrix) <- c("run_N",	"param1",	"param2",	"param3",	"param4",	"param5",	"sel_type",	"fleet")
   for (r in 1:nrow(zero_matrix)) { 
  sr_temp <- as.list(zero_matrix[r,]) 
  selectivity_uncert_vector_from_file_list <<- c(selectivity_uncert_vector_from_file_list, list(sr_temp)) 
  }
   
 }
# print("recruitments_fore_from_vector list successfully updated!", quote=F)
}

