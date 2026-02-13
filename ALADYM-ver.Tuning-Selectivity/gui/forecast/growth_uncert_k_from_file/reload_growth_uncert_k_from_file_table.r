# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/forecast/growth_uncert_k_from_file/reload_growth_uncert_k_from_file_table.r'), '\n')
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
# Function to reload the values for the recruitment_fore_from_vector according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_growth_uncert_k_from_file_table <- function(w) {
# if (printOK) if (printOK)   print('Calling function: reload_EMPTY_growth_uncert_k_from_file_tablew) {')
  growth_uncert_k_from_file_list <<- list()
growth_uncert_k_from_file_index <<- 0

   zero_matrix <- data.frame(matrix(-1, nrow=CI_NB_RUNS_FORE, ncol=3 ))
     colnames(zero_matrix) <- c("run_N", "MALES", "FEMALES") 
   for (r in 1:nrow(zero_matrix)) { 
  sr_temp <- as.list(zero_matrix[r,]) 
  growth_uncert_k_from_file_list <<- c(growth_uncert_k_from_file_list, list(sr_temp)) 
  }
  
  growth_uncert_k_from_file_matrix <<- zero_matrix
 growth_uncert_k_from_file_model <<- gtkListStoreNew("gchararray", "gdouble","gdouble", "gboolean")
 
  for (i in 1:length(growth_uncert_k_from_file_list)) {
    iter <-  growth_uncert_k_from_file_model$append()$iter
     growth_uncert_k_from_file_model$set(iter,0, growth_uncert_k_from_file_list[[i]]$run_N) 
     growth_uncert_k_from_file_model$set(iter, 1, as.numeric(as.character(growth_uncert_k_from_file_list[[i]]$MALES)) )
	    growth_uncert_k_from_file_model$set(iter, 2, as.numeric(as.character(growth_uncert_k_from_file_list[[i]]$FEMALES)) )
     growth_uncert_k_from_file_model$set(iter, 3,TRUE)
  } 

  growth_uncert_k_from_file_treeview$destroy()
 growth_uncert_k_from_file_treeview <<- gtkTreeViewNewWithModel( growth_uncert_k_from_file_model)
 growth_uncert_k_from_file_treeview$setRulesHint(TRUE)
 growth_uncert_k_from_file_treeview$getSelection()$setMode("single")
 
 growth_uncert_k_from_file_add_columns(  growth_uncert_k_from_file_treeview)  
 growth_uncert_k_from_file_sw$add( growth_uncert_k_from_file_treeview)
   
}



# if (printOK)   print('Calling function: reload_growth_uncert_k_from_file_tablew) {')
reload_growth_uncert_k_from_file_table <- function(w) {
  growth_uncert_k_from_file_list <<- list()
 growth_uncert_k_from_file_index <<- 0

       this_matrix <-  growth_uncert_k_from_file_matrix  
   for (r in 1:nrow(this_matrix)) { 
  sr_temp <- as.list(this_matrix[r,]) 
  growth_uncert_k_from_file_list <<- c(growth_uncert_k_from_file_list, list(sr_temp)) 
  }
  
   growth_uncert_k_from_file_model <<- gtkListStoreNew("gchararray", "gdouble" ,"gdouble" ,"gboolean")
 
  for (i in 1:length(growth_uncert_k_from_file_list)) {
    iter <-  growth_uncert_k_from_file_model$append()$iter
     growth_uncert_k_from_file_model$set(iter,0, growth_uncert_k_from_file_list[[i]]$run_N) 
     growth_uncert_k_from_file_model$set(iter, 1, as.numeric(as.character(growth_uncert_k_from_file_list[[i]]$MALES)) )
	 growth_uncert_k_from_file_model$set(iter, 2, as.numeric(as.character(growth_uncert_k_from_file_list[[i]]$FEMALES)) )
     growth_uncert_k_from_file_model$set(iter, 3,TRUE)
  } 

  growth_uncert_k_from_file_treeview$destroy()
 growth_uncert_k_from_file_treeview <<- gtkTreeViewNewWithModel( growth_uncert_k_from_file_model)
 growth_uncert_k_from_file_treeview$setRulesHint(TRUE)
 growth_uncert_k_from_file_treeview$getSelection()$setMode("single")
 growth_uncert_k_from_file_add_columns( growth_uncert_k_from_file_treeview)  
 growth_uncert_k_from_file_sw$add(growth_uncert_k_from_file_treeview)

    
}
