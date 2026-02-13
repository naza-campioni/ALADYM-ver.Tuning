# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/fishery/pproductionFun/reload_pproduction_table.r'), '\n')
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
# ------------------------------------------------------------------------------
# Function to reload the values for the production according to the 
# seed value
# ------------------------------------------------------------------------------
#
reload_EMPTY_pproduction_table<- function(w) {
# if (printOK) if (printOK)   print('Calling function: reload_EMPTY_pproduction_tablew) {')

  pproductions <<- list()
  pproductionsIndex <<- 0

   pproductions_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(pproductions_matrix) <- c("year",MONTHS)
     pproductions_matrix$year <- years
   for (r in 1:nrow(pproductions_matrix)) { 
  pproductions_temp <- as.list(pproductions_matrix[r,]) 
  pproductions <<- c(pproductions, list(pproductions_temp)) 
  }
  
    fleet.pproduction <<- pproductions_matrix
  
pproductions.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(pproductions)) {
    iter <- pproductions.model$append()$iter
    pproductions.model$set(iter,0, pproductions[[i]]$year)
    for (e in 1:length(MONTHS)) {
         pproductions.model$set(iter, e, as.numeric(pproductions[[i]][e+1]))
    }
       pproductions.model$set(iter, 13,TRUE)
  } 
 
 pproductions.treeview$destroy()
  pproductions.treeview <<- gtkTreeViewNewWithModel( pproductions.model)
 pproductions.treeview$setRulesHint(TRUE)
 pproductions.treeview$getSelection()$setMode("single")
pproductions.add_columns( pproductions.treeview) 
pproductions.sw$add(pproductions.treeview)
   
}



# if (printOK)   print('Calling function: reload_pproduction_tablew) {')
reload_pproduction_table<- function(w) {

  pproductions <<- list()
  pproductionsIndex <<- 0

   pproductions_matrix <- fleet.pproduction 
   for (r in 1:nrow(pproductions_matrix)) { 
  pproductions_temp <- as.list(pproductions_matrix[r,]) 
  pproductions <<- c(pproductions, list(pproductions_temp)) 
  }
  
pproductions.model <<- gtkListStoreNew("gchararray",  rep("gdouble", length(MONTHS)), "gboolean")  
  # add items 
  for (i in 1:length(pproductions)) {
    iter <- pproductions.model$append()$iter
    pproductions.model$set(iter,0, pproductions[[i]]$year)
    for (e in 1:length(MONTHS)) {
         pproductions.model$set(iter, e, as.numeric(pproductions[[i]][e+1]))
    }
       pproductions.model$set(iter, 13,TRUE)
  } 
 
 pproductions.treeview$destroy()
  pproductions.treeview <<- gtkTreeViewNewWithModel( pproductions.model)
 pproductions.treeview$setRulesHint(TRUE)
 pproductions.treeview$getSelection()$setMode("single")
pproductions.add_columns( pproductions.treeview) 
pproductions.sw$add(pproductions.treeview)
   
}
