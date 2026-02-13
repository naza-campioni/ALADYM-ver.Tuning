# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/fishery/effort.daysFun/add.DAYS.r'), '\n')
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
# ------------------------------------------------------------------------------
# add elements to the list of the p production values
# ------------------------------------------------------------------------------
#
add.DAYS <- function() {
#print("Adding elements to the list...")   
  if (!is.null(fleet.DAYS)) {
  for (r in 1:nrow(fleet.DAYS)) {
  DAYS_temp <- as.list(fleet.DAYS[r,]) 
  names(DAYS_temp) <- c("year",MONTHS)
  DAYS <<- c(DAYS, list(DAYS_temp)) 
  }
   } else {
   DAYS_matrix <- data.frame(matrix(0, nrow=length(years), ncol=13))
   colnames(DAYS_matrix) <- c("year",MONTHS)
     DAYS_matrix$year <- years
   for (r in 1:nrow(DAYS_matrix)) { 
  DAYS_temp <- as.list(DAYS_matrix[r,]) 
  DAYS <<- c(DAYS, list(DAYS_temp)) 
  }
 }
 #print("DAYS (simulation) list successfully updated!", quote=F)
}
