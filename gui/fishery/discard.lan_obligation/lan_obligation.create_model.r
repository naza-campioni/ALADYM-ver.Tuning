# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/gui/fishery/discard.lan_obligation/lan_obligation.create_model.r'), '\n')
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
# create model for the tree of p production
# ------------------------------------------------------------------------------
#
lan_obligation.create_model <- function() {
#print("Creating model...")   
  # create list store
  lan_obligation.model <<- gtkListStoreNew("gchararray",  rep("gchararray", length(MONTHS)), "gboolean")  
  add.lan_obligation()
  # add items 
  for (i in 1:length(lan_obligation)) {
    iter <- lan_obligation.model$append()$iter
   #print(paste("in sexratios.model:", as.character(sexratios[[i]]$month)))
    lan_obligation.model$set(iter,0, lan_obligation[[i]]$year)
    for (e in 1:length(MONTHS)) {
   # print(paste("in model:", years[nc]) )
         lan_obligation.model$set(iter, e, as.character(lan_obligation[[i]][e+1]) )
    }
       lan_obligation.model$set(iter, 13,TRUE)
  } 
  #print("lan_obligation Model successfully created!")  
}
