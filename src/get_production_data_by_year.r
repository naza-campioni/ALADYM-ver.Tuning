# if (printOK) if (printOK) cat('Sourcing file: ', basename('C:/Users/emant/Downloads/stecf_tech_measures-main3/stecf_tech_measures-main/BEMTOOL2.5/code/src/biol/bmtALADYM/ALADYM-ver12.3-2017_0501/src/get_production_data_by_year.r'), '\n')
# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




# #############################################################################################
# SET ALADYM FROM BEMTOOL IN ACCORDING TO THE SCENARIO
# #############################################################################################

get_production_data_by_year <- function() {
# if (printOK) if (printOK)   print('Calling function: get_production_data_by_year) {')
 
prod_data_all <- data.frame(matrix(nrow=length(FLEETSEGMENTS_names), ncol=length(years)+1))
colnames(prod_data_all) <- c("Gear",	years)

prod_data_all$Gear <- FLEETSEGMENTS_names

for (n_prod in 1:length(FLEETSEGMENTS_names)) {
for (yy in 1:length(years)) {
    prod_data_all[n_prod, yy+1] <- sum(as.numeric( as.character(FleetList_simulation[[n_prod]]@production.vector[yy, 3:14] )))
} # end years
 

}  # end fleet loop 

write.table(prod_data_all, file=PRODUCTION_DATA_byYEAR_working_table, sep=";", row.names=F)
 
return(prod_data_all) 
}
