# ALADYM  Age length based dynamic model - version 12.3
# Authors: G. Lembo, I. Bitetto, M.T. Facchini, M.T. Spedicato 2018
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# In case of use of the model, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail address: facchini@coispa.it
# ALADYM is believed to be reliable. However, we disclaim any implied warranty or representation about its accuracy, 
# completeness or appropriateness for any particular purpose.




RFSS_env <- function(S_unit, para_SS_Population, para_RLt, para_RLa, para_RLb, para_RLc, para_RLd, env1=0, env2=0, para_R=NA) {

if(BMT_SPECIES[ALADYM_spe]=="M.mer1718" ){
para_RLc=0.123  # HKE 1718
para_RLd=0  # HKE 1718
} else if (BMT_SPECIES[ALADYM_spe]=="M.mer19"){
para_RLc=0.244      # HKE19
para_RLd= 0     # HKE19
} else if (BMT_SPECIES[ALADYM_spe]=="M.bar1718"){
para_RLc=0.395      # mut1718
para_RLd=  0.014    # mut1718
} else if (BMT_SPECIES[ALADYM_spe]=="M.bar19"){
para_RLc=0.546      # mut19
para_RLd=  0   # mut19
} else if (BMT_SPECIES[ALADYM_spe]=="P.lon171819"){
para_RLc=0      # dps
para_RLd=  -0.424   # dps
} else if (BMT_SPECIES[ALADYM_spe]=="A.fol181920"){
para_RLc=0      # ARS
para_RLd= -0.32   # ARS
 } else if (BMT_SPECIES[ALADYM_spe]=="A.ant181920"){
para_RLc=0      # ARA
para_RLd=  -0.483   # ARA
}
#print(BMT_SPECIES[ALADYM_spe],quote=F) 
#print(para_RLc,quote=F) 
#print(para_RLd,quote=F)
  
if (FALSE) {
S_unit =INP$S_unit
#para_SS_Population= loca_SS_Population
para_RLt = INP$FRLt_fore
para_RLa= INP$FRLa_fore
para_RLb = INP$FRLb_fore
para_RLc=0      # ARS
para_RLd= -0.349
para_R = NA #INP$Recruits[loca_irun + 1]
  para_SS_Population=SRO$FSSBiomass[loca_irun]
}

#print(S_unit)
#print(para_SS_Population)
#print(para_RLt)
#print(para_RLa) 
#print(para_RLb)
#print(para_RLc)
#print(para_RLd)
#print(env1)
#print(env2)
#print(para_R)

  loca_temp <- 0
  
   if(INP$S_unit==1){      
   para_SS_Population <-    para_SS_Population/1000000 # biomassa in tons
   
   } else {
   para_SS_Population <-    para_SS_Population  /1000  # la relazione S-R vuole i numeri in migliaia    
   }
   
   
 #  print(   para_RLt)
   
  if(para_RLt == 1) {
     #loca_temp <- para_SS_Population / (para_RLa + para_RLb * para_SS_Population)  * 1000         # il numero di reclute ? in migliaia, devo trasformarlo il numeri assoluti # BEVHOLT
    # loca_temp <- para_RLa*para_SS_Population / ( 1+ para_RLb * para_SS_Population)  * 1000  
 #####################loca_temp <- para_RLa*para_SS_Population / ( 1+ para_RLb * para_SS_Population)  * 1000  
 # loca_temp <- para_RLa*para_SS_Population / ( para_RLb+ para_SS_Population)  * 1000  # * rlnorm(1,-0.5^2,0.5)
 
          loca_temp <- para_RLa * para_SS_Population / (1+ para_RLb * para_SS_Population) * exp(-para_RLc*env1-para_RLd*env2) * 1000 
         # print(loca_temp)  
     #Eqsim equation: ab$a * ssb / (1 + ab$b * ssb)
              
  } else if (para_RLt == 2) {
    ################loca_temp <- para_RLa * para_SS_Population * exp(-para_RLb * para_SS_Population) * 1000                  # il numero di reclute ? in migliaia       # RICKER
    # log(ab$a) + log(ssb) - ab$b * ssb
    
     #R ~ a * S * exp(-b * S - c * sst - d * nppv)
    
    loca_temp <- para_RLa * para_SS_Population *exp(-para_RLb * para_SS_Population-para_RLc*env1-para_RLd*env2) * 1000 
    
  } else if (para_RLt == 3) {
    loca_temp <- para_RLa * para_SS_Population / (1 + (para_SS_Population / para_RLc)^para_RLb)     * 1000       # il numero di reclute ? in migliaia
  
  } else if (para_RLt == 4) {
    loca_temp <- para_R  * 1000       # il numero di reclute ? in migliaia  # CONSTANT
  
  } else if(para_RLt == 5) {
    loca_temp <- para_RLa * min(para_SS_Population, para_RLb)  * 1000       # il numero di reclute ? in migliaia  # HOCKEY STICK
  
  } else if(para_RLt == 6) {
  
    if(para_SS_Population <= (para_RLb * (1 - para_RLc))) {
      loca_temp <- para_RLa * para_SS_Population   * 1000       # il numero di reclute ? in migliaia
    } else if (para_SS_Population >= (para_RLb * (1 + para_RLc))) {
      loca_temp <- para_RLa * para_RLb   * 1000       # il numero di reclute ? in migliaia
    } else {  
      #  R <- a * (SSB - (SSB - b * (1 - c))^2 / (4 * c * b))     
     loca_temp <- para_RLa * (para_SS_Population - (para_SS_Population - para_RLb * (1 - para_RLc))^2 / (4 * para_RLc * para_RLb))   * 1000       # il numero di reclute ? in migliaia
    }
    
  }
  
 # print(loca_temp)
  # else if(para_RLt == 7) { # BevHolt env
    
    # a * S/(1 + b * S) * exp(-c * botT - d * sst)
    #loca_temp <- para_RLa * para_SS_Population / (1+ para_RLb * para_SS_Population) * exp(-para_RLc*env1-para_RLc*env2) * 1000   
  #}  else if(para_RLt ==8) { # Ricker env
    
    # a * S * exp(-b * S - c * sst - d * nppv)
   # loca_temp <- para_RLa * para_SS_Population *exp(-para_RLb * para_SS_Population-para_RLc*env1-para_RLc*env2) * 1000   
  #}
  
  # print(loca_temp)
  return(loca_temp)
}
