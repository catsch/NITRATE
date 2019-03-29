read_META_NITRATE <-function(filenc_meta) {

# Get the PARAMETER

PARAMETER=ncvar_get(filenc_meta,"PARAMETER")

# Get the predeployment calibration coefficient

PCC=ncvar_get(filenc_meta,"PREDEPLOYMENT_CALIB_COEFFICIENT")

# index nitrate string  

NITRATE_STRING=str_pad("NITRATE",64,"right")

# Find the profile containing NITRATE 

index_nitrate=which(PARAMETER == NITRATE_STRING, arr.ind=TRUE)

PCC_NITRATE=PCC[index_nitrate]

###############################################################################
### PARSING the CALIBRATION ###################################################
###############################################################################

## Could add a test on the DAC, the parse depends on the DAC

######################################################
# Coriolis PARSING
###################################################### 

# Split the PREDEPLOYMENT CALIB COEFFICIENT with ; into each part of the calibration 
NITRATE_PART=str_split(PCC_NITRATE,";")

#########################
## PART 1 / PIXEL 
#########################
# NITRATE_PART[[1]][1] 

#####################################
## PART 2 / UV_INTENSITY_REF_NITRATE
#####################################

index_uv_ref=str_detect(NITRATE_PART[[1]],"UV_INTENSITY_REF_NITRATE")

## Remove all character that prevent to interpret the array as a real 
UV_INTENSITY_REF_NITRATE=gsub("\\[|\\]","",gsub("[a-zA-Z()_= ]","",NITRATE_PART[[1]][index_uv_ref]))

## Cut the character list 
UV_INTENSITY_REF_NITRATE_CHAR=str_split(UV_INTENSITY_REF_NITRATE,",")

## Transform it into an array of numbers 
UV_INTENSITY_REF_NITRATE=as.numeric(unlist(UV_INTENSITY_REF_NITRATE_CHAR))

###################################
## PART 3 / SAKAMOTO COEFFICIENT 
###################################

index_wl_o=str_detect(NITRATE_PART[[1]],"OPTICAL_WAVELENGTH_OFFSET")

SAKAMOTO=str_split(NITRATE_PART[[1]][index_wl_o],",")

index_AAA=str_detect(SAKAMOTO[[1]],"A=")
index_BBB=str_detect(SAKAMOTO[[1]],"B=")
index_CCC=str_detect(SAKAMOTO[[1]],"C=")
index_DDD=str_detect(SAKAMOTO[[1]],"D=")
index_OPO=str_detect(SAKAMOTO[[1]],"OPTICAL_WAVELENGTH_OFFSET=")

AAA=as.numeric(str_replace(SAKAMOTO[[1]][index_AAA],"A=",""))
BBB=as.numeric(str_replace(SAKAMOTO[[1]][index_BBB],"B=",""))
CCC=as.numeric(str_replace(SAKAMOTO[[1]][index_CCC],"C=",""))
DDD=as.numeric(str_replace(SAKAMOTO[[1]][index_DDD],"D=",""))
wl=as.numeric(str_replace(SAKAMOTO[[1]][index_OPO],"OPTICAL_WAVELENGTH_OFFSET=",""))

########################################
## PART 4 / OPTICAL_WAVELENGTH_UV
########################################

index_wl=str_detect(NITRATE_PART[[1]],"OPTICAL_WAVELENGTH_UV")

## Remove all character that prevent to interpret the array as a real 
OPTICAL_WAVELENGTH_UV=gsub("\\[|\\]","",gsub("[a-zA-Z()_= ]","",NITRATE_PART[[1]][index_wl]))

## Cut the character list 
OPTICAL_WAVELENGTH_UV_CHAR=str_split(OPTICAL_WAVELENGTH_UV,",")

## Transform it into an array of numbers 
OPTICAL_WAVELENGTH_UV=as.numeric(unlist(OPTICAL_WAVELENGTH_UV_CHAR))

#############################
## PART 5 / TEMP_CAL_NITRATE
#############################

index_tcn=str_detect(NITRATE_PART[[1]],"TEMP_CAL_NITRATE")

TEMP_CAL_NITRATE=as.numeric(gsub("[a-zA-Z()_= ]","",NITRATE_PART[[1]][index_tcn]))

#############################
## PART 6 / E_SWA_NITRATE
#############################

index_esn=str_detect(NITRATE_PART[[1]],"E_SWA_NITRATE")

## Remove all character that prevent to interpret the array as a real 
E_SWA_NITRATE=gsub("\\[|\\]","",gsub("[a-zA-Z()_= ]","",NITRATE_PART[[1]][index_esn]))

## Cut the character list 
E_SWA_NITRATE_CHAR=str_split(E_SWA_NITRATE,",")

## Transform it into an array of numbers 
E_SWA_NITRATE=as.numeric(unlist(E_SWA_NITRATE_CHAR))

#############################
## PART 7 / E_SWA_NITRATE
#############################

index_en=str_detect(NITRATE_PART[[1]],"E_NITRATE")

## Remove all character that prevent to interpret the array as a real 
E_NITRATE=gsub("\\[|\\]","",gsub("[a-zA-Z()_= ]","",NITRATE_PART[[1]][index_en]))

## Cut the character list 
E_NITRATE_CHAR=str_split(E_NITRATE,",")

## Transform it into an array of numbers 
E_NITRATE=as.numeric(unlist(E_NITRATE_CHAR))

######################################################
# End of Coriolis PARSING
###################################################### 

META_NITRATE=list("UV_INTENSITY_REF_NITRATE"=UV_INTENSITY_REF_NITRATE,"AAA"=AAA,"BBB"=BBB,"CCC"=CCC,"DDD"=DDD,"WL"=wl,"OPTICAL_WAVELENGTH_UV"=OPTICAL_WAVELENGTH_UV,"TEMP_CAL_NITRATE"=TEMP_CAL_NITRATE,"E_SWA_NITRATE"=E_SWA_NITRATE,"E_NITRATE"=E_NITRATE)

return(META_NITRATE)

}


