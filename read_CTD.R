read_CTD <- function(file_in_C) {

filenc_C=nc_open(file_in_C,readunlim=FALSE,write=FALSE)

PSAL=ncvar_get(filenc_C,"PSAL")
TEMP=ncvar_get(filenc_C,"TEMP")
PRES=ncvar_get(filenc_C,"PRES")

PSAL_QC=ncvar_get(filenc_C,"PSAL_QC")
TEMP_QC=ncvar_get(filenc_C,"TEMP_QC")
PRES_QC=ncvar_get(filenc_C,"PRES_QC")

#### CTD is always the 1st profile of the nc

PSAL_CTD=PSAL[,1]
TEMP_CTD=TEMP[,1]
PRES_CTD=PRES[,1]

nc_close(filenc_C)

##### Return
result=(list(PRES=PRES_CTD,PSAL=PSAL_CTD,TEMP=TEMP_CTD))

return(result)
}

