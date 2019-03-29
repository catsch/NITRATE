library(ncdf4)
library(stringr)

source("./read_BFILE_NITRATE.R")
source("./read_META_NITRATE.R")
source("./read_CTD.R")
source("./QC_NITRATE.R")
source("./RunningFilter.R")

#### threshold #### 
# values of the numerical counts for which the spectrophotmeter saturates
saturation=65535

###########################################################
#### Reading the metadatafile 
###########################################################

### Reading the metadata file name 
liste_meta=read.table("liste_meta", header=FALSE, as.is=TRUE)

#### Open the metadata file 
filenc_meta=nc_open(liste_meta$V1,readunlim=FALSE)

#### Read the metadata file

META=read_META_NITRATE(filenc_meta)

### Get the calibration 

N_wl <- length(which((217 <= META$OPTICAL_WAVELENGTH_UV) &(META$OPTICAL_WAVELENGTH_UV <= 240)))

istart <- which(META$OPTICAL_WAVELENGTH_UV >= 217)[1]

lambda <- META$OPTICAL_WAVELENGTH_UV[istart:(istart+N_wl-1)]

Iref <- META$UV_INTENSITY_REF_NITRATE[istart:(istart+N_wl-1)]

ENO3 <- META$E_NITRATE[istart:(istart+N_wl-1)]

ESW <- META$E_SWA_NITRATE[istart:(istart+N_wl-1)]

Tcal <- META$TEMP_CAL_NITRATE

wl <- META$WL

## Sakamoto Coefficients
 
AAA <- META$AAA

BBB <- META$BBB

CCC <- META$CCC

DDD <- META$DDD

###########################################################
#### End Reading the metafile name
###########################################################

#### Creating the list of files for which we need to recompute  
liste_to_do=read.table("liste_all_B",header=FALSE, as.is=TRUE)

# List of the file to process
LIST_nc=liste_to_do$V1

for (IDnc in LIST_nc) {

##########################
#### Reading the Bfile
##########################

filenc_B=nc_open(IDnc,readunlim=FALSE,write=TRUE)

##############################################################################
#### Check that there is NITRATE in the file if not, go to the next file
##############################################################################

#### Get the list of parameters in the profile
STATION_PARAMETERS=ncvar_get(filenc_B,"STATION_PARAMETERS")

# Stations parameters has a fixed length 64 characters 
NITRATE_STRING=str_pad("NITRATE",64,"right")

# Find the profile containing NITRATE 
index_nitrate=which(STATION_PARAMETERS == NITRATE_STRING, arr.ind=TRUE)

if ( length(index_nitrate)==0 ) {
    next
}

#### Read the NITRATE Variables necessary for the computation 

NITRATE=read_BFILE_NITRATE(filenc_B,index_nitrate)

#######################################
## Initialize working variables
#######################################

NITRATE_TCSS = rep(NA, length(NITRATE$PRES)) # NITRATE ESTIMATION with Multiple regression 

RMS_NITRATE_TCSS = rep(NA, length(NITRATE$PRES)) # RMS of the NITRATE ESTIMATION

ASW240_NITRATE_TCSS = rep(NA, length(NITRATE$PRES)) # Absorbance a 240nm 

SATURATION_NITRATE = rep(FALSE, length(NITRATE$PRES)) # Saturation

##################################################
#### Get CTD Data from the Netcdf C File
##################################################
#### Catherine Warning D MODE !!!

file_in_C=str_replace(IDnc,"BR","R")

print(file_in_C)

CTD=read_CTD(file_in_C)

# we get        : CTD$PRES
#               : CTD$PSAL
#               : CTD$TEMP
 
# We interpolate CTD DATA to get TEMP and PSAL a NITRATE$PRES LEVEL

if ( length(which(!is.na(CTD$TEMP)))>2 ) {
   
TEMP_NITRATE <- approx(CTD$PRES, CTD$TEMP, NITRATE$PRES+1.5, rule=2)$y

PSAL_NITRATE  <- approx(CTD$PRES, CTD$PSAL, NITRATE$PRES+1.5, rule=2)$y

} else {

next

}
#####################################################
# NITRATE COMPUTATION 
#####################################################

Ndepth=length(NITRATE$PRES[!is.na(NITRATE$PRES)])

for(p in 1:Ndepth){

	temp=TEMP_NITRATE[p]

	sal=PSAL_NITRATE[p]

	pres=NITRATE$PRES[p]

	I=NITRATE$UV_INTENSITY_NITRATE[1:N_wl , p] 
	
	Idark=NITRATE$UV_INTENSITY_DARK_NITRATE[p]

	A = -log10((I - Idark)/Iref)
	
	ASWTcal = (AAA + BBB*Tcal)*exp((CCC+DDD*Tcal)*(lambda-wl))
	
	ASWTis = (AAA + BBB*temp)*exp((CCC+DDD*temp)*(lambda-wl))
	
	ESWTis = (ESW*ASWTis)/ASWTcal

	ASW = ESWTis*sal

	# CORRECTION PRESSION (1% BY 1000m ORENS + KEN)
	ASW = ASW * (1-(0.026*pres) / 1000 )
	
	Aprim = A - ASW

	### Multiple regression 

	lm3 = NA
	
	lm3$coefficients = NA
	
	lm3$coefficients[2] = NA
	
	try(lm3 <- lm(Aprim~ENO3+lambda))
	
	NITRATE_TCSS[p] = lm3$coefficients[2] #Computed Value to check that it is correct with the actual value in the file

	NITRATE[p] = sqrt( mean( (Aprim-fitted(lm3))^2) ) #Quality of the fit

	ASW240_NITRATE_TCSS[p]=ASW[N_wl] # absorbance at 240nm

	if( length(which( I >= saturation )) > 0 ) SATURATION_NITRATE[p]=TRUE # One channel saturated 
	
}

QC=QC_NITRATE(filenc_B,index_nitrate,NITRATE_TCSS,RMS_NITRATE_TCSS,ASW240_NITRATE_TCSS,SATURATION_NITRATE)

nc_close(filenc_B)

}
