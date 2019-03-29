QC_NITRATE <-function(filenc_B,index_nitrate,NITRATE_TCSS,RMS_NITRATE_TCSS,ASW240_NITRATE_TCSS,SATURATION_NITRATE) {

### CONSTANTS 
MIN_RANGE=-2

MAX_RANGE=50

SPIKE_VALUE=1

FIT_RANGE=0.003

ABS240_MED=0.8

ABS240_MAX=1.1

# Profile index of the NITRATE 
i_prof_nitrate=index_nitrate[,2]

# param index of the NITRATE
i_param_nitrate=index_nitrate[,1]

# Read the variables in the file

PRES=ncvar_get(filenc_B,"PRES")

NITRATE=ncvar_get(filenc_B,"NITRATE")

NITRATE_ADJUSTED=ncvar_get(filenc_B,"NITRATE_ADJUSTED")

NITRATE_QC=ncvar_get(filenc_B,"NITRATE_QC")

NITRATE_ADJUSTED_QC=ncvar_get(filenc_B,"NITRATE_ADJUSTED_QC")

### PRESSURE FOR NITRATE

PRES_NITRATE=PRES[,i_prof_nitrate]

NITRATE=NITRATE[,i_prof_nitrate]

NITRATE_ADJUSTED=NITRATE_ADJUSTED[,i_prof_nitrate]

###########################################################
# QC tests ################################################
###########################################################

#### DEFINE QC

NITRATE_ADJUSTED_QC_value = rep(" ", length(PRES_NITRATE) )

NITRATE_QC_value = rep(" ", length(PRES_NITRATE) )

#### Number of measurements 

Ndepth=length(PRES_NITRATE[!is.na(PRES_NITRATE)])

#### Filter the data with a median 
if (length(which(!is.na(NITRATE))) >= 5 ) {

MED_NITRATE=RunningFilter(2,NITRATE,na.fill=T, ends.fill=T, Method="Median")

MED_NITRATE_ADJUSTED=RunningFilter(2,NITRATE_ADJUSTED,na.fill=T, ends.fill=T, Method="Median")

} else {

MED_NITRATE=NITRATE

MED_NITRATE_ADJUSTED=NITRATE_ADJUSTED

}

for(p in 1:Ndepth){

	if (is.na(NITRATE[p])) {
	
		NITRATE_QC_value[p]=4

	} else {

#### Initialise

		NITRATE_QC_value[p]=3

### Saturation

		if(SATURATION_NITRATE[p]) { 

			NITRATE_QC_value[p]=3

		}

### Absorbance

		if ( (ASW240_NITRATE_TCSS[p] < ABS240_MAX ) & ( ASW240_NITRATE_TCSS[p] >= ABS240_MED ) )  {

			NITRATE_QC_value[p]=3

		}
	
		if ( ASW240_NITRATE_TCSS[p] >= ABS240_MAX )   {

			NITRATE_QC_value[p]=4

		}

### Fit TEST 

		if ( RMS_NITRATE_TCSS[p] >= FIT_RANGE )   {

			NITRATE_QC_value[p]=4

		}

### Range Test 

		if ( (NITRATE[p] < MIN_RANGE) | (NITRATE[p] > MAX_RANGE) )  NITRATE_QC_value[p]=4

### Spike test

		if ( abs(MED_NITRATE[p]-NITRATE[p]) >= SPIKE_VALUE ) NITRATE_QC_value[p]=4

	}

	if (is.na(NITRATE_ADJUSTED[p])) {
	
		NITRATE_ADJUSTED_QC_value[p] =4

	} else {

#### Initialise

		NITRATE_ADJUSTED_QC_value[p] =2

### Saturation

		if(SATURATION_NITRATE[p]) { 

			NITRATE_ADJUSTED_QC_value[p]=3

		}

### Absorbance

		if ( (ASW240_NITRATE_TCSS[p] < ABS240_MAX ) & ( ASW240_NITRATE_TCSS[p] >= ABS240_MED ) )  {
	
			NITRATE_ADJUSTED_QC_value[p]=3

		}
	
		if ( ASW240_NITRATE_TCSS[p] >= ABS240_MAX )   {
	
			NITRATE_ADJUSTED_QC_value[p]=4

		}

### Fit TEST 

		if ( RMS_NITRATE_TCSS[p] >= FIT_RANGE )   {
	
			NITRATE_ADJUSTED_QC_value[p]=4

		}

### Range Test 

		if ( (NITRATE_ADJUSTED[p] < MIN_RANGE) | (NITRATE_ADJUSTED[p] > MAX_RANGE) )  NITRATE_ADJUSTED_QC_value[p]=4

### Spike test

		if ( abs(MED_NITRATE_ADJUSTED[p]-NITRATE_ADJUSTED[p]) >= SPIKE_VALUE ) NITRATE_ADJUSTED_QC_value[p]=4

	}

}

### Write the QC in the file 

NITRATE_QC[i_prof_nitrate]=paste(NITRATE_QC_value,collapse="")

NITRATE_ADJUSTED_QC[i_prof_nitrate]=paste(NITRATE_ADJUSTED_QC_value,collapse="")

ncvar_put(filenc_B,"NITRATE_QC",NITRATE_QC)

ncvar_put(filenc_B,"NITRATE_ADJUSTED_QC",NITRATE_ADJUSTED_QC)

QC="Done and Write"

return(QC)

}

