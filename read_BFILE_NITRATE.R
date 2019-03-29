read_BFILE_NITRATE <-function(filenc_B,index_nitrate) {

# Profile index of the NITRATE 
i_prof_nitrate=index_nitrate[,2]

# param index of the NITRATE
i_param_nitrate=index_nitrate[,1]

# Read the variables in the file to recompute NITRATE

PRES=ncvar_get(filenc_B,"PRES")
        
UV_INTENSITY_DARK_NITRATE=ncvar_get(filenc_B,"UV_INTENSITY_DARK_NITRATE")

UV_INTENSITY_NITRATE=ncvar_get(filenc_B,"UV_INTENSITY_NITRATE") 
                                  
### PRESSURE FOR NITRATE
PRES_NITRATE=PRES[,i_prof_nitrate]

### NITRATE without NA
UV_INTENSITY_DARK_NITRATE=UV_INTENSITY_DARK_NITRATE[,i_prof_nitrate]

UV_INTENSITY_NITRATE=UV_INTENSITY_NITRATE[,,i_prof_nitrate]

PROF_NITRATE=list("PRES"=PRES_NITRATE,"UV_INTENSITY_DARK_NITRATE"=UV_INTENSITY_DARK_NITRATE,"UV_INTENSITY_NITRATE"=UV_INTENSITY_NITRATE)

return(PROF_NITRATE)

}

