# global variables for user MunSim.Lai, UN notebook
msglobals_NL <- function() {
    ret <- NULL
    ret$LocIDsExcluded <- NULL
    
    ret$SHAREPOINTFOLDER    <- "C:/akf/DESA-POP-MUS/"
    ret$EMPIRICALFOLDERALL  <- "C:/akf/DESA-POP-MUS/processing/empiricalall/"
    ret$EMPIRICALFOLDER     <- "C:/akf/DESA-POP-MUS/processing/empirical/"
    ret$PROJECTFOLDER       <- "C:/akf/mus/MS/scripts/migrantstock/"
    
    
    # ret$LocIDsExcluded          <- c(124, 208, 32, 356, 36, 392, 458, 484, 554, 702, 710, 716, 784, 792, 826, 840)
    
    # ret$LocIDsSelected          <- 484  # Mexico
    # ret$LocIDsSelected      <- c(124, 208, 32, 356, 36, 392, 458, 484, 554, 702, 710, 716, 784, 792, 826, 84
    ret$LocIDsSelected <-384 #!!! change your selected country(s) here
    #  ret$LocIDsSelected          <- "all"
    #  ret$LocIDsSelected          <- "analyst"
    
	ret$UserName <- "NL"
	
    return(ret)
}
