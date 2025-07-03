# global variables for user Kirill.Andreev UN notebook
msglobals_KU <- function() {
    ret <- NULL
    ret$LocIDsExcluded <- NULL
    
    ret$SHAREPOINTFOLDER    <- "C:/akf/DESA-POP-MUS/MS/"
    ret$EMPIRICALFOLDERALL  <- "C:/akf/DESA-POP-MUS/MS/processing/empiricalall/"
    ret$EMPIRICALFOLDER     <- "C:/akf/DESA-POP-MUS/MS/processing/empirical/"
    ret$PROJECTFOLDER       <- "C:/akf/mus/MS/scripts/migrantstock/"
    
    # ret$LocIDsExcluded          <- c(124, 208, 32, 356, 36, 392, 458, 484, 554, 702, 710, 716, 784, 792, 826, 840)
    # ret$LocIDsSelected          <- 484  # Mexico
    # ret$LocIDsSelected          <- c(124, 208, 32, 356, 36, 392, 458, 484, 554, 702, 710, 716, 784, 792, 826, 840)

    ret$LocIDsSelected <- 840      #!!! change your selected country(s) here
    #  ret$LocIDsSelected          <- "all"
    #  ret$LocIDsSelected          <- "analyst"
    
	ret$UserName <- "KU"
	
    return(ret)
}
