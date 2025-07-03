msglobals_YH <- function() {
    
    ret <- NULL
    ret$UserName <- "YH"
    ret$SHAREPOINTFOLDER     <- "C:/akf/DESA-POP-MUS/MS/"
    ret$EMPIRICALFOLDERALL   <- "C:/akf/DESA-POP-MUS/MS/processing/empiricalall/"
    ret$EMPIRICALFOLDER      <- "C:/akf/DESA-POP-MUS/MS/processing/empirical/"
    ret$PROJECTFOLDER        <- "C:/akf/mus/MS/scripts/migrantstock/"
    ret$LocIDsExcluded       <- NULL
    
    # ret$LocIDsSelected          <- "all"
    # ret$LocIDsSelected          <- "analyst"
    
    ret$LocIDsSelected        <- 484  # Mexico
    
    ret$LocIDsSelected        <- c(124, 840)
    
    ret$LocIDsSelected        <- "all"
    
    
    return(ret)
    
}
