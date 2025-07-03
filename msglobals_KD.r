msglobals_KD <- function() {

    ret <- NULL
    ret$UserName <- "KA"
    ret$SHAREPOINTFOLDER     <- "C:/akf/DESA-POP-MUS/MS/"
    # msglobals[["EMPIRICALFOLDERALL"]]   <- "C:/akf/MUS/MS/Empiricalall/"
    # msglobals[["EMPIRICALFOLDER"]]      <- "C:/akf/MUS/MS/Empirical/"
    ret$EMPIRICALFOLDERALL      <- "C:/akf/DESA-POP-MUS/MS/processing/empiricalall/"
    ret$EMPIRICALFOLDER         <- "C:/akf/DESA-POP-MUS/MS/processing/empirical/"
    ret$PROJECTFOLDER           <- "C:/akf/mus/MS/scripts/migrantstock/"
    
    ret$LocIDsExcluded          <- NULL
    
    # ret$LocIDsSelected        <- 484  # Mexico
    # ret$LocIDsSelected        <- 214
    
    # ret$LocIDsSelected        <- c(484, 840)
    
    # ret$LocIDsSelected        <- c(124, 208, 32, 356, 36, 392, 458, 484, 554, 702, 710, 716, 784, 792, 826, 840)  # ready for modelling
    # ret$LocIDsExcluded          <- c(124, 208, 32, 356, 36, 392, 458, 484, 554, 702, 710, 716, 784, 792, 826, 840)

    # ret$LocIDsSelected          <- 4
    # ret$LocIDsSelected          <- 158  # Taiwan
    # ret$LocIDsSelected          <- 728  # South Sudan

    # ret$LocIDsSelected        <- c(535)  # Caribbean Netherlands_BES_535
    # ret$LocIDsSelected        <- c(348)  # Hungary_HUN_348
    # ret$LocIDsSelected        <- c(840) # United States
    # ret$LocIDsSelected        <- 792    # Turkey_TUR_792
    # ret$LocIDsSelected        <- 524    # Nepal
    # ret$LocIDsSelected        <- c(524)    # Nepal
    # ret$LocIDsSelected        <- 222
    # ret$LocIDsSelected        <- 524
    # ret$LocIDsSelected        <- 170  # Colombia
    
    ret$LocIDsSelected        <- 682  # Saudi Arabia_SAU_682
    # ret$LocIDsSelected          <- 840  # U.S.
    ret$LocIDsSelected        <- 152;      #  Chile
    # ret$LocIDsSelected        <- 32

    # ret$LocIDsSelected          <- "all"
    # ret$LocIDsSelected          <- "analyst"
        
    return(ret)
    
    # # create folders 
    # if (!file.exists(msglobals[["PROJECTOUTPUTFOLDER"]])){
    #     dir.create(msglobals[["PROJECTOUTPUTFOLDER"]], showWarnings = FALSE)
    # }

}
