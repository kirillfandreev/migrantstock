# Converts country long name to country numeric code (LocID)
# See rftLocation.xlsx for the format.
# Parameters 
#   LongNames - a list of country names
#
# See also
#    locid2longname.r
#
# Revision history: 
# 
# 2024/6
#   First version created. 
#   Kirill Andreev (KA) (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)


longname2locid <- function(LongNames) {

    rftLocation <- msglobalsf("rftLocation") # local version of the table. use mapcompiler script to update rdata file.

    LocIDs <- NULL
    
    for(i in 1:length(LongNames)){
        rftLocationi <- rftLocation[rftLocation$LongName == LongNames[i],]
        if(!all(rftLocationi$LocID[1] == rftLocationi$LocID)){stop("longname2locid(): LocID err")}
        LocIDs <- c(LocIDs, rftLocationi$LocID[1])
    }
    
    return(LocIDs)
}

