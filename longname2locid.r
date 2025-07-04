# LongName to LocIDs
longname2locid <- function(LongNames) {
rftLocation <- msglobalsf("rftLocation") # local version of the table. use mapcompiler script to update
LocIDs <- NULL
for(i in 1:length(LongNames)){
    rftLocationi <- rftLocation[rftLocation$LongName == LongNames[i],]
    if(!all(rftLocationi$LocID[1] == rftLocationi$LocID)){stop("longname2locid(): LocID err")}
    LocIDs <- c(LocIDs, rftLocationi$LocID[1])
}
return(LocIDs)
}
