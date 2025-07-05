# Converts country numeric code (LocID) into long name or niso3locid (combination of the long name, iso3 code, and locid)
# See rftLocation.xlsx for the format.
# Parameters 
#   LocIDs - a vector of numeric country identifiers 
#   niso3locid - true to return niso3locid instead of just a long name 
#
# See also
#    longname2locid.r
#
# Revision history: 
# 
# 2024/6
#   First version created. 
#   Kirill Andreev (KA) (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)

locid2longname <- function(LocIDs, niso3locid = FALSE) {

# browser()
    
rftLocation <- msglobalsf("rftLocation") # local version of the table. use mapcompiler script to update
n <- length(LocIDs)
LongNames <- rep("", n)
for(i in 1:n){

    LocID <- LocIDs[i]
    
    v <- rftLocation[rftLocation$LocID == LocID,]
    if(nrow(v) == 1){
        LongNames[i] <- v$LongName
        next
    }
    
    # multiple rows
    v1 <- v[(v$Preferred == 1) & (!is.na(v$Preferred)), ]
    # dfpref <- df[(df$Preferred == 1) & (!is.na(df$Preferred)), ] # preferred names

    if(nrow(v1) == 1){
        if(niso3locid){
            LongNames[i] <- paste0(v1$LongName, "_", v1$ISO3, "_", LocID)
        }else{
            LongNames[i] <- v1$LongName
        }
        next
    }
    
    msg <- paste0("locid2longname(): no preferred long name found: LocID = ", LocID)
    print(msg)

    if(niso3locid){
        LongNames[i] <- paste0(v1$LongName[1], "_", v1$ISO3[1], "_", LocID)
    }else{
        LongNames[i] <- v1$LongName[1]
    }
    
#     LongNames[i] <- v1$LongName[1]

} # loop of LocIDs

return(LongNames)
}
