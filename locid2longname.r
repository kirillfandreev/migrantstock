# LocIDs into LongName or niso3locid
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

# # Long Name from rftLocaiton table niso3locid
# v <- rftLocation[rftLocation$LocID == LocID,]
# if(nrow(v) == 1){
#     v2 <- v
# }else{
#     if(nrow(v) > 1){
#         v1 <- v[v$Preferred == 1  & (!is.na(v$Preferred)),]
#         if(nrow(v1) == 1){
#             v2 <- v1
#         }else{
#             v2 <- v1[1,]
#         }
#     }
# }
# 
# niso3locid <- paste0(v2$LongName, "_", v2$ISO3, "_", LocID)
