# Extracts/produces regular age structure
# Description:
#    Given a data by age groups, the procedure produces a regular age partition by 
#   a) separating grand total, subtotals and unknown age groups (dsext);
#   b) filling gaps between age groups to make the age scale continuous (dsreg).
#
#   Age groups are not necessarily must be integer.  If deaths, for example, 
#   are available by Lexis triangles one can assign value of 0.5 to length 
#   of the age groups and apply this procedure.
# 
# Parameters:
#   df – dataset with columns 
#       df$age
#       df$agelength
#       df$value
# Returns:
#   dfage$dfreg - dataset with regular age groups
#   dfage$dfext - extra age groups: grand total, subtotals, and unknown age groups.

# Revision history: 
# 
# 2023/10/13
#   First version created. 
#   Kirill Andreev (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)

ageregular <- function(df, missvalue = NaN) {

# browser()
    
# get standard columns
df <- df[, c("age", "agelength", "value")]
    
# check age agelength value dataset
if(!all(sort(names(df)) == c("age", "agelength", "value"))){
    stop("ageregular(): age,agelength,value dataset expected")
}

dfage <- NULL
dfage$dfreg <- NULL
dfage$dfext <- NULL
    
# extract total age group 
e <- df$age == 0 & df$agelength == 0
dftot <- df[e,]
df <- df[!e,]
if(nrow(df) == 0){
    dfage$dfext <- dftot
    return(dfage)
}

# unknown age group 
e <- df$age == -1 & df$agelength == 0
dfunk <- df[e,]
df <- df[!e,]
if(nrow(df) == 0){
    dfage$dfext <- rbind(dftot, dfunk)
    return(dfage)
}


# recode open age groups (agelength = 0) as +infinity so they appear the last among all age groups starting with the same age

# sort by age and agelength 
e <- df$agelength  == 0
df$agelength[e] <- +Inf
df  <- df[order(df$age, df$agelength),] # sort

if(any(df$age < 0 | df$agelength < 0)){
    stop("negative age or/and agelength present")
}

n <- nrow(df)
i <- 1
while (i <= n) {

    # advance j to the start of the next age group or to the end of the age vector
    for (j in i:n) {
        if(df$age[j] != df$age[i]){
            break
        }
    }
    
    if(df$age[j] != df$age[i]){
        
        dfins <- NULL
        
        # Next age group exists, age[j]
        #
        # Among several age groups with the same starting age, age[i], but different lengths,
        # select the age group which after adding agelength fits exactly the starting age of the next age group 
        # For example, among 
        #   0 1
        #   0 5
        #   select 0 1 if the next age starts with 1, e.g. 1 4

        isel  <-  0;
        for (k in i:(j-1)) {
            if(abs(df$age[k]  +  df$agelength[k] - df$age[j]) <= (.Machine$double.eps * 10)){
                isel  <-  k;
                break
            }
        }
        
        if(isel == 0){
            
            # browser()
            
            # No current age group is found that fits start of next age group: the current age group(s) maybe too long or too short
            # First try: select age group that covers the largest part of the interval to the next age group and add missing interval.
            # Assume that there is an age group that is missing in the age structure.
            
            # compute length of the "missing" age group 
            v <- rep(0, j - i)  # zero place holder 
            for (k in i:(j-1)) {
                v[k - i + 1] <- df$age[j] - df$age[k] - df$agelength[k];
            }
            
            v[v < 0] <- +Inf
            
            minv <- min(v)
            if(is.finite(minv)){
                # found an age group which is shorter than beginning of the next one
                k <- which(minv == v)
                isel <- k + i - 1
                
                dfins <- data.frame (
                    age       = df$age[isel] + df$agelength[isel],
                    agelength = df$age[j] - (df$age[isel] + df$agelength[isel]),
                    value     = missvalue
                )
            } else {

                # print("inserting age group not implemented")
                # browser()

                # All current age groups at index i are longer than beginning of the next age group at index j.
                # For example, for Bulgaria (LocID=100), Eurostat reports in 2011 for age zero:
                #     age	agelength	    value
                #       0	        0	    78,787
                #       0	        2	    411
                #       1	        4	    2,974
                #       5	        5	    3,334
                #       10	        5	    2,179
                # 
                # LocID sex yearref LocIDorg PopulationUniverse   source
                # 1   100   0    2011        0                  0 Eurostat
                # This is an obvious mistake.  Age length must 1 not 2.  
                # 
                # Such cases handled in this block by inserting an age group with missing value of correct length 
                # 0 1 NaN and moving all current age groups into dataset with extra age groups 
                
                dfins <- data.frame (
                    age       = df$age[i],
                    agelength = df$age[j] - df$age[i],
                    value     = missvalue
                )
                dfage$dfreg <- rbind(dfage$dfreg, dfins)
                
                k <- i:(j-1)
                dfage$dfext <- rbind(dfage$dfext, df[k, ])
                
                # skip the rest
                
                # advance index i to the next age group
                i <- j;
                
                next

            }

            # print("inserting age group not implemented")
            # browser()
            
        }

        if(isel == 0){
            stop("isel is still zero -- not implemeneted!!!")
        }
        
        # We have valid isel  -- index of an age group for regular age structure 
        # Add the current age group to the dataset with the regular age groups
        dfage$dfreg <- rbind(dfage$dfreg, df[isel, ])
        
        if(!is.null(dfins)){
            # add inserted (missing) age group
            dfage$dfreg <- rbind(dfage$dfreg, dfins)
        }
        
        # extract and collect irregular age groups
        k <- setdiff(i:(j-1), isel)
        if(length(k) > 0){
            dfage$dfext <- rbind(dfage$dfext, df[k, ])
        }

    }else{
        
        # no next age group exists: select the age group with the longest interval
        dfage$dfreg <- rbind(dfage$dfreg, df[n, ])
        if(i < n){
            k <- i:(n-1)
            dfage$dfext <- rbind(dfage$dfext, df[k, ])
        }
        
        break
        
        # print("# no next age group exists: select the age group with the longest interval -- not implemented")
        # browser()
        
    }# next age group exists 
    
    
    # advance index i to the next age group
    i <- j;
    
} # i-loop over all age groups


# browser()
# 
# print(df)
# 
# stop("ageregular")

# recode +Inf as 0
if(!is.null(dfage$dfreg)){
    e <- dfage$dfreg$agelength == Inf
    dfage$dfreg$agelength[e] <- 0
}

if(!is.null(dfage$dfext)){
    e <- dfage$dfext$agelength == Inf
    dfage$dfext$agelength[e] <- 0
}

if(nrow(dfunk) > 0){
    dfage$dfext <- rbind(dfage$dfext, dfunk)
}

if(nrow(dftot) > 0){
    dfage$dfext <- rbind(dfage$dfext, dftot)
}

return(dfage)
}
