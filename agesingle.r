# Converts arbitrary structure into single age groups 
# Applies linear interpolation assuming uniform distribution within age groups. 
# Future enhancements may incorporate pchip() or other spline-based methods. 
# The open age group distributed with a quadratic model fitted to log population of the world from WPP.
# The current approach is highly robust and handles arbitrary input reliably.
#
# Revision history: 
# 
# 2024/6
#   First version.
#   Kirill Andreev (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362) (KA)

agesingle <- function (df) {
    
###browser()

df1 <- NULL
n <- nrow(df)
for(i in 1:n){
    dfi <- df[i,]
    
    if(i < n){
        
        if(dfi$agelength > 1){
            # distribute evenly 
            dftmp <- do.call("rbind", replicate(dfi$agelength, dfi, simplify = FALSE))
            dftmp$value <- dfi$value / dfi$agelength
            dftmp$age <- dfi$age:(dfi$age + dfi$agelength -1)
            dftmp$agelength <- 1
            dfi <- dftmp
        }
        
    }else{
        # pop_rate_decline.m
        # open age group
        # rate of decline age 50
        b = c(-1.010539453418424, 0.032336797728432, -2.656482429040968e-04);
        ageopen <- dfi$age
        agemin <- 50;
        agemax <- 150;
        if(ageopen >= agemax){
            stop("cannot distribute open age")
        }
        if(ageopen < agemin){
            stop("cannot distribute open age")
        }

        ###browser()
        ages  <- ageopen:agemax
        px    <- rep(0, length(ages) + 1)
        px[1] <- 1000
        idx <- 1
        for(a in ages){
            rx <- b[1] + b[2] * a + b[3] * a * a
            px[idx + 1] <- px[idx] * exp(rx)
            idx <- idx + 1
        }
        
		# !!! this is a mistake here ned dx?
		
        # distribute and round
        px <- px / sum(px) * dfi$value
        px <- round(px);
        # cut tail
        idx <- min(which(px == 0))
        if(idx > 1){
            px <- px[1:idx]
        }else{
            px <- px[1]
        }
        px[1] <- px[1] + sum(px) - dfi$value # rounding correction 
        ages <- ageopen:(ageopen + length(px) - 1)

        if(any(px < 0) || any(!is.finite(px))){        
            stop("distribution of open age failed")
        }
        
        n <- length(px)
        dftmp <- do.call("rbind", replicate(n, dfi, simplify = FALSE))
        dftmp$value     <- px
        dftmp$age       <- ages
        dftmp$agelength <- 1
        dfi <- dftmp
        ### browser()
    }
    
    df1 <- rbind(df1, dfi)
}

return(df1)

} # end of function
