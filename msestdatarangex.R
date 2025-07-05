# Estimation for the data range of empirical data 
#
# Description:
#   
# Output
#   HTML reports e.g.
#
# Revision history: 
# 
# 2024/10/15
#   First version created. 
#   Yu Han (linkedin.com/in/dr-eng-yu-han-85353176)
#   Kirill Andreev (KA) (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)

msestdatarangex <- function(df, yfits) {
  # Data frame with empirical data:
  #   yfts     vfts PopulationUniverse
  #   1  1991.000 453908.0                -34
  #   2  1991.167 453908.0                -34
  #   3  2001.197 492366.6                -34
  #   4  2009.000 384161.0                -34
  #   5  2010.000 398493.0                -34
  #   6  2011.191 643524.2                -34
  #   7  2012.000 390843.0                -34
  #   8  2013.000 387337.0                -34
  #   9  2014.000 396156.0                -34
  #   10 2015.000 416454.0                -34
  #   11 2016.000 433290.0                -34
  # 
  # Years for which we produce estimates
  # yfts
  #   1991 1992 1993 1994 ... 2021 2022
  
  # browser()
  yfit <- df$yearref                       
  vfit <- df$value
  yfts <- yfits
  
  if(any(vfit == 0)){
    
    # we cannot use log population with zeros, use just population interpolation instead
    if(length(yfit) == 1){
      
      # a single empirical data point
      vfts <- vfit
      
    }else
      if(length(yfit) == 2){
        
        # two data points pchip() fails if two data points provide (unlike matlab) with the error message:
        # Error in c[idx, ] : subscript out of bounds
        # We switch to linear interpolation in this case 
        
        methodname <- "linear interpolation of population"
        vfts <- approx(x = yfit, y = vfit, xout = yfts)
        vfts <- vfts$y
        
      }else{
        
        # methodname <- "cubic spline interpolation of population"
        # func <- splinefun(x = yfit, y = vfit, method="fmm",  ties = mean)
        # vfts <- func(yfts)
        methodname <- "pchip interpolation of population"
        
        vfts <- pchip(yfit, vfit, yfts)    
        
        # if (length(yfts) == length(yfit) && all(yfts == yfit)){
        #     # There is a bug in pchip() – if x-values to be interpolated are exactly the same as input x-values, phcip() fails.  This code is a quick  fix. (KA)
        #     vfts <- vfit
        # }else{
        #     vfts <- pchip(yfit, vfit, yfts)    
        #     
        #     # fp <- pchip(yfit, vfit, NULL)    
        # }
        
        if(any(is.nan(vfts)) | any(is.na(vfts)) | any(is.null(vfts)) | any(vfts < 0)){
          
          # pchip() could produce negative values. If it is the case, run linear interpolation.
          
          fh <- file(msglobalsf("LOGFILE"), open = "at")
          msg <- paste0("warning: pchip() interpolation failed.  Switched to a linear interpolation.")
          cat(msg, "\n", append = T, file = fh)
          close(fh)
          
          methodname <- "linear interpolation of population"
          vfts <- approx(x = yfit, y = vfit, xout = yfts)
          vfts <- vfts$y
        }
        
        #!!! HERE some negative values in interpolation 
        # plot(yfit, vfit)
        # plot(yfts, v)
        
        # x <- c(1, 2, 3, 4, 5, 6)
        # y <- c(16, 18, 21, 17, 15, 12)
        # pchip(x, y, seq(1, 6, by = 0.5))
        # fp <- pchipfun(x, y)
        # fp(seq(1, 6, by = 0.5))
        
      }
    
    dfo <- data.frame(yearref = yfts, value = vfts)
    return(dfo)
    # return(vfts)
  } # end of case with zeros in values 
  
  # no zeros in values
  vfit_log <- log(vfit)
  
  if(length(yfit) == 1){
    # a single empirical data point
    vfts <- vfit_log
  }else{
    # Constant growth rate
    # Linear interpolation will produce constant growth rate between two data points if used for log population 
    vfts <- approx(x = yfit, y = vfit_log, xout = yfts)
    vfts <- vfts$y
  }
  
  # Cubic spline will be more smooth 
  # pchip() will be probably better, more smooth than linear and less wiggly than spline
  
  # Cubic spline for log(population)
  # methodname <- "cubic spline interpolation of log population"
  # func <- splinefun(x = yfit, y = vfit_log, method="fmm",  ties = mean)
  # vfts <- func(yfts)
  
  vfts <- exp(vfts)
  
  dfo <- data.frame(yearref = yfits, value = vfts)
  dfo <- dfo[complete.cases(dfo), ]
  return(dfo)
}
