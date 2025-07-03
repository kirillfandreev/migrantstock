# Functions for data loading 
#
# Revision history: 
# 
# 2020/10/17
# First version created. 
# Tested on the data for Australia (Trend in total migrant stocks in Australia is significantly affected by COVID-19).
# Kirill Andreev (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)

# library("tidyverse")
# source("msglobalsf.r", local = TRUE)

# Loads data from a .RData file
# Examples
#   Returns "df" variable / object
#     df <- msload(fname, "df")
#   Returns a single variable / object or list of variables
#     df <- msload(fname)
#     class(df)
#
#   See save_and_load.r for testing
#
# Revision history: 
#   2022/11/4
#   First version. 
#   Kirill Andreev (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)


msload <- function(fname = NULL, varname = NULL) {
    
    # assign a new value if provided 
    if (is.null(fname)) {
        # Load from a default saved file 
        # not implemented
        return(NULL)
    }
    
    # create a temporary environment
    tmpEnvironment__ <- new.env(parent = emptyenv())
    # objs <- ls(envir = tmpEnvironment__)
    # length(objs)
    
    # load all objects into the temporary environment
    load(file = fname, envir = tmpEnvironment__)
    objs <- ls(envir = tmpEnvironment__)
    print(objs)  # available vars
    length(objs)
    
    # df <- get("df", envir = tmpEnvironment__, inherits = FALSE)
    # 
    # var1 <- get(objs[1], envir = tmpEnvironment__, inherits = FALSE)
    # var2 <- get(objs[2], envir = tmpEnvironment__, inherits = FALSE)
    
    
    if (!is.null(varname)) {
        
        # return requested variable 
        
        rs <- get(varname, envir = tmpEnvironment__, inherits = FALSE)
        
        # Do I need to delete???
        rm(tmpEnvironment__)  # delete temporary environment
        
        return(rs)
        
    }
    
    # if not variable name specified, return all variables (objects) in the file in a named list
    
    # !!!HERE "Fri Nov  4 11:45:42 2022"
    
    # if(length(objs) == 1) {
    #   # single object
    #   varname <- objs[1]
    # }else{
    #   stop("Return of all objects is not implemented")  
    # }
    
    # load all variables in a list
    
    rs <- list()
    for (i in 1:length(objs))
    {
        rs[[i]] <- get(objs[i], envir = tmpEnvironment__, inherits = FALSE)
    }
    
    if(length(objs) == 1) {
        # If there is a single object e.g. data frame return it instead of list 
        rs <- rs[[1]]
        rm(tmpEnvironment__)  # delete temporary environment
        return(rs)
    }
    
    # If there are multiple objects, return a named list with them 
    names(rs) <- objs
    rm(tmpEnvironment__)  # delete temporary environment
    return(rs)
    
}

