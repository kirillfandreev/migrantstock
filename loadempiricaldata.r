# Loads empirical data on migrant stocks 
# Parameters 
#   from 
#       NULL - from .\data folder for the migrantstock shiny application  
#       empirical    - empirical folder 
#       empiricalall - empiricalal folder
#
# Examples:
#   LocID <- 232
#   msglobalsf("LocID", LocID)
#   loadEmpiricalData()
#   dftot <- msglobalsf("EmpiricalDataTotal")
#   ...
#
#   dfemp <- loadEmpiricalData(from = "empirical", LocIDp = LocID)
#
# Revision history: 
# 
# 2020/10/17
#   First version created. 
#   Tested on the data for Australia (Trend in total migrant stocks in Australia is significantly affected by COVID-19).
#   Kirill Andreev (KA) (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)
#
# 2023/2/13
#   Several updates and loading of the data by country of origin (KA)
#
# 2023/5/25
#   Added "from" parameter (KA)

library("tidyverse")
source("msglobalsf.r", local = TRUE)
source("msload.r", local = TRUE)

loadEmpiricalData <- function(from = NULL, IncludeRevisions = FALSE, LocIDp = NULL) {
    
    ###browser() 
    
    # update global variables with data
    force(msglobalsf("EmpiricalData", NULL))
    force(msglobalsf("EmpiricalDataTotal", NULL))
    force(msglobalsf("EmpiricalDataTotalOrigin", NULL))
    force(msglobalsf("EmpiricalDataPopulationUniverse", NULL))
    force(msglobalsf("EmpiricalDataFileName", NULL))
    
    if(!is.null(LocIDp)){
        msglobalsf("LocID", LocIDp)         # set current country    
    }
    LocID <- msglobalsf("LocID")            # current country    
    
    if (is.null(LocID)){
        return(NULL)
    }
    
    if (is.na(LocID)){
        return(NULL)
    }

    df  <- NULL
    dfa <- NULL
    
    # browser()

    if(is.null(from)){stop("parameter \"from\" must be specified")}

    # file name with the empirical data
    if(from == "migrantstock_shiny"){
        # browser()
        fname <- "data/" # ./data
    }else
    if(from == "empiricalall"){
        fname <- msglobalsf("EMPIRICALFOLDERALL")
    }else
    if(from == "empirical"){
        fname <- msglobalsf("EMPIRICALFOLDER")
    }else
    if(from == "msest"){
        fname <- paste0(msglobalsf("PROJECTOUTPUTFOLDER"), "msest", LocID, "/")
    }else{
        # any other folder with data
        fname <- from
    }
    # browser()
    fname <- paste0(fname, LocID, "_EmpiricalData.ms.RData")
        
    # fname <- switch(from,
    #                  migrantstock_shiny = {fname <- paste0("data/", LocID, "_EmpiricalData.ms.RData")}, # C:/akf/mus/MS/scripts/migrantstock/data
    #                  empirical          = {fname <- paste0(msglobalsf("EMPIRICALFOLDER"), LocID, "_EmpiricalData.ms.RData")},
    #                  empiricalall       = {fname <- paste0(msglobalsf("EMPIRICALFOLDERALL"), LocID, "_EmpiricalData.ms.RData")},
    #                  msest              = {fname <- paste0(msglobalsf("PROJECTOUTPUTFOLDER"), "msest", LocID, "/", LocID, "_EmpiricalData.ms.RData")},
    #                  stop("invalid value of parameter \"from\"")
    # )
    
    # print(fname) 

    if (file.exists(fname)){
        load(fname)
        msg <- paste0("loadempiricaldata(): ", fname); print(msg);
        if(nrow(df) > 0){
            if(is.null(dfa)){
                dfa <- df  
            }else{
                dfa <- rbind(dfa, df)
            }
        }
    }
    
    if (IncludeRevisions){
        if(from != "migrantstock_shiny"){
            stop("from must be equal to migrantstock_shiny")
        }
        fname <- paste0("data/", LocID, "_Rev2020.RData")  
        if (file.exists(fname)){
            load(fname)
            if(!is.null(df)){
                if(nrow(df) > 0){
                    if(is.null(dfa)){
                        dfa <- df  
                    }else{
                        dfa <- rbind(dfa, df)
                    }
                }
            }
        }
        
        # browser() 
        fname <- paste0("data/", LocID, "_Rev2023.RData")  
        if (file.exists(fname)){
            load(fname)
            if(!is.null(df)){
                if(nrow(df) > 0){
                    if(is.null(dfa)){
                        dfa <- df  
                    }else{
                        dfa <- rbind(dfa, df)
                    }
                }
            }
        }
        
    }
    
    # remove all records with missing data
    if (!is.null(dfa)){
        e <- !is.nan(dfa$value)
        dfa = dfa[e,]
        if(nrow(dfa) == 0){
            dfa <- NULL
        }
    }
    
    # all empirical data 
    EmpiricalData <- dfa
    
    if (is.null(EmpiricalData)){
        return(NULL)
    }
    
    # default sort 
    # see "Notes on RLang.docx"
    
    # variables to sort, including numeric and character (!!! alternatively we can sort in ms_scan and skip sorting here …)
    vsort <- msglobalsf("DEFAULT_SORT_ORDER")
    # EmpiricalData <- EmpiricalData %>% arrange(!!!vsort)    # doesn't always work
    # call of order(df$source, df$LocID, ...) and use the returned index to select rows from df
    EmpiricalData <- EmpiricalData[do.call(what = order, args = EmpiricalData[vsort]),]

    # select empirical data on total migrant stock
    df0 <- EmpiricalData  %>%  filter(sex == 0 & age == 0 & agelength == 0 & LocIDorg == 0) 
    if(nrow(df0) == 0){df0 <- NULL}
    
    if(!is.null(df0)){
        df <- df0[is.element(df0$PopulationUniverse, msglobalsf("PU_MIGRANT_STOCKS")), ]         # select only populations for migrant stocks
        if(nrow(df) == 0){df <- NULL}
    }

    EmpiricalDataPopulationUniverse <- unique(df$PopulationUniverse)
    
    if(!is.null(df)){
        if (nrow(df) > 0){
            df <- df %>% select(LocID, PopulationUniverse, yearref, value, source) %>% arrange(source, LocID, PopulationUniverse, yearref)
        }
    }
    
    EmpiricalDataTotal <- df;

    # EmpiricalData <<- EmpiricalData  %>%
    #   filter(sex == 0 & age == 0 & agelength == 0 & LocIDorg == 0 & PopulationUniverse == 3) %>%  #!!! msglobals[["PU_FOREIGNBORN"]] here
    #   select(yearref, value, source)
    # summary(EmpiricalData)
    # summary(EmpiricalDataTotal)

    # origins
    ####browser()
    
    # current origin
    LocIDorgv <- msglobalsf("LocIDorg")     
    
    df <- NULL
    
    if (!is.null(LocIDorgv)){
        if (!is.na(LocIDorgv)){
            #    df <- EmpiricalData  %>%  filter(sex == 0 & age == 0 & agelength == 0 & LocIDorg == LocIDorgv)  # !!! will it work ?
            df  <-  EmpiricalData[EmpiricalData$sex == 0 & EmpiricalData$age == 0 & EmpiricalData$agelength == 0 & EmpiricalData$LocIDorg == LocIDorgv,]  # should be faster?
            if(nrow(df) == 0){df <- NULL}
        } 
    } 

    if (!is.null(df)){
        if (nrow(df) > 0){
            df <- df[is.element(df$PopulationUniverse, msglobalsf("PU_MIGRANT_STOCKS")), ]         # select only populations for migrant stocks
            if(nrow(df) == 0){df <- NULL}
        }
    }
    
    if (!is.null(df)){
        if (nrow(df) > 0){
            df <- df %>% select(LocID, LocIDorg, PopulationUniverse, yearref, value, source) %>% arrange(source, LocID, LocIDorg, PopulationUniverse, yearref)
        }
    }
    
    EmpiricalDataTotalOrigin <- df;

    # update global variables
    force(msglobalsf("EmpiricalData", EmpiricalData))
    force(msglobalsf("EmpiricalDataTotal", EmpiricalDataTotal))
    force(msglobalsf("EmpiricalDataTotalOrigin", EmpiricalDataTotalOrigin))
    force(msglobalsf("EmpiricalDataPopulationUniverse", EmpiricalDataPopulationUniverse))
    force(msglobalsf("EmpiricalDataFileName", fname))

    return(EmpiricalData)

}

##################################################################################################
# # test
# msglobalsf("LocID", 524)
# loadEmpiricalData("migrantstock_shiny")
# dforg <- msglobalsf("EmpiricalDataTotal")

