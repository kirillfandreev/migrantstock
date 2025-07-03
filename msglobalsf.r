# Global variables and folders for migrant stock project
# Examples: 
#   msglobalsf() -- returns all global variables
#   msglobalsf("SHAREPOINTFOLDER")
#   msglobalsf("PU_TOTAL")
#   msglobalsf("PU_FOREIGNBORN")
#   msglobalsf("PU_FOREIGNERS")
#   msglobalsf("PU_REFUGEES")
#   msglobalsf("PU_ASYLUM_SEEKERS")
#   msglobalsf("PU_VENEZUELANS_DISPLACED_ABROAD")
#
# Revision history: 
# 
#   2023/12/15
#   First version created. 
#   Tested on the data for Australia (Trend in total migrant stocks in Australia is significantly affected by COVID-19).
#   Kirill Andreev (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)
# 
#   2023/5/4
#       Updates (Kirill Andreev)
#       Setup for Nicole
#
#   2023/5/9
#       Some error handling code (Kirill Andreev)

# See "Notes on RLang.docx"

#!!!todo
# Sys.getenv("USERNAME")
# Sys.getenv()
# Sys.getenv("HOME")

library("tidyverse")
library("readxl")
source("msglobals_KA.r", local = TRUE)
source("msglobals_KJ.r", local = TRUE)  # UN notebook JB's office 

# source("msglobals_DG.r", local = TRUE)
# source("msglobals_NL.r", local = TRUE)
# source("msglobals_KU.r", local = TRUE)
# source("msglobals_KD.r", local = TRUE)

# delete all objects 
# rm(list=ls())
# Clear console
# cat("\014")  # ctrl+L

# Function factory for msglobalsf()
msglobalsfct <- function() {

    # list with globals in enclosing environment 
    msglobals <- list()

    # standard list of LocIDs, 2020 Rev from
    # <International_migrant_stock_by_sex_and_destination_2020.xlsx; Table 1;D12> with duplicated 927, AUS/NZL
    LocIDWORLD <- 900
    LocIDsMS2020sorted <- c(900, 947, 1833, 921, 1832, 1830, 1835, 927, 1829, 901, 902, 934, 948, 941, 1636, 1637, 1503, 1517, 1502, 1501, 1500, 903, 910, 108, 174, 262, 232, 231, 404, 450, 454, 480, 175, 508, 638, 646, 690, 706, 728, 800, 834, 894, 716, 911, 24, 120, 140, 148, 178, 180, 226, 266, 678, 912, 12, 818, 434, 504, 729, 788, 732, 913, 72, 748, 426, 516, 710, 914, 204, 854, 132, 384, 270, 288, 324, 624, 430, 466, 478, 562, 566, 654, 686, 694, 768, 935, 5500, 398, 417, 762, 795, 860, 906, 156, 344, 446, 158, 408, 392, 496, 410, 920, 96, 116, 360, 418, 458, 104, 608, 702, 764, 626, 704, 5501, 4, 50, 64, 356, 364, 462, 524, 586, 144, 922, 51, 31, 48, 196, 268, 368, 376, 400, 414, 422, 512, 634, 682, 275, 760, 792, 784, 887, 908, 923, 112, 100, 203, 348, 616, 498, 642, 643, 703, 804, 924, 830, 208, 233, 234, 246, 352, 372, 833, 428, 440, 578, 752, 826, 925, 8, 20, 70, 191, 292, 300, 336, 380, 470, 499, 807, 620, 674, 688, 705, 724, 926, 40, 56, 250, 276, 438, 442, 492, 528, 756, 904, 915, 660, 28, 533, 44, 52, 535, 92, 136, 192, 531, 212, 214, 308, 312, 332, 388, 474, 500, 630, 652, 659, 662, 663, 670, 534, 780, 796, 850, 916, 84, 188, 222, 320, 340, 484, 558, 591, 931, 32, 68, 76, 152, 170, 218, 238, 254, 328, 600, 604, 740, 858, 862, 905, 60, 124, 304, 666, 840, 909, 927, 36, 554, 928, 242, 540, 598, 90, 548, 954, 316, 296, 584, 583, 520, 580, 585, 957, 16, 184, 258, 570, 882, 772, 776, 798, 876, 2003)
    LocIDs2020Countries  <- LocIDsMS2020sorted[LocIDsMS2020sorted < LocIDWORLD]
    # Country assignments by analyst from 
    # C:\akf\DESA-POP-MUS\MS\docs\Migrant_Stock_Project_Overview.xlsx
    # #fixme – need to read from Excel file
    LocIDsAnalyst <- list(c(840,	276,	643,	826,	250,	124,	36,	724,	380,	792,	804,	398,	392,	702,	756,	528,	56,	752,	40,	554,	300,	860,	112,	620,	372,	578,	688,	616,	208,	642,	348,	203,	191,	246,	442,	705,	762,	31,	428,	233,	417,	703,	795,	196,	51,	100,	440,	807,	470,	498,	830,	316,	268,	499,	352,	8,	20,	70,	492,	438,	16,	580,	60,	292,	234,	304,	666,	336))
    names(LocIDsAnalyst) <- c("KA")

    # current revision    
    LocIDsAllCountries <- LocIDs2020Countries           # all countries / areas for the current revision
    msglobals[["REVISION_NAME_PRV"]] <- "Rev2020"
    msglobals[["REVISION_NAME"]]     <- "Rev2023"
    msglobals[["ESTIMATED_SERIES_NAME"]] <- msglobals[["REVISION_NAME"]]
    
    # msglobals[["YEARS_PUBLISHED"]] <- c(1990.5, 1995.5, 2000.5, 2005.5, 2010.5, 2015.5, 2020.5, 2023.5)
    msglobals[["YEARS_PUBLISHED"]] <- c(1990.5, 1995.5, 2000.5, 2005.5, 2010.5, 2015.5, 2020.5, 2024.5)
    msglobals[["FIRST_YEAR_PROJECTION"]] <- c(1990)
    msglobals[["LAST_YEAR_PROJECTION"]]  <- c(2025)
    msglobals[["YEARS_PROJECTION_SINGLE"]]  <- msglobals[["FIRST_YEAR_PROJECTION"]]:msglobals[["LAST_YEAR_PROJECTION"]]
    
    # first year 1990 = floor(msglobalsf("YEARS_PUBLISHED")[1])

    msglobals[["AGE_PUBLISHED"]] <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)
    
    # current LocID
    LocIDsOriginSelected <- NULL
    LocID <- NULL
    
    ####################################################################################################################################################
    # setup global folders and list of countries
    ret <- NULL
    msglobals[["rftLocation"]]                      <- NULL
    msglobals[["rftPopulationUniverse"]]            <- NULL
    msglobals[["Migrant_Stock_Project_Overview"]]   <- NULL
    msglobals[["SHAREPOINTFOLDER"]]                 <- "C:/akf/DESA-POP-MUS/MS/"
    msglobals[["EMPIRICALFOLDERALL"]]               <- "C:/akf/DESA-POP-MUS/MS/processing/empiricalall/"
    msglobals[["EMPIRICALFOLDER"]]                  <- "C:/akf/DESA-POP-MUS/MS/processing/empirical/"
    msglobals[["CURRENT_DATA_FOLDER"]]              <- msglobals[["EMPIRICALFOLDER"]]  # used by loadempiricaldata()
    
    msglobals[["PROJECTFOLDER"]]                    <- "C:/akf/mus/ms/scripts/migrantstock/"
    # set later
    msglobals[["PROJECTOUTPUTFOLDER"]]              <- NULL
    msglobals[["PROJECTINPUTFOLDER"]]               <- NULL
    msglobals[["SHAREPOINTFOLDER_PROJECTFOLDER"]]   <- ""
    msglobals[["UserName"]]                         <- NULL

    LocIDDataAnalyst <- LocIDsAllCountries
    LocIDsSelected   <- LocIDsAllCountries    # selected countries or one country to run 

    # global variables for user Kirill at KASPECTRA computer
    if((Sys.getenv("USERNAME") == "Kirill" && Sys.getenv("COMPUTERNAME") == "KASPECTRA")|
       (Sys.getenv("USERNAME") == "Kirill.Andreev" && Sys.getenv("COMPUTERNAME") == "WIN7ENT-AP8P18S") |
       (Sys.getenv("USERNAME") == "Kirill.Andreev" && Sys.getenv("COMPUTERNAME") == "W10LT-PF3AZK0E")  |
       (Sys.getenv("USERNAME") == "Kirill" && Sys.getenv("COMPUTERNAME") == "KADELL23")){ 
        # browser()
        msglobals[["UserName"]]  <- "KA"
        ret <- msglobals_KA()
    } else 
    if(Sys.getenv("USERNAME") ==  "KANDREEV" && Sys.getenv("COMPUTERNAME") == "UNHQESAPF3AZK0E") {
      # browser()
      msglobals[["UserName"]]  <- "KJ"
      ret <- msglobals_KJ()
    } else 
    if(Sys.getenv("USERNAME") == "MunSim.Lai" && Sys.getenv("COMPUTERNAME") == "W10LT-PF2S0TFN"){
        msglobals[["UserName"]]  <- "NL"
        ret <- msglobals_NL()
    } else 
    if(Sys.getenv("USERNAME") == "MLAI9" && Sys.getenv("COMPUTERNAME") == "UNHQESAPF2RJETM"){
        msglobals[["UserName"]]  <- "NL"
        ret <- msglobals_NL()
    } else 
    if(Sys.getenv("USERNAME") == "sapph" && Sys.getenv("COMPUTERNAME") == "LAPTOP-FJE9OT6C"){  # if(Sys.getenv("USERNAME") == "yhan" && Sys.getenv("COMPUTERNAME") == "SC-AD-LA-1346")
        msglobals[["UserName"]]  <- "YH"
        ret <- msglobals_YH()
    } else 
    if(Sys.getenv("USERNAME") ==  "Danan.Gu" && Sys.getenv("COMPUTERNAME") == "W10LT-PF3AZM95"){
        msglobals[["UserName"]]  <- "DG"
        ret <- msglobals_DG()
    } else {
        # unknown user / computer 
        isLocal <- Sys.getenv('SHINY_PORT') == ""
        # Sys.getenv('SHINY_PORT') = 42331 or "" if runs locally 
        
        if(isLocal){
            # throw an error if not online 
            msg <- paste0("<", Sys.getenv("USERNAME"), "> : <", Sys.getenv("COMPUTERNAME"), ">  -- no global folders implemented in msglobalsf.r")
            stop(msg)
        }
    }
    
    # else 
    #     if(Sys.getenv("USERNAME") == "Kirill.Andreev" && Sys.getenv("COMPUTERNAME") == "W10LT-PF3AZK0E"){
    #         # global variables for user Kirill Andreev, UN notebook
    #         ret <- msglobals_KU()
    #     }
    
    # else 
    #     if(Sys.getenv("USERNAME") == "Kirill" && Sys.getenv("COMPUTERNAME") == "KADELL23"){
    #         ret <- msglobals_KD()
    #     }
    # 

    # browser()
    # ret <- NULL
    # 
    if(!is.null(ret)){
        
        # 
        if(is.element("UserName", names(ret))){
            msglobals[["USERNAME"]] <- ret$UserName
        }
        
        if(is.element("SHAREPOINTFOLDER", names(ret))){
            msglobals[["SHAREPOINTFOLDER"]] <- ret$SHAREPOINTFOLDER
        }
        
        if(is.element("EMPIRICALFOLDER", names(ret))){
            msglobals[["EMPIRICALFOLDER"]] <- ret$EMPIRICALFOLDER
        }
        
        if(is.element("EMPIRICALFOLDERALL", names(ret))){
            msglobals[["EMPIRICALFOLDERALL"]] <- ret$EMPIRICALFOLDERALL
        }
        
        if(is.element("PROJECTFOLDER", names(ret))){
            msglobals[["PROJECTFOLDER"]] <- ret$PROJECTFOLDER
        }
        
        if(is.element("CURRENT_DATA_FOLDER", names(ret))){
            msglobals[["CURRENT_DATA_FOLDER"]] <- ret$CURRENT_DATA_FOLDER
        }
        
        # update global variables from ret returned by msglobals_KA() etc
        # # globals provided by msglobals_??.r
        # msglobals[["SHAREPOINTFOLDER"]]     <- ret$SHAREPOINTFOLDER
        # msglobals[["EMPIRICALFOLDER"]]      <- ret$EMPIRICALFOLDER
        # msglobals[["EMPIRICALFOLDERALL"]]   <- ret$EMPIRICALFOLDERALL
        # msglobals[["PROJECTFOLDER"]]        <- ret$PROJECTFOLDER
        
        if(is.null(msglobals[["PROJECTFOLDER"]])){
            msg <- "msglobalsf(\"PROJECTFOLDER\") is null"
            stop(msg)
        }

        if(is.null(msglobals[["SHAREPOINTFOLDER"]])){   # n.b. if NULL, the syntax msglobals$SHAREPOINTFOLDER will return msglobals$SHAREPOINTFOLDER_PROJECTFOLDER due to partial name match 
            msg <- "msglobalsf(\"SHAREPOINTFOLDER\") is null"
            stop(msg)
        }
        
        # fixed folder structure 
        msglobals[["PROJECTOUTPUTFOLDER"]]  <- paste0(msglobals[["PROJECTFOLDER"]], "output/")
        msglobals[["PROJECTINPUTFOLDER"]]   <- paste0(msglobals[["PROJECTFOLDER"]], "input/")
        msglobals[["SHAREPOINTFOLDER_PROJECTFOLDER"]]  <- paste0(msglobals[["SHAREPOINTFOLDER"]], "scripts/migrantstock/")
        msglobals[["FINALFOLDER"]]  <- paste0(msglobals[["SHAREPOINTFOLDER"]], "processing/final/")
        
        if(is.null(msglobals[["EMPIRICALFOLDER"]])){
            # default empirical folder 
            msglobals[["EMPIRICALFOLDER"]] <- paste0(msglobals[["SHAREPOINTFOLDER"]], "processing/empirical/")
        }
        
        if(is.null(msglobals[["EMPIRICALFOLDERALL"]])){
            # default empiricalall folder 
            msglobals[["EMPIRICALFOLDERALL"]] <- paste0(msglobals[["SHAREPOINTFOLDER"]], "processing/empiricalall/")
        }
        
        # create folders
        if (!dir.exists(msglobals[["PROJECTOUTPUTFOLDER"]])){
            dir.create(msglobals[["PROJECTOUTPUTFOLDER"]], showWarnings = FALSE)
        }
        
        # create folders
        folder <- paste0(msglobals[["SHAREPOINTFOLDER"]], "scripts")
        if(!dir.exists(folder)){dir.create(folder, showWarnings = FALSE)}
        
        folder <- paste0(msglobals[["SHAREPOINTFOLDER"]], "scripts/migrantstock")
        if(!dir.exists(folder)){dir.create(folder, showWarnings = FALSE)}
        
        LocIDsSelected <- ret$LocIDsSelected

        if(is.character(LocIDsSelected)){
            if(LocIDsSelected == "all"){
                LocIDsSelected <- LocIDsAllCountries
            }else
            if(LocIDsSelected == "analyst"){
                if(is.element(ret$UserName, names(LocIDsAnalyst))){
                    LocIDsSelected <- LocIDsAnalyst[[ret$UserName]]
                }else{
                    stop("analyst not implemented")
                }
            }else{
                msg <- "msglobalsf(): LocIDsOriginSelected -- invalid parameter"
                stop(msg)
            }
        }
        
        if(!is.null(ret$LocIDsExcluded)){    
            LocIDsSelected <- setdiff(LocIDsSelected, ret$LocIDsExcluded)
        }
        
        LocIDsOriginSelected <- ret$LocIDsOriginSelected
        
        # browser()
        
        if(is.character(LocIDsOriginSelected)){
            if(LocIDsOriginSelected == "all"){
                # all countries
                LocIDsOriginSelected <- LocIDsAllCountries
            }else
            if(LocIDsOriginSelected == "final"){
                # browser()
                if(length(LocIDsSelected) == 1){
                    fname <- paste0(msglobals[["FINALFOLDER"]], LocIDsSelected, "_final_Rev2023.RData")
                    if(file.exists(fname)){
                        load(fname)
                        LocIDsOriginSelected <- unique(df$LocIDorg)
                        LocIDsOriginSelected <- LocIDsOriginSelected[LocIDsOriginSelected > 0 & LocIDsOriginSelected < 900]
                    }else{
                        LocIDsOriginSelected <- LocIDsAllCountries
                    }
                }else{
                    msg <- "msglobalsf(): LocIDsOriginSelected -- single country expected"
                    stop(msg)
                }
            }
            else{
                msg <- "msglobalsf(): LocIDsOriginSelected -- invalid parameter"
                stop(msg)
            }
        }

        if(is.null(LocIDsOriginSelected)){    
            # total only
            LocIDsOriginSelected <- 0
        }
    }

    # Population universe constants 
    # Data are from rftPopulationUniverse.xlsx table
    # Alternatively we can use factors or functions PU_TOTAL <- function() {return(0)}
    # neither looks good
    msglobals[["PU_TOTAL"]]                         <- 0
    msglobals[["PU_CITIZEN"]]                       <- 1
    msglobals[["PU_REFUGEES"]]                      <- 2
    msglobals[["PU_FOREIGNBORN"]]                   <- 3
    msglobals[["PU_FOREIGNERS"]]                    <- 4
    msglobals[["PU_NATIVE"]]                        <- 5
    msglobals[["PU_STATELESS"]]                     <- 8
    msglobals[["PU_UNKNOWN"]]                       <- -1
    msglobals[["PU_ASYLUM_SEEKERS"]]                <- 6
    msglobals[["PU_VENEZUELANS_DISPLACED_ABROAD"]]  <- 7
    msglobals[["PU_INTERNATIONAL_MIGRANTS"]]        <- 9
    msglobals[["PU_OTHER_PEOPLE_IN_NEED"]]          <- 10
    msglobals[["PU_NON_PERMANENT_RESIDENTS"]]       <- 11

    #
    msglobals[["PU_FOREIGNBORN_PU_FOREIGNERS"]] <- -34 # combined PU_FOREIGNBORN and PU_FOREIGNERS
    # series used for computing migrant stocks
    msglobals[["PU_MIGRANT_STOCKS"]] <- c(msglobals$PU_REFUGEES, msglobals$PU_FOREIGNBORN, msglobals$PU_FOREIGNERS, msglobals$PU_ASYLUM_SEEKERS, msglobals$PU_VENEZUELANS_DISPLACED_ABROAD, msglobals$PU_INTERNATIONAL_MIGRANTS, msglobals$PU_OTHER_PEOPLE_IN_NEED, msglobals$PU_NON_PERMANENT_RESIDENTS)
  
  # rftPopulationUniverse = msrecode('rftPopulationUniverse');
  
    # output_folder <- substr(output_folder, 1, nchar(output_folder) - 1) # remove last /
    # msglobals[["TEMPORARY_FOLDER"]]  <- "c:/temp/migrantstock/"
    msglobals[["TEMPORARY_FOLDER"]]  <- paste0(Sys.getenv("TEMP"), "\\migrantstock\\")  # C:\Users\Kirill\AppData\Local\Temp\migrantstock

    if (!dir.exists(msglobals[["TEMPORARY_FOLDER"]])){
        dir.create(msglobals[["TEMPORARY_FOLDER"]], showWarnings = FALSE)
    }

    # selected LocID codes
    msglobals[["LocIDsMS2020sorted"]]     <- LocIDsMS2020sorted
    msglobals[["LocIDWORLD"]]             <- LocIDWORLD
    msglobals[["LocIDs2020Countries"]]    <- LocIDs2020Countries      # all 235 countries / areas in the 2020 revision
    msglobals[["LocIDsSelected"]]         <- LocIDsSelected
    msglobals[["LocIDsAllCountries"]]     <- LocIDsAllCountries       # msglobalsf("COUNTRIESALL")
    msglobals[["LocIDorgOther"]]          <- 2003                     # the code used before for OTHER / RESIDUAL group
    
    # indicators 
    msglobals[["INDICATORSFOLDER"]]  <- paste0(msglobals[["SHAREPOINTFOLDER"]], "processing/empindic/")
    msglobals[["INDIC_PRPF"]]  <- 1  # DESA-POP/MUS/MS/sys/rftIndicators.xlsx
    
    # sex codes
    msglobals[["SEX_TOTAL"]]   <- 0
    msglobals[["SEX_MALE"]]    <- 1
    msglobals[["SEX_FEMALE"]]  <- 2

    # user data
    msglobals[["UserData"]]  <- NULL
    msglobals[["DEBUG"]]  <- FALSE 
    
    # length(msglobalsf("LocIDs235"))
    # browser()
    
    # rftLocation table
    fname <- paste0("data/rftLocation.RData") # "data/rftLocation.RData", copy of rftLocation.xlsx from SP updated by msappcompiler.R
    if (file.exists(fname)){
        load(fname)
        msglobals[["rftLocation"]] <- df # keep entire rftLocation, msglobalsf("rftLocation")
    }else{
        msglobals[["rftLocation"]] <- NULL
    }

    # rftPopulationUniverse table
    fname <- paste0("data/rftPopulationUniverse.RData") 
    if (file.exists(fname)){
        load(fname)
        msglobals[["rftPopulationUniverse"]] <- df
    }else{
        msglobals[["rftPopulationUniverse"]] <- NULL
    }
    
    # Migrant_Stock_Project_Overview table
    fname <- paste0("data/Migrant_Stock_Project_Overview.RData") 
    if (file.exists(fname)){
        load(fname)
        msglobals[["Migrant_Stock_Project_Overview"]] <- df
    }else{
        msglobals[["Migrant_Stock_Project_Overview"]] <- NULL
    }
    
    # Countries to include into the application
    # all
    fname <- paste0("data/rftLocation_Rev2020.RData") # "data/rftLocation_Rev2020.RData", copy of rftLocation.xlsx from SP updated by msappcompiler.R
    df <- NULL
    if (file.exists(fname)){
        
        load(fname)
        
        # all countries in the current revision
        dfall <- df  %>% dplyr::filter(df$LocID %in% LocIDsAllCountries) %>% arrange(LocID)
        msglobals[["COUNTRIESALL"]] <- dfall                                 

        # LocIDsSelected: countries selected by user
        df <- dfall
        # check if valid LocIDs
        for(locidi in LocIDsSelected){
            v <- sum(df$LocID == locidi)
            if(v==0){
                msg <- paste0("LocID = ", locidi, " does not exist in rftLocation.RData table", " {{errID=2}}");
                stop(msg)
            }else
            if(v > 1){
                msg <- paste0("LocID = ", locidi, " has several entries in rftLocation.RData table", " {{errID=3}}");
                stop(msg)
            }
        }
        
        # df <- df %>% filter(LocID %in% c(208, 36, 124, 356))
        df <- df  %>% dplyr::filter(df$LocID %in% LocIDsSelected) %>% arrange(LocID)
        msglobals[["COUNTRIES"]] <- df                                    # Selected countries for the current revision 
        if(nrow(df) == 0){stop(paste0("No countries selected for LocIDs = ", LocIDsSelected, " {{errID=1}}"))}

        # LocIDsOriginSelected: countries of origin selected by user
        
        LocIDsOriginSelected <- LocIDsOriginSelected[LocIDsOriginSelected > 0]
        if(length(LocIDsOriginSelected) > 0){

            df <- dfall    
         
            e <- is.element(LocIDsOriginSelected, dfall$LocID)
            if(all(e)){
                # print("Ok")
            }else{
                msg <- paste0("msglobalsf.r: LocIDsOriginSelected includes invalid LocIDs", " {{errID=4}}"); stop(msg);
            }
            
            df <- df  %>% dplyr::filter(df$LocID %in% LocIDsOriginSelected)
            msglobals[["COUNTRIES_ORIGIN"]] <- df   # selected countries of origin for the current revision 
            # if(nrow(df) == 0){stop(paste0("No countries selected for LocIDs = ", LocIDsSelected, " {{errID=1}}"))}
        }
        msglobals[["COUNTRIES_ORIGIN"]] <- rbind(data.frame(LongName = "Total", LocID = 0, ISO3 = "000"), msglobals[["COUNTRIES_ORIGIN"]]) # add total
    }else{
        msglobals[["COUNTRIESALL"]]         <- NULL
        msglobals[["COUNTRIES"]]            <- NULL
        msglobals[["COUNTRIES_ORIGIN"]]     <- NULL
        msglobals[["rftLocation"]]          <- NULL
        message("data/rftLocation.RData not found {{errID=1}}")
        # stop("data/rftLocation.RData not found {{errID=1}}")
    }
    # df <- read_csv("data/rftLocation.csv")

    # print(df)
  
    # data loaded into memory for the current LocID and unique series names
    msglobals[["LocID"]]                            <- LocID
    msglobals[["LocIDorg"]]                         <- NULL
    msglobals[["EmpiricalData"]]                    <- NULL
    msglobals[["EmpiricalDataTotal"]]               <- NULL
    msglobals[["EmpiricalDataPopulationUniverse"]]  <- NULL
    msglobals[["EmpiricalDataFileName"]]            <- NULL
    msglobals[["EmpiricalDataTotalOrigin"]]         <- NULL
    # ??? state 
    # log file
    msglobals[["LOGFILE"]] <- "log.txt"

    fh <- file(msglobals[["LOGFILE"]], open = "at")
    msg <- "Application started ----------------------------------------------------"
    cat(format(Sys.time(), "%Y-%m-%d %X"), ":", msg, "\n", append = T, file = fh)
    close(fh)
  
    # primary key
    msglobals[["EMPIRICAL_DATA_FIELDS_PK"]] <- c('LocID', 'sex', 'yearref', 'age', 'agelength', 'LocIDorg', 'PopulationUniverse', 'source')
    msglobals[["DEFAULT_SORT_ORDER"]] <- c('source', 'LocID', 'sex', 'PopulationUniverse', 'LocIDorg', 'yearref', 'age', 'agelength')

    # EMPIRICAL_DATA_FIELDS_PK = {'LocID' 'sex'	'yearref'	'age'	'agelength'	'LocIDorg'	'PopulationUniverse'	'source'};
    # EMPIRICAL_DATA_NAN = -999999;       % numeric code for NaN for text files
    # EMPIRICAL_DATA_NA = {'n/a'};        % cell code for empty value or NaN, % not available
    # EMPIRICAL_DATA_FIELDS_CHAR = {'country' 'FieldWorkStart' 'FieldWorkEnd' 'agestr' 'origin' 'source' 'note' 'fname'};        % fields of character type
  
    # a function to be returned 
    function(vname = NULL, vvalue = NULL) {
        
        # if(vname == "LocIDorg"){
        #     #!!!
        #     browser()
        # }
        
        # def <- get("x", environment())
        # cll <- get("x", parent.frame())
        # list(defined = def, called = cll)
    
        # cat("Number of arguments", nargs())
    
        if(nargs() == 1){
            # just get variable from the global list
            if(is.null(vname)){
            print(msglobals)
            }else{
            return(msglobals[[vname]])  
            }
        } else if(nargs() == 2) {
            # update global variable
            msglobals[[vname]] <<- vvalue
            return(vvalue)
        } else if(nargs() == 0) {
            # display some help ...
            print(msglobals)
            return(NULL)
        }else{
            stop("invalid nargs()")
        }

    } # a function to be returned -- end
  
}

# function to manage all global variables
#  msglobalsf("SHAREPOINTFOLDER") - returns path to a sharepoint folder 
msglobalsf <- msglobalsfct()

# $REVISION_NAME_PRV
# $REVISION_NAME
# $ESTIMATED_SERIES_NAME
# $YEARS_PUBLISHED
# $FIRST_YEAR_PROJECTION
# $LAST_YEAR_PROJECTION
# $YEARS_PROJECTION_SINGLE
# $AGE_PUBLISHED
# $SHAREPOINTFOLDER "C:/akf/DESA-POP-MUS/MS/"
# $PROJECTFOLDER "C:/akf/mus/MS/scripts/migrantstock/"
# $PROJECTOUTPUTFOLDER "C:/akf/mus/MS/scripts/migrantstock/output/"
# $PROJECTINPUTFOLDER "C:/akf/mus/MS/scripts/migrantstock/input/"
# $SHAREPOINTFOLDER_PROJECTFOLDER "C:/akf/DESA-POP-MUS/MS/scripts/migrantstock/"
# $USERNAME "KA"
# $FINALFOLDER "C:/akf/DESA-POP-MUS/MS/processing/final/"
# $EMPIRICALFOLDER "C:/akf/DESA-POP-MUS/MS/processing/empirical/"
# $EMPIRICALFOLDERALL "C:/akf/DESA-POP-MUS/MS/processing/empiricalall/"
# $PU_TOTAL 
# $PU_CITIZEN
# $PU_REFUGEES
# $PU_FOREIGNBORN
# $PU_FOREIGNERS
# $PU_NATIVE
# $PU_STATELESS
# $PU_UNKNOWN
# $PU_ASYLUM_SEEKERS
# $PU_VENEZUELANS_DISPLACED_ABROAD
# $PU_INTERNATIONAL_MIGRANTS
# $PU_OTHER_PEOPLE_IN_NEED
# $PU_NON_PERMANENT_RESIDENTS
# $PU_FOREIGNBORN_PU_FOREIGNERS
# $PU_MIGRANT_STOCKS
# $TEMPORARY_FOLDER
# $LocIDsMS2020sorted
# $LocIDWORLD
# $LocIDs2020Countries
# $LocIDsSelected
# $LocIDsAllCountries
# $LocIDorgOther
# $INDICATORSFOLDER
# $INDIC_PRPF
# $SEX_TOTAL
# $SEX_MALE
# $SEX_FEMALE
# $rftLocation
# $rftPopulationUniverse
# $Migrant_Stock_Project_Overview
# $COUNTRIESALL
# $COUNTRIES
# $COUNTRIES_ORIGIN
# $LOGFILE
# $EMPIRICAL_DATA_FIELDS_PK
# $DEFAULT_SORT_ORDER
# $LocID
# $EmpiricalData
# $EmpiricalDataTotal
# $EmpiricalDataTotalOrigin
# $EmpiricalDataPopulationUniverse
# $EmpiricalDataFileName
# $UserData
