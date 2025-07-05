# Global variables and folders for migrant stock project for user Kirill Andreev (KA), UN notebook, JB's office 
# This function is executed at startup by msglobalsf.r.
# source("msglobalsf.r")
# @@@all
# @@@locid
msglobals_KJ <- function() {
    
    # msglobals_data <- NULL
    # if(file.exists("msglobals_KA.RData")){
    #     load("msglobals_KA.RData")
    #     message("Using msglobals_KA.RData")
    # }

    ret <- NULL
    
    ##################################################################################################################################################################
    # Custom global parameters for computer setup
    # ret$UserName <- "KA"
    
    # the folder with the synchronized Share Point folders or any folder
    # ret$SHAREPOINTFOLDER    <- "C:/akf/DESA-POP-MUS/MS/"      
    # ret$SHAREPOINTFOLDER      <- "C:/akf/mus/MS_TMP/"            # the folder with R scripts etc.  
    # ret$SHAREPOINTFOLDER           <- "C:/akf/mus/MS_SP/"            # the folder with R scripts etc.  
    # ret$SHAREPOINTFOLDER    <- "C:/work/"
    ret$SHAREPOINTFOLDER    <- ""
    
    # local folder with scripts etc.
    # ret$PROJECTFOLDER         <- "C:/akf/mus/MS/scripts/migrantstock/"            # the folder with R scripts etc.  
    ret$PROJECTFOLDER         <- "C:/work/migrantstock/"            # the folder with R scripts etc.  
    
    # data folders: all and "cleaned", used for estimation 
    # ret$EMPIRICALFOLDER      <- "C:/akf/MUS/MS/Empirical/"
    # ret$EMPIRICALFOLDERALL   <- "C:/akf/MUS/MS/empiricalall/"
    
    # # SharePoint (SP)
    # ret$EMPIRICALFOLDERALL   <- "C:/akf/DESA-POP-MUS/MS/processing/empiricalall/"
    # ret$EMPIRICALFOLDER      <- "C:/akf/DESA-POP-MUS/MS/processing/empirical/"
    
    # set current folder with the data here or commment for defaults
    # ret$CURRENT_DATA_FOLDER <- "C:/akf/DESA-POP-MUS/MS/processing/empirical/"     # NO NEED TO UNCOMMENT
    
    # @@@all
    # ret$CURRENT_DATA_FOLDER <- "C:/akf/DESA-POP-MUS/MS/processing/empiricalall/"    # Use all data --  uncomment to run at all data (!!!)
    # ret$CURRENT_DATA_FOLDER <- "C:/akf/MUS/MS/scripts/migrantstock/output/test/"

    ##################################################################################################################################################################
    # Countries of Origin 
    #
    # # LocIDorgs <- c(233, 752, 368)
    # LocIDorgs <- c(368,	233,	752,	643,	706,	760,	764,	704,	156,	688,	356,	792,	364,	840,	608,	4,	276,	616,	524,	729,	826,	
    #                203,	642,	566,	50,	804,	586,	104,	428,	348,	504,	231,	180,	380,	100,	724,	124,	76,	300,	250,	232,	288,	120,	818,	578,	440,	528,	178,	392,	208,	404,	270,	12,	8,	36,	484,	756,	144,	376,	70,	710,	422,	170,	646,	788,	894,	860,	410,	191,	682,	498,	56,	360,	40,	887,	152,	458,	703,	372,	604,	192,	620,	834,	112,	398,	400,	32,	784,	800,	24,	862,	414,	434,	352,	218,	554,	196,	454,	116,	686,	68,	702,	516,	31,	558,	214,	807,	705,	858,	430,	108,	268,	324,	417,	384,	716,	51,	762,	496,	694,	340,	768,	275,	188,	320,	442,	508,	222,	418,	728,	480,	795,	328,	562,	388,	470,	634,	780,	499,	450,	332,	478,	48,	591,	600,	466,	204,	72,	854,	262,	624,	226,	748,	52,	140,	132,	148,	512,	266,	598,	44,	492,	242,	740,	690,	90,	662,	212,	408,	20,	308,	174,	798,	659,	462,	96,	533,	
    #                674,	670,	64,	548,	28,	882,	84,	336,	776,		258,	796,	60, 585)

    # all including total 236
    LocIDorgsAll <- c(0, 4,	8,	12,	16,	20,	24,	28,	31,	32,	36,	40,	44,	48,	50,	51,	52,	56,	60,	64,	68,	70,	72,	76,	84,	90,	92,	96,	100,	104,	108,	112,	116,	120,	124,	132,	136,	140,	144,	148,	152,	156,	158,	170,	174,	175,	178,	180,	184,	188,	191,	192,	196,	203,	204,	208,	212,	214,	218,	222,	226,	231,	232,	233,	234,	238,	242,	246,	250,	254,	258,	262,	266,	268,	270,	275,	276,	288,	292,	296,	300,	304,	308,	312,	316,	320,	324,	328,	332,	336,	340,	344,	348,	352,	356,	360,	364,	368,	372,	376,	380,	384,	388,	392,	398,	400,	404,	408,	410,	414,	417,	418,	422,	426,	428,	430,	434,	438,	440,	442,	446,	450,	454,	458,	462,	466,	470,	474,	478,	480,	484,	492,	496,	498,	499,	500,	504,	508,	512,	516,	520,	524,	528,	531,	533,	534,	535,	540,	548,	554,	558,	562,	566,	570,	578,	580,	583,	584,	585,	586,	591,	598,	600,	604,	608,	616,	620,	624,	626,	630,	634,	638,	642,	643,	646,	652,	654,	659,	660,	662,	663,	666,	670,	674,	678,	682,	686,	688,	690,	694,	702,	703,	704,	705,	706,	710,	716,	724,	728,	729,	732,	740,	748,	752,	756,	760,	762,	764,	768,	772,	776,	780,	784,	788,	792,	795,	796,	798,	800,	804,	807,	818,	826,	830,	833,	834,	840,	850,	854,	858,	860,	862,	876,	882,	887,	894);
    
    # LocIDorgs <- LocIDorgsAll;
    # 
    # # LocIDorgs <-760
    # # LocIDorgs <-706
    # # LocIDorgs <- 184
    # # LocIDorgs <- c(368)
    # # LocIDorgs <- c(796)
    # # LocIDorgs <- c(2003)
    # 
    # # LocIDorgs <- c(478) # no empirical data
    
    
    # not a good idea -- all paths donot exist yet

    # ret$LocIDsOriginSelected <- c(504,	642,	170,	218,	862,	32,	826,	604,	250,	156)    
    # ret$LocIDsOriginSelected <- c(250)    # France
    # ret$LocIDsOriginSelected <- c(276, 380)    


    ret$LocIDsOriginSelected<- "all"          # countries of origins 235
    # ret$LocIDsOriginSelected<- "final"          # list of final
    # ret$LocIDsOriginSelected<- 804
    
    ##################################################################################################################################################################
    
    
    ##################################################################################################################################################################
    #                       Countries
    ##################################################################################################################################################################
    
    ret$LocIDsExcluded          <- NULL
    
    ret$LocIDsSelected        <- "all"
    
    # ret$LocIDsSelected        <- c(484, 840)
    
    # ret$LocIDsSelected        <- c(124, 208, 32, 356, 36, 392, 458, 484, 554, 702, 710, 716, 784, 792, 826, 840)  # ready for modelling
    # ret$LocIDsExcluded          <- c(124, 208, 32, 356, 36, 392, 458, 484, 554, 702, 710, 716, 784, 792, 826, 840)
    
    # ret$LocIDsSelected        <- 4
    # ret$LocIDsSelected        <- 158  # Taiwan
    # ret$LocIDsSelected        <- 728  # South Sudan
    
    # ret$LocIDsSelected        <- c(535)  # Caribbean Netherlands_BES_535
    # ret$LocIDsSelected        <- c(348)  # Hungary_HUN_348
    
    
    # !!!534  doesn't work
    
    # print(ret$LocIDsSelected)
    
    # 235 except completed
    
    # my, unassigned and not completed
    # e <- (Migrant_Stock_Project_Overview$Analyst == "KA" | is.na(Migrant_Stock_Project_Overview$Analyst)) & is.na(Migrant_Stock_Project_Overview$Completed)
    # v <- as.matrix(LocIDExclAanalysts <-  Migrant_Stock_Project_Overview$LocID[e])
    # paste(as.matrix(unlist(lapply(v, paste0, ","))))
    # Migrant_Stock_Project_Overview <- msglobalsf("Migrant_Stock_Project_Overview")
    ret$LocIDsSelected <- c(276,	643,	826,	36,	724,	380,	804,	392,	702,	756,	528,	56,	300,	566,	860,	112,	620,	180,	48,	372,	760,	578,	434,	688,	616,	854,	642,	24,	214,	348,	120,	203,	191,	188,	72,	524,	466,	288,	834,	266,	716,	446,	204,	178,	887,	246,	562,	108,	508,	591,	442,	768,	705,	762,	686,	275,	12,	630,	428,	226,	270,	233,	417,	795,	454,	196,	894,	478,	600,	68,	4,	638,	807,	324,	262,	254,	470,	96,	175,	516,	858,	504,	312,	140,	430,	320,	830,	316,	268,	116,	780,	104,	540,	499,	462,	474,	352,	44,	84,	788,	706,	531,	850,	694,	64,	533,	408,	8,	418,	740,	20,	833,	222,	558,	144,	340,	70,	450,	52,	748,	328,	598,	258,	28,	136,	480,	534,	492,	535,	438,	796,	388,	16,	92,	580,	496,	60,	332,	624,	132,	242,	232,	690,	174,	426,	292,	626,	662,	212,	659,	308,	234,	304,	660,	674,	732,	585,	670,	184,	882,	776,	584,	548,	296,	192,	583,	90,	520,	678,	876,	238,	500,	772,	666,	336,	570,	654,	798,	646,	652,	663)
    
    # ret$LocIDsSelected <- setdiff(ret$LocIDsSelected, c(840, 124, 208))
    # ret$LocIDsSelected <- ret$LocIDsSelected[(which(ret$LocIDsSelected == 535)+1):length(ret$LocIDsSelected)]
    
    # all except assigned to Analysts
    # Migrant_Stock_Project_Overview <- msglobalsf("Migrant_Stock_Project_Overview")
    # e <- Migrant_Stock_Project_Overview$Analyst == "KA" | is.na(Migrant_Stock_Project_Overview$Analyst)
    # LocIDExclAanalysts <-  Migrant_Stock_Project_Overview$LocID[e]
    # ret$LocIDsSelected <- LocIDExclAanalysts

    # ret$LocIDsSelected <- 535
    # ret$LocIDsSelected <- 352 # Iceland_ISL_352
    
    # ret$LocIDsSelected <- 392 # Japan
    
    # ret$LocIDsSelected <- 752
    # ret$LocIDsSelected <- 276  # Germany
    
    
    # ret$LocIDsSelected <- 400
    ret$LocIDsSelected <- 156
    ret$LocIDsSelected <- 204
    ret$LocIDsSelected <- 400
    ret$LocIDsSelected <- 792
    ret$LocIDsSelected <- 208
    
    ret$LocIDsSelected <- 204
    ret$LocIDsSelected <- 300
    
    # ret$LocIDsSelected <- 0
    
    # ret$LocIDsSelected        <- 792    # Turkey_TUR_792
    # ret$LocIDsSelected        <- "all"
    ret$LocIDsSelected <- 458  # Malaysia_458_MYS
    ret$LocIDsSelected <- 276
    ret$LocIDsSelected <- 756
    ret$LocIDsSelected <- 528
    ret$LocIDsSelected <- 616  # Poland
    
    ret$LocIDsSelected <- 56   # Belgium
    ret$LocIDsSelected <- 203  # Czechia_CZE_203
    ret$LocIDsSelected <- 372  # Ireland
    ret$LocIDsSelected <- 702  # Singapore_SGP_702
    ret$LocIDsSelected <- 642  # Romania_ROU_642
    ret$LocIDsSelected <- 643  # Russian Federation_RUS_643
    ret$LocIDsSelected <- 344  # China, Hong Kong SAR_HKG_344
    ret$LocIDsSelected <- 499  # Montenegro_MNE_499
    ret$LocIDsSelected <- 531  
    ret$LocIDsSelected <- 534  # Sint Maarten (Dutch part)_SXM_534
    ret$LocIDsSelected <- 728  # South Sudan_SSD_728:
    # ret$LocIDsSelected <- 826   # U.K.
    ret$LocIDsSelected <- 578     # Norway_NOR_578
    ret$LocIDsSelected <- 246   # Finland_FIN_246
    ret$LocIDsSelected <- 40
    
    # ret$LocIDsSelected <- 158   # 
    # ret$LocIDsSelected <- 586   # Pakistan_PAK_586
    
    # ret$LocIDsSelected        <- "all"  # 233 done
    
    # all my unassigned "blank" running all data 
    # ret$LocIDsSelected <- c(804,	398,	364,	50,	376,	170,	40,	729,	300,	862,	566,	860,	112,	404,	620,	180,	728,	760,	578,	434,	688,	854,	24,	214,	348,	120,	148,	191,	188,	646,	524,	466,	288,	834,	266,	716,	446,	204,	178,	887,	246,	562,	108,	508,	591,	442,	768,	705,	762,	686,	275,	12,	630,	428,	226,	270,	233,	417,	703,	795,	454,	196,	894,	100,	478,	600,	68,	440,	4,	638,	807,	324,	262,	254,	470,	96,	175,	72,	516,	858,	504,	312,	140,	430,	320,	830,	316,	268,	116,	780,	104,	540,	499,	462,	474,	352,	44,	84,	788,	706,	531,	850,	694,	64,	533,	408,	8,	418,	740,	20,	833,	222,	558,	144,	340,	70,	450,	52,	748,	328,	598,	258,	28,	136,	480,	534,	492,	535,	438,	796,	388,	16,	92,	580,	496,	60,	332,	624,	132,	242,	232,	690,	174,	426,	292,	626,	662,	212,	659,	308,	234,	304,	660,	674,	732,	585,	670,	184,	882,	776,	584,	548,	296,	192,	583,	90,	520,	678,	876,	238,	500,	772,	666,	336,	570,	654,	798,	652,	663)
    # # all
    # ret$LocIDsSelected <- c(4,	8,	12,	16,	20,	24,	28,	40,	44,	50,	52,	60,	64,	68,	70,	72,	84,	90,	92,	96,	100,	104,	108,	112,	116,	120,	132,	136,	140,	144,	148,	170,	174,	175,	178,	180,	184,	188,	191,	192,	196,	204,	212,	214,	222,	226,	232,	233,	234,	238,	242,	246,	254,	258,	262,	266,	268,	270,	275,	288,	292,	296,	300,	304,	308,	312,	316,	320,	324,	328,	332,	336,	340,	348,	352,	364,	376,	388,	398,	404,	408,	417,	418,	426,	428,	430,	434,	438,	440,	442,	446,	450,	454,	462,	466,	470,	474,	478,	480,	492,	496,	499,	500,	504,	508,	516,	520,	524,	531,	533,	534,	535,	540,	548,	558,	562,	566,	570,	578,	580,	583,	584,	585,	591,	598,	600,	620,	624,	626,	630,	638,	646,	652,	654,	659,	660,	662,	663,	666,	670,	674,	678,	686,	688,	690,	694,	703,	705,	706,	716,	728,	729,	732,	740,	748,	760,	762,	768,	772,	776,	780,	788,	795,	796,	798,	804,	807,	830,	833,	834,	850,	854,	858,	860,	862,	876,	882,	887,	894)
    
    # yet to be run 
    # ret$LocIDsSelected <- c(246,	254,	258,	262,	266,	268,	270,	275,	288,	292,	296,	300,	304,	308,	312,	316,	320,	324,	328,	332,	336,	340,	348,	352,	364,	376,	388,	398,	404,	408,	417,	418,	426,	428,	430,	434,	438,	440,	442,	446,	450,	454,	462,	466,	470,	474,	478,	480,	492,	496,	499,	500,	504,	508,	516,	520,	524,	531,	533,	534,	535,	540,	548,	558,	562,	566,	570,	578,	580,	583,	584,	585,	591,	598,	600,	620,	624,	626,	630,	638,	646,	652,	654,	659,	660,	662,	663,	666,	670,	674,	678,	686,	688,	690,	694,	703,	705,	706,	716,	728,	729,	732,	740,	748,	760,	762,	768,	772,	776,	780,	788,	795,	796,	798,	804,	807,	830,	833,	834,	850,	854,	858,	860,	862,	876,	882,	887,	894)
    
    # all my left 
    # ret$LocIDsSelected <- c(643,     826,     702,     528,     56,     372,     616,    642,     203)
    
    # ret$LocIDsSelected <- c(643, 826, 702, 528, 56, 372, 616, 642, 203, 756)
    # ret$LocIDsSelected <- c(702, 528, 56, 372, 616, 642, 203, 756) # will be sorted
    
    # overwrite globals
    
    # if(!is.null(msglobals_data)){
    #     if(exists("ret")){
    #         ret$CURRENT_DATA_FOLDER   <-  msglobals_data$CURRENT_DATA_FOLDER
    #         ret$LocIDsSelected        <-  msglobals_data$LocIDsSelected
    #         ret$LocIDsOriginSelected  <-  msglobals_data$LocIDsOriginSelected
    #     }
    # }    

    #@@@locid
    return(ret)
    
    # # create folders 
    # if (!dir.exists(msglobals[["PROJECTOUTPUTFOLDER"]])){
    #     dir.create(msglobals[["PROJECTOUTPUTFOLDER"]], showWarnings = FALSE)
    # }
    
}

# msglobalsf()
# msglobalsf("COUNTRIES")


############################################################################################
# # NOT USED ANYMORE read from Excel file e.g."C:\akf\DESA-POP-MUS\MS\countries\Spain\rev2023\724_Rev2023.xlsx"
# # browser()
# # LocID <- 724
# LocID <- 392
# REVISION_NAME <- "Rev2023"
# MIGRANT_STOCK_PROJECT_OVERVIEW <- paste0(ret$PROJECTFOLDER, "data/Migrant_Stock_Project_Overview.RData")
# load(MIGRANT_STOCK_PROJECT_OVERVIEW)
# df <- df[df$LocID == LocID,]
# fname <- paste0(ret$SHAREPOINTFOLDER, "countries/", df$WorkingFolder, "/", REVISION_NAME, "/", LocID, "_", REVISION_NAME, ".xlsx")
# msg <- paste0("Loading origins from ", fname)
# print(msg)
# dforgw <- read_excel(fname, sheet = "Origin");  # , guess_max = 235
# LocIDsOriginSelected <- dforgw[[1]]
# if(LocIDsOriginSelected[1] != "LocID"){
#     stop("LocID is expected")
# }
# LocIDsOriginSelected <- as.numeric(LocIDsOriginSelected[2:length(LocIDsOriginSelected)])
# ret$LocIDsOriginSelected <- LocIDsOriginSelected
###########################################################################################
