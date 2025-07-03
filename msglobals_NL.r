# global variables for user MunSim.Lai, UN notebook
msglobals_NL <- function() {
    
    ret <- NULL
    ret$UserName <- "NL"
    
    
    ret$SHAREPOINTFOLDER    <- "C:/akf/DESA-POP-MUS/"
    # ret$SHAREPOINTFOLDER      <- "C:/akf/DESA-POP-MUS/MS/" 
    ret$EMPIRICALFOLDERALL  <- "C:/akf/DESA-POP-MUS/processing/empiricalall/"
    ret$EMPIRICALFOLDER     <- "C:/akf/DESA-POP-MUS/processing/empirical/"
    ret$PROJECTFOLDER           <- "C:/akf/mus/MS/scripts/migrantstock/"            # the folder with R scripts etc. 
        
    # ret$SHAREPOINTFOLDER           <- "C:/akf/mus/MS_SP/"            # the folder with R scripts etc. 
    
    # ret$SHAREPOINTFOLDER        <- "C:/akf/DESA-POP-MUS/MS/users/yh/MS/"
    ret$CURRENT_DATA_FOLDER <- "C:/akf/DESA-POP-MUS/MS/processing/empirical/"    
        
        # # SharePoint (SP)
        
        # ret$EMPIRICALFOLDERALL                  <- "C:/akf/DESA-POP-MUS/MS/processing/empiricalall/"
        
        # ret$EMPIRICALFOLDER                     <- "C:/akf/DESA-POP-MUS/MS/processing/empirical/"
        
        
        
        # local folders
        
        # ret$EMPIRICALFOLDER      <- "C:/akf/MUS/MS/Empirical/"
        
        # ret$EMPIRICALFOLDERALL   <- "C:/akf/MUS/MS/empiricalall/"
        
        
        
        # all including total 236
        
        LocIDorgsAll <- c(0, 4,              8,            12,         16,         20,         24,         28,         31,         32,         36,         40,         44,                48,         50,         51,         52,         56,         60,         64,         68,         70,         72,         76,         84,         90,         92,                96,         100,       104,       108,       112,       116,       120,       124,       132,       136,       140,       144,       148,       152,                156,       158,       170,       174,       175,       178,       180,       184,       188,       191,       192,       196,       203,       204,                208,       212,       214,       218,       222,       226,       231,       232,       233,       234,       238,       242,       246,       250,                254,       258,       262,       266,       268,       270,       275,       276,       288,       292,       296,       300,       304,       308,                312,       316,       320,       324,       328,       332,       336,       340,       344,       348,       352,       356,       360,       364,                368,       372,       376,       380,       384,       388,       392,       398,       400,       404,       408,       410,       414,       417,                418,       422,       426,       428,       430,       434,       438,       440,       442,       446,       450,       454,       458,       462,                466,       470,       474,       478,       480,       484,       492,       496,       498,       499,       500,       504,       508,       512,                516,       520,       524,       528,       531,       533,       534,       535,       540,       548,       554,       558,       562,       566,                570,       578,       580,       583,       584,       585,       586,       591,       598,       600,       604,       608,       616,       620,                624,       626,       630,       634,       638,       642,       643,       646,       652,       654,       659,       660,       662,       663,                666,       670,       674,       678,       682,       686,       688,       690,       694,       702,       703,       704,       705,       706,                710,       716,       724,       728,       729,       732,       740,       748,       752,       756,       760,       762,       764,       768,                772,       776,       780,       784,       788,       792,       795,       796,       798,       800,       804,       807,       818,       826,                830,       833,       834,       840,       850,       854,       858,       860,       862,       876,       882,       887,       894);
        
        
        
        ##################################################################################################################################################################
        
        #                       Origin
        
    
        
        # ret$LocIDsOriginSelected <- NULL                # no specific countries of origins, total only
        
        #ret$LocIDsOriginSelected<- "all"                # countries of origins 235
        
        #Sweden cp90
        #ret$LocIDsOriginSelected  <- c(760,	368,	246,	616,	364,	4,	706,	356,	70,	232,	792,	276,	764,	156,	578,	642,	208,	826,	586,	422,	152,	643,	840,	231,	704,	300,	688,	440,	608,	380,	348,	804,	528,	50,	724,	250,	170,	76,	191,	504,	410,	818,	8,	807,	428,	100,	729,	144,	233,	275,	566,	604,	682,	180,	496)
         
        #Saudi 682  (countries with at least 2 data points)
        #ret$LocIDsOriginSelected <- c(356,360,586,50,818,760,887,608,144,524,4,729,104,400,231,422,368,706,275,800,404,566,504,232)
        #final for SAUDI
       ret$LocIDsOriginSelected <- c(356,360,586,50,818,760,887,608,144,524,4,729,104,400,231,422,368,706,275)
        
        
        
        # ret$LocIDsOriginSelected <- c(4,   32,         50,         76,         104,       116,       124,       156,       158,       170,       192,                214,       218,       222,       231,       250,       276,       288,       300,       320,       328,       332,       340,       344,       356,                364,       368,       372,       376,       380,       388,       392,       404,       410,       418,       422,       484,       524,       558,                566,       586,       591,       604,       608,       616,       620,       630,       642,       643,       704,       710,       724,       764,                780,       792,       804,       818,       826,       862)
       # ret$LocIDsExcluded          <- NULL
        
      ###################################################  
        
    
        
       ret$LocIDsSelected <- 682 # SAU
        
       # ret$LocIDsSelected <- 784 # UAE
        
        #ret$LocIDsSelected <- 414 # Kuwait
        
        #ret$LocIDsSelected <- 512 # Oman
        
        #ret$LocIDsSelected <- c(752,40,554) # Sweden Austria New Zealnd 
        
        #ret$LocIDsSelected <- 752 #Sweden
        
        #ret$LocIDsSelected <- 40 #Austria
        
        #Generating country codes for the region
        #library(readxl)
        #file_path <- "C:/akf/mus/MS/scripts/migrantstock/NL/WPP2024_F01_LOCATIONS.xlsx"
        #dfa <- read_excel(file_path, sheet = "Location", skip = 17, col_names = FALSE)
        #colnames(dfa)[7]<-"LocID"
        #colnames(dfa)[17]<-"SDG"
        #colnames(dfa)[18]<-"Region"
        #colnames(dfa)[16]<-"SDGcode"
        #test <- filter(dfa, Region==903)
        #test <- filter(dfa, SDGcode==947)
        #test$LocID <- as.numeric(test$LocID)
        #unique(test$LocID)
       
        #Arfica region
        #ret$LocIDsSelected <- c( 108,174,262,232,231,404,450,454,480,175,508,638,646,690,706,	728,	800,	834,	894,	716,24,	120,140,148,178,180,226,266,678,12,818,434,504,729,788,732,	72,	748,426,516,	710,	204,	854,	132,	384,	270,	288,	324,	624,	430,	466,	478,562,	566,	654,	686,	694,	768	)

        return(ret)
        
    }
    
    
    
    
    
    