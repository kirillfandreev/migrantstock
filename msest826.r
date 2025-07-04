# Aggregate all single-age age structures in a dataset with empirical data into 5-year age groups
# TMPFUNC <- function (df) {
    # get variables to iterate over age structures (one can use map() etc but it is somewhat easier to debug in the script)

# msest1y.m    
    
# Estimation script for United Kingdom_GBR_826
# This script is created by Kirill Andreev (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)
# 2024/11/11

###########################################################################################################
# Initialize the environment  ---------------------------------------------------------------------

# delete all objects
rm(list=ls())
# Clear console
cat("\014")  # ctrl+L

# # close current plot
# # https://stackoverflow.com/questions/22640016/code-to-clear-all-plots-in-rstudio
# dvs <- dev.list()["RStudioGD"]
# if(!is.null(dvs)){dev.off(dvs)}

# dev.off(dev.list()["RStudioGD"])
# dev.off()

# https://stackoverflow.com/questions/63744905/attaching-packages-to-a-temporary-search-path-in-r
library("signal")
# pchip() is also in pracma: Practical Numerical Math Functions

library("tidyverse")
source("ms_source_all_functions.r", local = TRUE)

# put dplyr at the top
detach("package:dplyr", character.only = TRUE)
library("dplyr")

# search()
# detach("package:signal", character.only = TRUE)

# set working folder
setwd(msglobalsf("PROJECTFOLDER"))

print(commandArgs(trailingOnly = FALSE))

################################################################################
# Global variables  ------------------------------------------------------------

LocID <- 826

SCRIPT_NAME <- paste0("msest", LocID)

# create folders
#
folder_output <- msglobalsf("PROJECTOUTPUTFOLDER")
if (!dir.exists(folder_output)){
    dir.create(folder_output, showWarnings = FALSE)
}
script_output_folder <- paste0(folder_output, SCRIPT_NAME, "/")
if (!dir.exists(script_output_folder)){
    dir.create(script_output_folder, showWarnings = FALSE)
}

script_output_folder <- paste0(folder_output, SCRIPT_NAME, "/")
script_output_folder_SP <- paste0(msglobalsf("SHAREPOINTFOLDER"), "processing/", "ms_total_est", "/")

PU_FOREIGNERS                   <- msglobalsf("PU_FOREIGNERS")
PU_FOREIGNBORN                  <- msglobalsf("PU_FOREIGNBORN")
PU_FOREIGNBORN_PU_FOREIGNERS    <- msglobalsf("PU_FOREIGNBORN_PU_FOREIGNERS")
PU_ASYLUM_SEEKERS               <- msglobalsf("PU_ASYLUM_SEEKERS")
PU_REFUGEES                     <- msglobalsf("PU_REFUGEES")
PU_VENEZUELANS_DISPLACED_ABROAD <- msglobalsf("PU_VENEZUELANS_DISPLACED_ABROAD")
PU_INTERNATIONAL_MIGRANTS       <- msglobalsf("PU_INTERNATIONAL_MIGRANTS")
PU_OTHER_PEOPLE_IN_NEED         <- msglobalsf("PU_OTHER_PEOPLE_IN_NEED")
PU_STATELESS                    <- msglobalsf("PU_STATELESS")

LAST_YEAR <- msglobalsf("LAST_YEAR_PROJECTION")

EMPIRICALFOLDER <- msglobalsf("EMPIRICALFOLDER")
ESTIMATED_SERIES_NAME <- msglobalsf("ESTIMATED_SERIES_NAME")
SERIES_9P <- paste0(ESTIMATED_SERIES_NAME, "_", PU_INTERNATIONAL_MIGRANTS, "P")

# years for which migrant stocks are published
YEARS_PUBLISHED <- msglobalsf("YEARS_PUBLISHED")

# log file
logfile <- paste0(script_output_folder, LocID, "_log.txt");
if (file.exists(logfile)){unlink(logfile, force = TRUE)}
msglobalsf("LOGFILE", logfile) # current log file

################################################################################

# Countries of origin to run, see msglobals_KA.r
COUNTRIES_ORIGIN <- msglobalsf("COUNTRIES_ORIGIN"); TNamesOrigin <- COUNTRIES_ORIGIN$LocID;

# total origin must be included
if(!is.element(0, TNamesOrigin)) {stop("no total included -- check msglobalsf.r");}

# msg <- paste0("\n",  niso3locid, " --------------------------- \n"); fh <- file(logfile, open = "at"); cat(msg, append = T, file = fh); close(fh);
time_start <- Sys.time()

# Load empirical data
# Select immigrant (foreign-born population only) in the empirical data
df <- loadEmpiricalData("empirical", LocID = LocID)
df <- df[, c("LocID", "sex", "yearref", "year", "age", "agelength", "LocIDorg", "PopulationUniverse", "value", "source")] # delete redundant columns to speed things up
df <- df %>% filter(df$PopulationUniverse == PU_FOREIGNBORN)

# remove UNHCR data for now ... 
df <- df %>% filter(!(grepl("^UNHCR", df$source))) # delete

# save local empirical data
ofname <- paste0(script_output_folder, LocID, "_EmpiricalData.ms.RData")
save(df, file = ofname)
print(paste0("Saved: ", ofname))
# load empirical data to update all cache variables
dfemp <- loadEmpiricalData("msest")

# compute proportion of females from the dataset with the local empirical data
# dfemp <- df
dftmp <- dfemp %>% filter(age == 0 & agelength == 0)
dfprpf <- msempprpf(dftmp)

# stop("115")

################################################################################
# Totals -----------------------------------------------------------------------
dftot <- msglobalsf("EmpiricalDataTotal")               # msglobalsf("EmpiricalDataFileName")
dfest <- msinterpextrapol(dftot, iter = "total")
# dfest1y <- dfest$dfest
dftotpub <- dfest$dfestpub
print(dftotpub)

################################################################################
# Proportion of females, Totals  -------------------------------------------

# Run ms_prpf.r to create a datatset of sex ratios
# fname <- paste0(msglobalsf("INDICATORSFOLDER"), LocID, '_', msglobalsf("INDIC_PRPF"), '.RData')  # "C:/akf/DESA-POP-MUS/MS/processing/empindic/124_1.RData"
# load(fname) # produced by ms_prpf.r

df <- dfprpf  # proportion of females
df <- df[df$age == 0 & df$agelength == 0 & df$LocIDorg == 0,]
df <- df %>% filter(df$PopulationUniverse == PU_FOREIGNBORN)
# delete Rev2020, Rev2023 series ... and sort
df <- df %>% filter(!grepl("^Rev[0-9]+", df$source, ignore.case = TRUE)) %>% arrange(yearref)

# produces quite high proportion of female. Negatively affects all sex-specific trends by country of origin.
# setupest <- NULL
# # setupest$weights <- data.frame(series = "^Census", weight = 100)
# dftotprpfest <- msprpfest(df, "total", dfest, setupest)  # logistic regression


# estimates
df$value <- mslogit(df$value)

prjprmf <- NULL
prjprmf$method <- "LinearGrowth"
prjprmf$dTfit  <- 5
prjprmf$output_bounds  <- c(-Inf, +Inf)

prjprmb <- NULL
prjprmb$method <- "ConstValue"
prjprmb$datapoints  <- 1
prjprmb$output_bounds  <- c(-Inf, +Inf)

dftotprpfest <- msinterpextrapol(df, iter = "total", prjprmf, prjprmb, roundprm = FALSE, datarange_method = "pchip")

dftotprpfest$dfest$value <- mslogitinv(dftotprpfest$dfest$value)
dftotprpfest$dfestpub$value <- mslogitinv(dftotprpfest$dfestpub$value)

dftotprpfest$dmfpub <- dftotprpfest$dfestpub

tmp <-  dftotprpfest$dmfpub %>% select(yearref, value) %>% mutate(sex = 2)
tmp$value <- dfest$dfestpub$value * dftotprpfest$dfestpub$value
dftotprpfest$dmfpub <- tmp
tmp$value <- dfest$dfestpub$value - tmp$value
tmp$sex <- 1
dftotprpfest$dmfpub <- rbind(dftotprpfest$dmfpub, tmp)

# save output file
dftmfpub <- rbind(dftotpub %>% select(yearref, value) %>% mutate(sex = 0), dftotprpfest$dmfpub %>% select(sex, yearref, value))
dftmfpub <- dftmfpub %>% arrange(sex, yearref)
htmfile <- paste0(script_output_folder, LocID, "_total_published.htm")
totalpubhtm(htmfile, dftmfpub)
print(htmfile)

# stop("~~~total~~~")
# browser()

################################################################################
# Origins (Totals) -------------------------------------------------------------
# Need to select countries of origin to run in advance – check the report first
# ~90%  C:\Users\Kirill\United Nations\DESA-POP - MUS\MS\countries\Canada\rev\Origins.xlsx
# TNamesOrgs <- c(356,	608,	156,	826,	840,	586,	344,	380,	364,	704,	388,	410,	144,	616,	620,	276,	250,	332,	760,	422,	484,	642,	328,	368,	643,	504,	566,	804,	170,	12,	528,	818,	50,	158,	780,	4,	300,	710,	222,	76,	231,	191,	70,	792,	180,	706,	688,	604,	232,	348,	392,	682,	784,	376,	372,	404,	120,	152,	862,	288,	36,	458,	242,	788,	116,	834,	498,	192)
# TNamesOrgs <- c(840)
# TNamesOrgs <- msglobalsf("COUNTRIESALL");

TNamesOrgs <- msglobalsf("COUNTRIES_ORIGIN");
TNamesOrgs <- sort(TNamesOrgs$LocID); TNamesOrgs <- setdiff(TNamesOrgs, LocID); length(TNamesOrgs);
# TNamesOrgs <- c(654)

# retrieve empirical data from cache 
dfemp <- msglobalsf("EmpiricalData")
dfemp <- dfemp[dfemp$sex == 0 & dfemp$age == 0 & dfemp$agelength == 0 & dfemp$LocIDorg > 0, ]
dfemp <- dfemp[is.element(dfemp$LocIDorg, TNamesOrgs),]
# dfemp <- dfemp %>% filter(LocIDorg == 894) 

# # run the "median" constant growth rate projections
# prjprmfp <- NULL
# prjprmfp$method <- "median"
# dforgest <- msinterpextrapol(dfemp, iter = "origin", prjprmf = prjprmfp, prjprmb = prjprmfp)

# Linear Growth using the last two censuses 

# for fprward projection
prjprmfp <- NULL
prjprmfp$method <- "LinearGrowth"
# prjprmf$dTfit  <- 5
prjprmfp$datapoints  <- 2

#prjprmf$output_bounds  <- c(-Inf, +Inf)

# for backward projection
prjprmbp <- NULL
prjprmbp$method <- "LinearGrowth"
prjprmbp$datapoints  <- 2 # constant
# prjprmb$method <- "LinearGrowth"
# prjprmb$method <- "ConstValue" # constant
# prjprmb$datapoints  <- 1 # constant
# prjprmb$method <- "LinearGrowth"
# prjprmb$dTfit  <- 5
#prjprmb$output_bounds  <- c(-Inf, +Inf)
dforgest <- msinterpextrapol(dfemp, iter = "origin", prjprmf = prjprmfp, prjprmb = prjprmbp)

# dforg1y  <- dforgest$dfest
# dforgpub <- dforgest$dfestpub

# # msest1ya.m, msest1yb.m
# fname <- "C://akf//MUS//MS//Projects//origins//616_df1y_adjf.csv"
# dfflowadj <- read.csv(fname)
# dfflowadj <- dfflowadj %>% filter(!(source == "OTH")) # drop residual
# dfflowadj$PopulationUniverse <- 3
# dforgest$dfest <- dforgest$dfest[dforgest$dfest$yearref < min(dfflowadj$yearref),]
# dforgest$dfest <- rbind(dforgest$dfest, dfflowadj[, c("yearref", "value", "LocIDorg", "PopulationUniverse", "source")])
# dforgest$dfest <- dforgest$dfest %>% arrange(LocIDorg, yearref)
# # dforgest$dfest %>% filter(LocIDorg == 12)
# # unique((dforgest$dfest$yearref))
# dforgest$dfestpub <- NULL
# LocIDorgu <- unique(dforgest$dfest$LocIDorg)
# for(i in 1:length(LocIDorgu)){
#     tmp <- dforgest$dfest %>% filter(LocIDorg == LocIDorgu[i])
#     tmp <- msestpublished(tmp)    
#     tmp$LocIDorg <- LocIDorgu[i];
#     dforgest$dfestpub <- rbind(dforgest$dfestpub, tmp)
# }

###############################################################################
# Proportion of females, origins  -------------------------------------------
df <- dfprpf
df <- df[df$age == 0 & df$agelength == 0 & df$LocIDorg > 0,]
df <- df[is.element(df$LocIDorg, TNamesOrgs),]  # only origins we publish
# delete Rev2020, Rev2023 series ... and sort
df <- df %>% filter(!grepl("^Rev[0-9]+", df$source, ignore.case = TRUE)) %>% arrange(yearref)
# df <- df %>% filter(LocIDorg == 50) 

# browser()

# estimates
df <- df %>% filter(df$value > 0 & df$value < 1)
df$value <- mslogit(df$value)

prjprmf <- NULL
prjprmf$method <- "LinearGrowth"
prjprmf$dTfit  <- 5
prjprmf$output_bounds  <- c(-Inf, +Inf)

prjprmb <- NULL
prjprmb$method <- "ConstValue"
prjprmb$datapoints  <- 1
prjprmb$output_bounds  <- c(-Inf, +Inf)

# msglobalsf("UserData", "prpforg")
# For the rest of the parameters the default values are used: yjumpoff, vjumpoff, year_last 

dforgprpfest <- msinterpextrapol(df, iter = "origin", prjprmf, prjprmb, roundprm = FALSE, datarange_method = "pchip")

dforgprpfest$dfest$value <- mslogitinv(dforgprpfest$dfest$value)
dforgprpfest$dfestpub$value <- mslogitinv(dforgprpfest$dfestpub$value)

# # Published estimates of trends by origin by sex 
# dftmforgpub <- NULL 
# LocIDorgu <- unique(c(dforgest$dfestpub$LocIDorg, dforgprpfest$dfestpub$LocIDorg))
# for(li in 1:length(LocIDorgu)){
#     
#     dforgesti   <- dforgest$dfestpub[dforgest$dfestpub$LocIDorg == LocIDorgu[li],]
#     if(nrow(dforgesti) == 0){
#         msg <- paste0(LocIDorgu[li], " - no esitmates of origin found")
#         stop(msg)
#     }
#     
#     dforgprpfi  <- dforgprpfest$dfestpub[dforgprpfest$dfestpub$LocIDorg == LocIDorgu[li],]
#     if(nrow(dforgprpfi) == 0){
#         # browser()
#         msg <- paste0(LocIDorgu[li], " - no proportion of females found")
#         message(msg)
#         dforgprpfi <- dforgesti
#         dforgprpfi$value <- 0.5 
#     }
#     
#     if(!all(dforgesti$yearref == dforgprpfi$yearref)){
#         stop("dforgprpfi error")
#     }
#     
#     dftmp <- dforgesti
#     dftmp$sex <- 0
#     dftmforgpub <- rbind(dftmforgpub, dftmp)
#     dftmpt <- dftmp
#     # females
#     dftmp$value <- dforgesti$value * dforgprpfi$value
#     dftmp$value <- round(dftmp$value)
#     dftmp$sex <- 2
#     dftmforgpub <- rbind(dftmforgpub, dftmp)
#     dftmpf <- dftmp
#     # males
#     dftmp$sex <- 1
#     dftmp$value <- dftmpt$value - dftmpf$value
#     dftmforgpub <- rbind(dftmforgpub, dftmp)
#     
#     if(any(dftmp$value < 0) || any(dftmpt$value < 0) || any(dftmpf$value < 0)){
#         stop("value must be positive!")
#     }
# }

# # save single year estimates
# df1y <- dfest$dfest[, c("yearref", "value", "source")]
# df1y$LocIDorg <- 0
# df1y <- rbind(df1y, dforgest$dfest[, c("yearref", "value", "source", "LocIDorg")])
# df1y$sex <- 0
# ofname <- paste0(script_output_folder, LocID, "_df1y.csv")
# write.csv(df1y, file = ofname, quote=TRUE, row.names = FALSE)
# print(ofname)

#!!!! HERE ipf_script.m to keep M/F residual updated !

# # single year series for males and females
# msorg1y <- msms1y(dforgest$dfest, dforgprpfest$dfest)
# # v <- msorg1y %>% filter(LocIDorg == 32 & sex == 0) # % yearref == 2024

# single year series for males and females
msorg1y <- msms1y(dforgest$dfest, dforgprpfest$dfest)
# v <- msorg1y %>% filter(LocIDorg == 32 & sex == 0) # % yearref == 2024

# compute flows
mforg1y <- msms2mf(msorg1y)
v <- mforg1y %>% filter(LocIDorg == 32 & sex == 0 & yearref <= 2024) # % 

# update flows for Ukraine 

# load pre-computed UNHCR flows 
LocIDorgi <- 804
fname <- paste0(msglobalsf("SHAREPOINTFOLDER"), "data/UNHCR/DemographicsOrg/demographics.xlsx")  #  "C:/akf/DESA-POP-MUS/MS/data/UNHCR/DemographicsOrg/demographics.xlsx"
unhcrd <- read_excel(fname, sheet = "flows", guess_max = 235)
unhcrd <- unhcrd %>% filter(unhcrd$LocID == !!LocID & unhcrd$LocIDorg == LocIDorgi & unhcrd$sex %in% c(1,2) & yearref >= 2022 & yearref <= msglobalsf("LAST_YEAR_PROJECTION"))
if(nrow(unhcrd) == 0){stop("no now unhcr data found")}

# update flows with UNHCR's
# check LocIDorgi only
# e <- mforg1y$LocIDorg == LocIDorgi
# mforg1yoth <-  mforg1y[!e,]
# mforg1y    <-  mforg1y[e,]
# browser()
mforg1y <- msmfupdate(mforg1y, unhcrd)

# compute stocks from the updated flows
msorg1y <- msmf2ms(mforg1y, msorg1y)

# write.csv(msorg1y, file="C:/temp/df.csv", quote=TRUE, row.names = FALSE)

# compute 5-year the published series
dftmforgpub <- msms1y2pub(msorg1y)

#!!!fixme UNHCR flows from Ukraine should be included in the total count (a step we currently omit, unlike for Poland) because the number of Ukrainian refugees is very low compared to the total foreign-born population.

# adjust estimates to make the residual be positive (sum of origins < total)
# dftmforgpub <- mstotoriginadj(dftmfpub, dftmforgpub)

# save output file
# dftmforgpub <- dftmforgpub %>% filter(dftmp$LocIDorg == 4)
dftmforgpub <- dftmforgpub %>% arrange(sex, LocIDorg, yearref)
# dftmforgpub <- dforgprpfest$dfestpub
htmfile <- paste0(script_output_folder, LocID, "_origin_published.htm")
originpubhtm(htmfile, dftmforgpub)

# copy link for Excel
write.table(str_replace_all(htmfile, "/", "\\\\"), "clipboard", sep="\t", row.names=FALSE, col.names=FALSE,  quote=FALSE)
cat("\n")
cat(crayon::bold(crayon::green(htmfile)))

# # save R format
# fname = "C:/temp/r.RData"
# save(dftmfpub, dftmforgpub, file = fname)
