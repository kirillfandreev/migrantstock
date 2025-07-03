# @@@1
# @@@plotly
# @@@CountrySelection
# output$plotageempirical

# migrantstock shiny application 
#
# Revision history: 
# 
# 2023/5/27
#   Bug fixes.  
#   Selection of series now works.
#   Animation now works
#   Colors for estimates changed a little to be distinguishable
#   Kirill Andreev (kirillandreev.com and https://www.linkedin.com/in/kirill-andreev-8bb12362)

# output$plotage
# @@@testtotalButton
# @@@testButton
# @@@testAgeButton

#!!!todo
# USe selectedID to select a country in the list!
# create a single function for plotting: observeEvent(input$CountrySelectionOrigin, {, single function for styling 
# styles for series
# Shiny one country troubles 
#fixme plotting of oring the same as total (loop over PU)

library("tidyverse")
library("shiny")
library("plotly")
library("data.table")

# P__ <- NULL  #!!!

# rm(list=ls())

# Loads functions and global variables
source("loadempiricaldata.r", local = TRUE)
source("msglobalsf.r", local = TRUE)
source("agesingle.r", local = TRUE)
source("msestdatarangex.r", local = TRUE)
source("locid2longname.r", local = TRUE)
source("ageregular.r", local = TRUE)


# source("agelen2agestr.r", local = TRUE)

# EMPIRICALDATATOTAL <- NULL
    
# search 
# observeEvent(input$CountrySelection
# https://migrantstock.shinyapps.io/migrantstock/
# server <- function(input, output, session){
# 
#   # onClick listener, same for all examples
#   observeEvent(input$testButton, {
#     print("input$testButton")
#     
#     # output$txt_example1 <- renderText({ "xxxxxxx" })
#     
#     # rv$x  <- rv$x + 1 # Example 1, update reactive list
#     # x     <<- x + 1   # Example 2, Save x to parent enviroment
#     # 
#     # # Example 3
#     # x2 <<- x2 + 1     # Again, save x2 to parent enviroment, see help('<<-') 
#     # printUpdate()     # Update textOutput
#     
#     output$testtOutput <- renderText({ 1 })
#   })
# } # end of  server <- function(input, output, session){


msplotlystyle <- function(seriesname) {

# print(seriesname)
    
# styling
# https://htmlcolorcodes.com/
# https://codepen.io/etpinard/pen/LLZGZV
# https://html-color.codes/red
    
ESTIMATED_SERIES_NAME <- msglobalsf("ESTIMATED_SERIES_NAME") # Rev2023

rftpu <- msglobalsf("rftPopulationUniverse")
    
l <- list()

# if (identical(seriesname, "NSO")){

# browser()

#!!!fixme -- add UNHCR OTHERS
PU_FOREIGNBORN_LABEL       <- rftpu$Label[match(msglobalsf("PU_FOREIGNBORN"), rftpu$ID)]
PU_FOREIGNERS_LABEL        <- rftpu$Label[match(msglobalsf("PU_FOREIGNERS"), rftpu$ID)]
PU_ASYLUM_SEEKERS_LABEL    <- rftpu$Label[match(msglobalsf("PU_ASYLUM_SEEKERS"), rftpu$ID)]
PU_REFUGEES_LABEL          <- rftpu$Label[match(msglobalsf("PU_REFUGEES"), rftpu$ID)]
PU_VENEZUELANS_DISPLACED_ABROAD_LABEL <- rftpu$Label[match(msglobalsf("PU_VENEZUELANS_DISPLACED_ABROAD"), rftpu$ID)]
PU_INTERNATIONAL_MIGRANTS_LABEL <- rftpu$Label[match(msglobalsf("PU_INTERNATIONAL_MIGRANTS"), rftpu$ID)]
PU_OTHER_PEOPLE_IN_NEED_LABEL <- rftpu$Label[match(msglobalsf("PU_OTHER_PEOPLE_IN_NEED"), rftpu$ID)]

#   msglobalsf("")

if ((startsWith(seriesname, 'NSO') | startsWith(seriesname, 'SYB')) & endsWith(seriesname, PU_FOREIGNBORN_LABEL)){
    # triangle square triangle'
    l$marker <- list(symbol = "triangle-up", color = 'rgb(0, 255, 255)', size = 10, line = list(color = 'rgb(0, 0, 128)', width = 1))
    #l$line <- list(color = 'rgb(0, 0, 128)')
    # l$line <- list(color = 'rgb(0,127,255)')
    l$line <- list(color = 'rgb(64,224,208)')
}else if((startsWith(seriesname, 'NSO') | startsWith(seriesname, 'SYB')) & endsWith(seriesname, PU_FOREIGNERS_LABEL)){
    # l$marker <- list(symbol = "triangle-up", color = 'rgb(0, 139, 139)', size = 10, line = list(color = 'rgb(0, 0, 128)', width = 1))
    l$marker <- list(symbol = "triangle-up", color = 'rgb(0,206,209)', size = 10, line = list(color = 'rgb(0, 0, 128)', width = 1))
    #l$line <- list(color = 'rgb(0, 0, 128)')
    l$line <- list(color = 'rgb(0,127,255)')
}else if(identical(seriesname, "Rev2020") | identical(seriesname, "UNPD_2020")){
    l$marker <- list(symbol = "triangle-down", color = 'rgb(240,128,128)', size = 10, line = list(color = 'rgb(0, 0, 0)', width = 1))
    l$line <- list(color = 'rgb(240,128,128)')
}else if(identical(seriesname, paste0("UNHCR", '|', PU_ASYLUM_SEEKERS_LABEL))){
    l$marker <- list(symbol = "square", color = 'rgb(250, 250, 51)', size = 7, line = list(color = 'rgb(255, 0, 0)', width = 1))
    l$line <- list(color = 'rgb(250, 250, 51)')
}else if(identical(seriesname, paste0("UNHCR", '|', PU_OTHER_PEOPLE_IN_NEED_LABEL))){
    l$marker <- list(symbol = "square", color = 'rgb(250, 250, 51)', size = 7, line = list(color = 'rgb(0, 190, 0)', width = 1))
    l$line <- list(color = 'rgb(250, 250, 51)')
}else if(identical(seriesname, paste0("UNHCR", '|', PU_REFUGEES_LABEL))){
    l$marker <- list(symbol = "square", color = 'rgb(250, 250, 51)', size = 7, line = list(color = 'rgb(0, 0, 0)', width = 1))
    l$line <- list(color = 'rgb(250, 250, 51)')
}else if(identical(seriesname, paste0("UNHCR", '|', PU_VENEZUELANS_DISPLACED_ABROAD_LABEL))){
    l$marker <- list(symbol = "square", color = 'rgb(250,250,250)', size = 6, line = list(color = 'rgb(102,102,0)', width = 1))
    l$line <- list(color = 'rgb(112,128,144)')
}else if(startsWith(seriesname, "UNSDDYB_B09") | startsWith(seriesname, "UNSDDYB_B59") | startsWith(seriesname, "Census") | startsWith(seriesname, "CENSUS") & endsWith(seriesname, PU_FOREIGNBORN_LABEL)){
    # census
    l$marker <- list(symbol = "square", color = ' rgb(0,255,255)', size = 8, line = list(color = ' rgb(0,0,128)', width = 1))
    # l$line <- list(color = ' rgb(0,255,255)')
    l$line <- list()
}else if(startsWith(seriesname, "UNSDDYB_B61") | startsWith(seriesname, "UNSDDYB_B62") | startsWith(seriesname, "Census") | startsWith(seriesname, "CENSUS") & endsWith(seriesname, PU_FOREIGNERS_LABEL)){
    # l$marker <- list(symbol = "triangle-up", color = 'rgb(0, 139, 139)', size = 10, line = list(color = 'rgb(0, 0, 128)', width = 1))
    l$marker <- list(symbol = "square", color = 'rgb(0,206,209)', size = 8, line = list(color = 'rgb(0, 0, 128)', width = 1))
    #l$line <- list(color = 'rgb(0, 0, 128)')
    l$line <- list()
}else if(startsWith(seriesname, "Eurostat")){
    l$marker <- list(symbol = "circle", color = ' rgb(255,204,153)', size = 8, line = list(color = ' rgb(153,51,0)', width = 1))
    l$line <- list(color = ' rgb(255,204,153)')
    l$mode <- 'lines'
}else if(startsWith(seriesname, "OECD") & endsWith(seriesname, PU_FOREIGNERS_LABEL)){
    l$marker <- list(symbol = "triangle-down", color = ' rgb(147, 92, 156)', size = 9, line = list(color = ' rgb(63,127,127)', width = 1))
    l$line <- list(color = ' rgb(63,127,127)')
    l$mode <- 'lines'
#else if(startsWith(seriesname, "Rev2023")){
#     l$marker <- list(symbol = "triangle-down", color = ' rgb(186,212,244)', size = 8, line = list(color = ' rgb(63,127,127)', width = 1))
#     l$line <- list(color = ' rgb(63,127,127)')
#     l$mode <- 'lines'
#}else if(startsWith(seriesname, paste0(msglobalsf("ESTIMATED_SERIES_NAME"), "_"))){
    # l$line <- list(color = ' rgb(0,255,0)')
    # l$mode <- 'lines'
}else if(startsWith(seriesname, "OECD") & endsWith(seriesname, PU_FOREIGNBORN_LABEL)){
    l$marker <- list(symbol = "triangle-up", color = ' rgb(186,212,244)', size = 9, line = list(color = ' rgb(63,127,127)', width = 1))
    l$line <- list(color = ' rgb(63,127,127)')  
    l$mode <- 'lines'
}else if(startsWith(seriesname, "WorldBank")){
    l$marker <- list(symbol = "circle", color = ' rgb(255, 0, 255)', size = 8, line = list(color = ' rgb(0, 0, 128)', width = 1))
    l$line <- list(color = ' rgb(255, 182, 193)')
    l$mode <- 'lines'
}else{
    l$marker <- list()
    l$line <- list()
}

# estimates 
if(identical(seriesname, ESTIMATED_SERIES_NAME) | identical(seriesname, "UNPD_2024")){ # PU_INTERNATIONAL_MIGRANTS_LABEL
    l$marker <- list(symbol = "triangle-up", color = 'rgb(0, 255, 0)', size = 10, line = list(color = 'rgb(0, 128, 0)', width = 1))
    l$line <- list(color = 'rgb(0,128,0)', width = 2)
}else
if(identical(seriesname, paste0(ESTIMATED_SERIES_NAME, '_', msglobalsf("PU_INTERNATIONAL_MIGRANTS")))){
    l$marker <- list()
    l$mode <- 'lines'
    # width = 0 -- invisible
    l$line <- list(color = 'rgb(0,255,0)', width = 2)
}else
if(identical(seriesname, paste0(ESTIMATED_SERIES_NAME, '_', msglobalsf("PU_INTERNATIONAL_MIGRANTS"), "P"))){
    l$marker <- list(symbol = "square", color = 'rgb(0, 128, 0)', size = 5, line = list(color = 'rgb(0, 128, 0)', width = 1))
    # l$line <- list(color = 'rgb(0,128,0)', width = 2)
    l$mode <- 'scatter'
}else 
if(identical(seriesname, paste0(ESTIMATED_SERIES_NAME, '_', msglobalsf("PU_FOREIGNBORN")))){ #!!!fixme remove
    l$line <- list(color = 'rgb(0, 220, 0)', width = 1)
    l$marker <- list()
    l$mode <- 'lines'
}else 
if(identical(seriesname, paste0(ESTIMATED_SERIES_NAME, '_', msglobalsf("PU_FOREIGNERS")))){
    l$line <- list(color = 'rgb(128,128,0)', width = 1) # olive = rgb(128, 128, 0)
    l$marker <- list()
    l$mode <- 'lines'
}else 
if(identical(seriesname, paste0(ESTIMATED_SERIES_NAME, '_', msglobalsf("PU_REFUGEES")))){
    l$line <- list(color = 'rgb(22,160,133)', width = 1)   # rgb(0,255,0)
    l$marker <- list()
    l$mode <- 'lines'
    # l$line <- list(color = 'rgb(0, 255, 0)')
    #l$line <- list(color = 'rgb(64, 64, 64)') # olive
}else 
if(identical(seriesname, paste0(ESTIMATED_SERIES_NAME, '_', msglobalsf("PU_ASYLUM_SEEKERS")))){
    # l$line <- list(color = 'rgb(128, 128, 128)') # olive
    l$line <- list(color = ' rgb(165,105,189)', width = 1)
    l$marker <- list()
    l$mode <- 'lines'
}else 
if(identical(seriesname, paste0(ESTIMATED_SERIES_NAME, '_', msglobalsf("PU_VENEZUELANS_DISPLACED_ABROAD")))){
    l$line <- list(color = ' rgb(169,50,38)', width = 1)
    l$marker <- list()
    l$mode <- 'lines'
}else 
if(identical(seriesname, paste0(ESTIMATED_SERIES_NAME, '_', msglobalsf("PU_OTHER_PEOPLE_IN_NEED")))){
        l$line <- list(color = ' rgb(169,50,38)', width = 1)
        l$marker <- list()
        l$mode <- 'lines'
}

return(l)

# marker = list(
#   color = 'rgb(17, 157, 255)',
#   size = 20,
#   line = list(
#     color = 'rgb(231, 99, 250)',
#     width = 2
#   )
# ),
#
# my_lines[[i]] <- append(my_lines, l)

# if (i == 1) {
#
#   EmpiricalDataTotalWide <- df
#
# } else {
#
#   EmpiricalDataTotalWide <- EmpiricalDataTotalWide %>% full_join(df, by = "yearref")
#
# }

    # stl <- list()
    # 
    # for (i in 1:length(SeriesNames)) {
    #     
    #     seriesname <- SeriesNames[i]
    #     l <- list()        
    #     if (startsWith(seriesname, 'NSO')){
    #         # triangle square triangle'
    #         l$marker <- list(symbol = "triangle-up", color = 'rgb(0, 255, 255)', size = 10, line = list(color = 'rgb(0, 0, 128)', width = 1))
    #         l$line <- list(color = 'rgb(0, 0, 128)')
    #     }
    #     
    #     stl[[i]] <- l
    # 
    # }
    # 
    # return(stl)
}

# Clear all empirical data and the current LocID
msglobals_clearall <- function() {
    
msglobalsf("LocID", NULL)
msglobalsf("EmpiricalData", NULL)
msglobalsf("EmpiricalDataTotal", NULL)
msglobalsf("EmpiricalDataPopulationUniverse", NULL)
msglobalsf("EmpiricalDataFileName", NULL)
msglobalsf("EmpiricalDataTotalOrigin", NULL)

}

mssource2sourcepu <- function(dfsrc) {

    sourcepu <- dfsrc$source
    
    # # Reduce Length of sources for display
    # for(i in 1:length(df$sourcepu)){
    #     s <- df$sourcepu[i]
    #     if(nchar(s) < 64){
    #         next
    #     }
    #     # s <- s[1:64]
    #     s <- substr(s, 1 ,64)
    #     s <- paste0(s, "{", i, "}", "...")
    #     df$sourcepu[i] <- s
    #     # browser()
    # }
    
    # create sourcepu variable: source + population universe 
    # browser()
    # add PU suffix    
    
    # skip suffix for Rev2020, Rev2023 
    e <- !((sourcepu == msglobalsf("REVISION_NAME_PRV")) | 
           (sourcepu == msglobalsf("REVISION_NAME")) |
           (sourcepu == "UNPD_2024") |
           (sourcepu == "UNPD_2020")
          )
    
    rftpu <- msglobalsf("rftPopulationUniverse")
    sourcepu[e] <- paste0(sourcepu[e], "|", rftpu$Label[match(dfsrc$PopulationUniverse[e], rftpu$ID)])  # add label
    # e <- any(grepl("^UNSDDYB_B09", sourceu, ignore.case = TRUE))

    return(sourcepu)    
    
    # 
    # # Reduce Length of sources for display
    # for(i in 1:length(df$sourcepu)){
    #     s <- df$sourcepu[i]
    #     if(nchar(s) < 64){
    #         next
    #     }
    #     # s <- s[1:64]
    #     s <- substr(s, 1 ,64)
    #     s <- paste0(s, "{", i, "}", "...")
    #     df$sourcepu[i] <- s
    #     # browser()
    # }
    
    
    # # sourceu <- unique(df$source)
    # df$sourcepu <- df$source
    # 
    # # 
    # # # Reduce Length of sources for display
    # # for(i in 1:length(df$sourcepu)){
    # #     s <- df$sourcepu[i]
    # #     if(nchar(s) < 64){
    # #         next
    # #     }
    # #     # s <- s[1:64]
    # #     s <- substr(s, 1 ,64)
    # #     s <- paste0(s, "{", i, "}", "...")
    # #     df$sourcepu[i] <- s
    # #     # browser()
    # # }
    # 
    # 
    # # create sourcepu variable: source + population universe 
    # sourcepuu <- unique(df$sourcepu)
    # for(i in 1:length(sourcepuu)){
    #     e <- df$sourcepu == sourcepuu[i]
    #     pu <- df$PopulationUniverse[e]
    #     if(all(pu == pu[1])){
    #         # if PopulationUniverse is the same, keep sourceup equal to source 
    #     }else{
    #         # src|2
    #         df$sourcepu[e] <- paste0(df$sourcepu[e], "|", df$PopulationUniverse[e])
    #     }
    # }
    # 
    # # unique(df$sourcepu)
        
}




################################################################################
# @@@server
server <- function(input, output, session) {

useShinyjs()

# enable reactive graph
# * While the app is running, press Cmd+F3 (Ctrl+F3 on Windows) to << works
# show the reactlog generated up to that point.
# * After the app has closed, run shiny::reactlogShow() to see
# the log for the complete session.
# reactlog::reactlog_enable()
    
# showNotification("Server function is called")
# Sys.sleep(3)
    
# observe({
#         # Re-execute this reactive expression after 1000 millisecon <- 
# 
#     click("nextButton")
#     invalidateLater(3000, session)
#     
#         
#         # Do something each time this is invalidated.
#         # The isolate() makes this observer _not_ get invalidated and re-executed
#         # when input$n changes.
#     
#     print(paste("invalidateLater"))
#     
# #         print(paste("The value of input$n is", isolate(input$n)))
# })


# observeEvent(input$totalslideranimation, {
# 
#     # print("function called: observeEvent(input$totallideranimation)")
# 
#     # browser()
# 
#     if (input$CountrySelection == ""){
#         return(NULL)
#     }
# 
#     # current country index
#     idx = which(msglobalsf("COUNTRIES")$LongName == input$CountrySelection)
# 
#     # # Error in if: argument is of length zero
#     # # if no data
#     # if (idx < length(msglobalsf("COUNTRIES")$LongName)){
#     #     idx <- idx + 1
#     # } else {
#     #     idx <- 1
#     # }
# 
#     if(input$totalslideranimation != idx){
#         # I can also set the label and select items
#         updateSelectInput(session, "CountrySelection",
#                           # label = paste("Select input label", length(x)),
#                           # choices = x,
#                           # selected = tail(x, 1)
#                           selected = msglobalsf("COUNTRIES")$LongName[[input$totalslideranimation]])
#     }
# 
# })
    
        
observeEvent(input$nextButton, {
    
print("function called: observeEvent(input$nextButton)")
    
if (input$CountrySelection == ""){
    return(NULL)    
}
    
# current country index
idx = which(msglobalsf("COUNTRIES")$LongName == input$CountrySelection)

# Error in if: argument is of length zero
# if no data
if (idx < length(msglobalsf("COUNTRIES")$LongName)){
    idx <- idx + 1
} else {
    idx <- 1
}

# I can also set the label and select items
updateSelectInput(session, "CountrySelection",
                # label = paste("Select input label", length(x)),
                # choices = x,
                # selected = tail(x, 1)
                selected = msglobalsf("COUNTRIES")$LongName[[idx]])
#  output$textOutput <- renderText({ "xxxxxxx" })
})

observeEvent(input$prevButton, {
    
print("function called: observeEvent(input$prevButton)")

# current country index
idx = which(msglobalsf("COUNTRIES")$LongName == input$CountrySelection)

if (idx > 1){
idx <- idx - 1
} else {
idx <- length(msglobalsf("COUNTRIES")$LongName)
}

# Can also set the label and select items
updateSelectInput(session, "CountrySelection",
                # label = paste("Select input label", length(x)),
                # choices = x,
                # selected = tail(x, 1)
                selected = msglobalsf("COUNTRIES")$LongName[[idx]])


#  output$textOutput <- renderText({ "xxxxxxx" })

})
  
# @@@testtotalButton
observeEvent(input$testTotalButton, {
    
    # input$CountrySelectionOrigin
    
    # browser()

    # autoInvalidate()
    
    # updateSelectInput(session, "CountrySelection",
    #                           # label = paste("Select input label", length(x)),
    #                           # choices = x,
    #                           # selected = tail(x, 1)
    #                           selected = "Italy")  #msglobalsf("COUNTRIES")$LongName[[1]]

    # system("C:/akf/matlab/image/screencapture.cmd")
    
    # library("reticulate")
    # 
    # browser()
    # # p <- msglobalsf("UserData")
    # p <- P__
    # 
    # setwd("c:/temp")
    # # kaleido(p)
    # 
    # export(p, file = 'c:/temp/tempPlot.png')
    
    
    #     if (input$CountrySelection == ""){
    #         return(NULL)
    #     }
    
    
    # observe({
    #     click("testTotalButton")
    #     # invalidateLater(3000)
    # })

    
    if (input$CountrySelection == ""){
        return(NULL)
    }
    
    LongNames <- msglobalsf("COUNTRIES")$LongName
    
    i <- which(LongNames == input$CountrySelection)
    # showNotification(input$CountrySelection)
    i <- i + 1
    
    # i <- 1
    updateSelectInput(session, "CountrySelection",
                      # label = paste("Select input label", length(x)),
                      # choices = x,
                      # selected = tail(x, 1)
                      selected = msglobalsf("COUNTRIES")$LongName[[i]])
    
    
    # for(i in 1:3){
    #     
    #     # observe(
    #     
    #     updateSelectInput(session, "CountrySelection",
    #                               # label = paste("Select input label", length(x)),
    #                               # choices = x,
    #                               # selected = tail(x, 1)
    #                               selected = msglobalsf("COUNTRIES")$LongName[[i]])
    #     # )
    #     
    #     #autoInvalidate()
    #     
    #     Sys.sleep(5)
    # 
    # }

})


# @@@testAgeButton
observeEvent(input$testAgeButton, {
    
    # browser()
    
    if (input$CountrySelection == ""){
        return(NULL)
    }
    
    LongNames <- msglobalsf("COUNTRIES")$LongName
    
    idx <- which(LongNames == input$CountrySelection)

    # browser()
    
    # loadEmpiricalData("migrantstock_shiny", IncludeRevisions = TRUE)
    
    # all empirical and revision data 
    df <- msglobalsf("EmpiricalData")
    LocID <- msglobalsf("LocID")
    ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, ".csv")
    write.csv(df, ofname, row.names=FALSE)
    print(msglobalsf("TEMPORARY_FOLDER")) # C:\Users\Kirill\AppData\Local\Temp\\migrantstock\
    
    # output$textOutput <- renderText(ofname)
    # 
    # # save EmpiricalDataTotal
    # df <- msglobalsf("EmpiricalDataTotal")
    # LocID <- msglobalsf("LocID")
    # ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_total.csv")
    # write.csv(df, ofname, row.names=FALSE)
    # output$textOutput <- renderText(ofname)
    # 
    # # Save Rev 2023
    # dfrev <- df[df$source == msglobalsf("ESTIMATED_SERIES_NAME"),]
    # ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_", msglobalsf("ESTIMATED_SERIES_NAME"), "_final.csv")
    # write.csv(dfrev, ofname, row.names=FALSE)
    # 
    # SeriesNames <- unique(df$source)
    # # SeriesNames <- paste(SeriesNames, sep = "<br>")
    # # str1 <- paste("You have selected", input$var)
    # # str2 <- paste("You have chosen a range that goes from",
    # #               input$range[1], "to", input$range[2])
    # # HTML(paste(str1, str2, sep = '<br/>'))
    # 
    # ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_series.txt")
    # #sink(ofname)
    # write.csv(SeriesNames, ofname, row.names=FALSE)
    # #print(SeriesNames)
    # 
    # dforg <- msglobalsf("EmpiricalDataTotalOrigin")
    # LocIDorg <- msglobalsf("LocIDorg")
    # ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_", LocIDorg, "_origin.csv")
    # write.csv(dforg, ofname, row.names=FALSE)
    # 
    # # sourceu <- unique(df$source);
    # 
    # # output$textOutput <- renderText({ HTML(paste(SeriesNames, collapse  = '<br>')) })
    # # output$textOutput <- renderText({ "xxxxxxx" })
    # 
    # ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_.RData")
    # save(df, dforg, dfrev, SeriesNames, file = ofname)
    
    # 
    # 
    # 
    # 
    # # showNotification(input$CountrySelection)
    # i <- i + 1
    # 
    # # i <- 1
    # updateSelectInput(session, "CountrySelection",
    #                   # label = paste("Select input label", length(x)),
    #                   # choices = x,
    #                   # selected = tail(x, 1)
    #                   selected = msglobalsf("COUNTRIES")$LongName[[i]])
    # 
    
    # for(i in 1:3){
    #     
    #     # observe(
    #     
    #     updateSelectInput(session, "CountrySelection",
    #                               # label = paste("Select input label", length(x)),
    #                               # choices = x,
    #                               # selected = tail(x, 1)
    #                               selected = msglobalsf("COUNTRIES")$LongName[[i]])
    #     # )
    #     
    #     #autoInvalidate()
    #     
    #     Sys.sleep(5)
    # 
    # }
    
})

output$plotage <- renderPlotly({
    
    # browser()

    # Using this reactive expression makes renderPlotly() function to be dependent on input$totalseriesfilter and 
    # input$CountrySelection.  As a result, the plot will be updated if any of the controls will change.
    ds <- empiricaltotaldataset()
    
    # req() for “required.”
    # When the input to req() is not true, it sends a special signal to tell Shiny
    # that the reactive does not have all the inputs that it requires, so it should be
    # “paused.”
    
    # Having input$CountrySelection in the function body makes 
    # it is reactive for any changes in the list of countries 
    # Requesting req(input$CountrySelection) to be initialized we avoid running this function with uninitialized controls
    req(input$AgeGroupSelection)
    
    if (input$AgeGroupSelection == ""){
        p <- plot_ly()
        p <- layout(p, title = 'No data found')
        return(p)
    }

    age <- 0
    agelength <- 5
    
    idx <- str_locate(input$AgeGroupSelection, "-")
    if(!(is.na(idx[1]) | is.na(idx[2]) | (idx[2]!=idx[1]))){
        # 0-4 format
        a1 <- substr(input$AgeGroupSelection, 1, idx[1]-1)
        a1 <- as.numeric(a1)
        a2 <- substr(input$AgeGroupSelection, idx[1]+1, str_length(input$AgeGroupSelection))
        a2 <- as.numeric(a2)
        age <- a1
        agelength <- a2-a1+1
    }else{
        # 100+
        idx <- str_locate(input$AgeGroupSelection, "[+]")
        a1 <- substr(input$AgeGroupSelection, 1, idx[1]-1)
        age <- as.numeric(a1)
        agelength <- 0
    }
    
    print(input$AgeGroupSelection)
    
    # file:///C:/akf/MUS/MS/scripts/migrantstock/output/msplot_age/112/index.htm
    # load("C:/akf/gd/MSGlance/scripts/data/112_EmpiricalData.ms.RData")
    # df <- df[df$age == 5 & df$agelength == 5 & df$sex == 0, ]
    # unique(df$source)
    # 
    # browser() !!!!!
    
    # all empirical and revision data 
    
    df <- loadEmpiricalData("migrantstock_shiny", IncludeRevisions = TRUE)
    # df <- msglobalsf("EmpiricalData")
    df <- df[df$LocIDorg == 0, ]
    df <- df[df$sex == 0, ]
    df <- df %>% filter(PopulationUniverse %in% c(4,3,2,6,7,9,10,11))  # Foreigners, Foreign-born, Refugees, Asylum seekers, Venezuelans displaced abroad, INTERNATIONAL MIGRANTS	9, Other people in need of international protection
    df <- df[!(df$source %in% c("UNPD_2024")), ] # no data on age in UNPD 2024
    df <- df[df$age == age & df$agelength == agelength, ]
    
    LocID <- msglobalsf("LocID")
    
    if(is.null(LocID) | is.null(df) | (nrow(df) == 0)){
        p <- plot_ly()
        p <- layout(p, title = 'No data found')
        return(p)
    }
    
    # the same code as for total, need a function #!!!fixme
    
    df$sourcepu <- mssource2sourcepu(df)

    # produce attributes for the series plotted 
    series <- list()
    SeriesNames <- unique(df$sourcepu)
    
    # stl <- msplotlystyle(SeriesNames) # !!!!!
    
    for (i in 1:length(SeriesNames)) {
        
        # newname <- paste("v", i, sep="", collapse=NULL)
        
        seriesname <- SeriesNames[[i]]
        
        # dfi <- df %>% dplyr::filter(source == seriesname)
        dfi <- df %>% dplyr::filter(sourcepu == seriesname)
        
        # sourcepu
        
        # single PopulationUniverse per series
        l <- list(x = dfi$yearref, y = dfi$value, name = seriesname, mode = 'lines+markers') # 
        
        l1 <- msplotlystyle(seriesname) # get styles for plotly
        l$marker  <- l1$marker
        l$line    <- l1$line
        l$mode    <- l1$mode
        
        series[[length(series)+1]] <- l
        # series[[i]] <- l
    }
    
    
    # p <- plot_ly(EmpiricalDataTotal, x = ~yearref, y = ~value, split = ~source)
    
    # https://stackoverflow.com/questions/38828875/add-multiple-lines-to-a-plot-ly-graph-with-add-trace
    # evaluate = TRUE
    
    p <- plot_ly()
    # p <- plot_ly( height = 600)
    
    # p
    for(i in 1:length(series)){
        
        seriesi <- series[[i]]
        print(seriesi$name)
        
        if(seriesi$name == "UNHCR|2"){
            seriesi$name = seriesi$name
        }
        # print(line)
        
        p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']], mode = seriesi[['mode']], name = seriesi[['name']], line = seriesi[['line']], marker = seriesi[['marker']]) # marker=list(color=line[['color']]),
        # p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']])
        
        # p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']], mode = seriesi[['mode']], name = seriesi[['name']], line = seriesi[['line']])
        
    }
    
    # https://plotly.com/r/figure-labels/
    
    niso3locid <- msglobalsf("COUNTRIESALL")
    niso3locid <- niso3locid[niso3locid$LongName == input$CountrySelection,]
    niso3locid <- paste0(niso3locid$LongName, "_", niso3locid$LocID, "_", niso3locid$ISO3)
    stitle     <- paste0("Age group = ", ifelse(agelength == 0,  paste0(age, "+"), paste0(age, "-", age + agelength - 1)), ", ", niso3locid)

    # browser()
    p <- layout(p, title = stitle, xaxis = list(title = 'Year'), yaxis = list(title = 'Number of migrants')) # , plot_bgcolor = "#e5ecf6"
    
    # export(p, file = "c:/temp/plot.png") 
    
}) # end of renderPlotly()


# plot comparing empirical and estimated age distributions
output$plotageempirical <- renderPlotly({
    
    # make this function  reactive to changes in the country selection 
    tmp <- input$CountrySelection
    
    # browser()
    
    #!!!fixme function select age compositions 
    
    # data with age structures
    df <- loadEmpiricalData("migrantstock_shiny", IncludeRevisions = TRUE)
    # df <- msglobalsf("EmpiricalData")
    df <- df[df$LocIDorg == 0, ]
    df <- df[df$sex == 0, ]
    df <- df[!(df$age == 0 & df$agelength == 0), ]
    df <- df %>% filter(PopulationUniverse %in% c(4,3,2,6,7,9,10,11))  #fixme need func for filtering  Foreigners, Foreign-born, Refugees, Asylum seekers, Venezuelans displaced abroad, INTERNATIONAL MIGRANTS	9, Other people in need of international protection
    
    source_to_compare <- "UNPD_2020"
    dfcmp <- df[df$source == source_to_compare, ]
    # dfcmp <- df[df$source == "UNPD_2020", ]
    
    # delete sources
    df <- df[!(df$source %in% c("UNPD_2020", "UNPD_2024", "WorldBank")), ]

    vcat <- msglobalsf("EMPIRICAL_DATA_FIELDS_PK")
    vcat <- setdiff(vcat, list("age", "agelength"))
    dfiter <- df[,vcat]
    dfiter <- distinct(dfiter)
    dfiter <- dfiter %>% arrange(yearref, sex, source)

    dfn <- nrow(dfiter)
    
    if(dfn == 0){
        p <- plot_ly()
        p <- layout(p, title = 'No data found')
        return(p)
    }

    # browser()

    LocID <- msglobalsf("LocID")
    
    if(is.null(LocID)){
        p <- plot_ly()
        p <- layout(p, title = 'No data found')
        return(p)
    }
    
    # if(!(LocID == 208)){
    #     p <- plot_ly()
    #     p <- layout(p, title = '208 only')
    #     return(p)
    # }
    
    # df$sourcepu <- mssource2sourcepu(df)
    
    agestruc  <-  unlist(strsplit(input$AgeEmpirical, split = "|", fixed = TRUE))
    
    # browser()
    
    rftpu <- msglobalsf("rftPopulationUniverse")
    
    # get filter from the input interface control
    flt <- NULL
    flt$source <- trimws(agestruc[1])
    flt$yearref <- trimws(agestruc[2])                  # !!! might not work need fixme two digital 
    flt$PopulationUniverse <- trimws(agestruc[3])
    flt$PopulationUniverse <- rftpu$ID[rftpu$Label == flt$PopulationUniverse]
    
    # flt$yearref <- as.numeric(flt$yearref)
    flt$PopulationUniverse <- as.numeric(flt$PopulationUniverse)
    
    # e <- df$LocID == flt$LocID & df$sex == flt$sex & df$yearref == flt$yearref & df$PopulationUniverse == flt$PopulationUniverse
    # e <- df$sex == 0 & signif(df$yearref, digits = 2) == flt$yearref & df$PopulationUniverse == flt$PopulationUniverse & df$source == flt$source
    # Or we use flt$yearref as character 

    # browser()
    
    # select current age structure
    df$yearref2 <- sprintf("%.2f", df$yearref)
    e <- df$sex == 0 & df$yearref2 == flt$yearref & df$PopulationUniverse == flt$PopulationUniverse & df$source == flt$source
    dfi <- df[e,]
    
    if(nrow(dfi) == 0){
        p <- plot_ly()
        p <- layout(p, title = 'No data found')
        return(p)
    }
    
    # get regular age structure 
    dfir <- ageregular(dfi)
    dfir <- dfir$dfreg
    dfir$yearref <- dfi$yearref[1]
    dfi <- dfir
    
    # Distribute by single age groups so the plot will resemble a step plot 
    # Keep open age group
    
    ageopen <- NULL
    if(tail(dfi$agelength,1) == 0){
        ageopen <- tail(dfi$age, 1)
        valopen <- tail(dfi$value, 1)
    }
    
    # get single age groups
    dfi <- agesingle(dfi)

    if(!is.null(ageopen)){
        e <- dfi$age >= ageopen
        dfiopen <- dfi[1, ]
        dfiopen$age <- ageopen
        dfiopen$agelength <- 0
        dfiopen$value <- valopen
        dfi <- rbind(dfi[!e,], dfiopen)
    }
    
    # # aggregate open age group        
    # e <- dfcmpy$age >= ageopen
    # dfcmpyopen <- dfcmpy[1, ]
    # dfcmpyopen$age <- ageopen
    # dfcmpyopen$agelength <- 0
    # # dfcmpyopen$value <- sum(dfcmpy$value[e])
    # dfcmpyopen$value <- valopen
    # dfcmpy <- rbind(dfcmpy[!e,], dfcmpyopen)
    # 

    # produce attributes for the series plotted 
    series <- list()
    seriesname <- input$AgeEmpirical
    l <- list(x = dfi$age, y = dfi$value, name = seriesname, mode = 'lines+markers') #
    
    # l1 <- msplotlystyle(seriesname) # get styles for plotly
    # l$marker  <- l1$marker
    # l$line    <- l1$line
    # l$mode    <- l1$mode
    
    series[[length(series)+1]] <- l

    SeriesNames <- seriesname
    
    # browser()

    dfcmp <- dfcmp %>% filter(dfcmp$sex == 0 & dfcmp$LocIDorg == 0 & (!(dfcmp$age == 0 & dfcmp$agelength  == 0)))
    agegroups <- unique(dfcmp$age)      # msglobalsf("AGE_PUBLISHED")  # c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75)
    vali <- rep(NaN, length(agegroups))            
    if(dfi$yearref[1] >= dfcmp$yearref[1] & dfi$yearref[1] <= tail(dfcmp$yearref,1)){
        # if inside the region covered by the estimates, do interpolation of the estimates to the year with empirical data 
        # Interpolate age groups
        for(i in seq_along(agegroups)){
            # browser()
            dfcmpa <- dfcmp %>% filter(dfcmp$age == agegroups[i])
            dfcmpay <- msestdatarangex(dfcmpa, dfi$yearref[1])
            vali[i] <- dfcmpay$value
            # save.image(file = "c:/temp/orkspace.RData")
        }
        
    }
    
    if(!any(is.nan(vali))){
        
        # we got interpolated values
        
        # get data set for a single age structure 
        dfcmpy <- dfcmp %>% filter(dfcmp$sex == 0 & dfcmp$LocIDorg == 0 & (!(dfcmp$age == 0 & dfcmp$agelength  == 0)) & dfcmp$yearref == dfcmp$yearref[1])    
        if(nrow(dfcmpy) == length(vali)){
            # Ok
        }else{
            stop("nrow(dfcmpy) == length(vali)")
        }
        dfcmpy$yearref <- dfi$yearref[1]
        dfcmpy$value <- vali
        
        # browser()
        # Distribute by single age to show as a step plot 
        # Keep open age group the same 
        ageopen <- tail(dfcmpy$age, 1)
        valopen <- tail(dfcmpy$value, 1)
        dfcmpy <- agesingle(dfcmpy)
        
        # aggregate open age group        
        e <- dfcmpy$age >= ageopen
        dfcmpyopen <- dfcmpy[1, ]
        dfcmpyopen$age <- ageopen
        dfcmpyopen$agelength <- 0
        # dfcmpyopen$value <- sum(dfcmpy$value[e])
        dfcmpyopen$value <- valopen
        dfcmpy <- rbind(dfcmpy[!e,], dfcmpyopen) # add open age group 

        # series for plotly()        
        seriesname <- paste0(source_to_compare, " [year=", sprintf("%.2f", dfi$yearref[1]), "] ")
        
        # seriesname <- "cmp"
        l <- list(x = dfcmpy$age, y = dfcmpy$value, name = seriesname, mode = 'lines+markers')
        series[[length(series)+1]] <- l
    }
    

    # # source to compare
    # if(LocID == 208){
    #     browser()
    # }
    
    p <- plot_ly()
    # p <- plot_ly( height = 600)
    
    # p
    for(i in 1:length(series)){
        seriesi <- series[[i]]
        # print(seriesi$name)
        p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']], mode = seriesi[['mode']], name = seriesi[['name']], line = seriesi[['line']], marker = seriesi[['marker']]) # marker=list(color=line[['color']]),
    }
    
    # https://plotly.com/r/figure-labels/
    
    niso3locid <- msglobalsf("COUNTRIESALL")
    niso3locid <- niso3locid[niso3locid$LongName == input$CountrySelection,]
    niso3locid <- paste0(niso3locid$LongName, "_", niso3locid$LocID, "_", niso3locid$ISO3)
    #stitle     <- paste("Total, ", niso3locid, sep="", collapse=NULL)  # need some space for the plot controls
    # stitle     <- paste0("Age structure, ", sprintf("%.2f", dfi$yearref[1]), ", ", dfi$PopulationUniverse[1], ", ", dfi$source[1], ", ", niso3locid)
    stitle     <- paste0("Age structures", ", ", niso3locid)
    
    # browser()
    p <- layout(p, title = stitle, xaxis = list(title = 'Age'), yaxis = list(title = 'Number of migrants'), showlegend=T) # , plot_bgcolor = "#e5ecf6"
    # p <- p %>% layout(showlegend=T) # displays legend for single series / trace 
    # export(p, file = "c:/temp/plot.png") 
    
})


# observeEvent(input$totalseriesfilter, {
# print("function called: observeEvent(input$totalseriesfilter)")
# 
# if (input$CountrySelection == ""){
#     return(NULL)    
# }
# 
# # current country index
# idx = which(msglobalsf("COUNTRIES")$LongName == input$CountrySelection)
# 
# # I can also set the label and select items
# updateSelectInput(session, "CountrySelection",
#                   # label = paste("Select input label", length(x)),
#                   # choices = x,
#                   # selected = tail(x, 1)
#                   selected = "")
# 
# # I can also set the label and select items
# updateSelectInput(session, "CountrySelection",
#                   # label = paste("Select input label", length(x)),
#                   # choices = x,
#                   # selected = tail(x, 1)
#                   selected = msglobalsf("COUNTRIES")$LongName[[idx]])
# #  output$textOutput <- renderText({ "xxxxxxx" })
#     
# })


# reactive expression depending on the current country selection (via input$CountrySelection) and the current filter (via input$totalseriesfilter)
# it runs each time user changes any of these controls 
empiricaltotaldataset <- reactive({
    
    if(session$clientData$url_hostname == "127.0.0.1"){
        showNotification("CountrySelection")
    }

    # Sys.sleep(1)
    # browser()
    
    if (input$CountrySelection == ""){
        msglobals_clearall()
        return(NULL)
    }
    
    # msg <- paste0("input$CountrySelection = ", input$CountrySelection); print(msg)
    
    # current user-specific countries
    COUNTRIES <- msglobalsf("COUNTRIES")
    
    # get current LocID
    df <- COUNTRIES %>% dplyr::filter(LongName == input$CountrySelection) %>% select(LocID)
    
    # print("My current countries:")          #fixme!!! need all 235 countries to run
    # print(msglobalsf("COUNTRIES"))
    # print("My selected dataset: ")
    # print(df)
    
    if(nrow(df) > 0){
        # update global LocID in msglobalsf()
        msglobalsf("LocID", df$LocID)
    } else {
        # no LocID found
        msglobals_clearall()
        return(NULL)
    }
    
    # index
    idx <- which(COUNTRIES$LocID == df$LocID)
    
    # browser()
    
    # if(input$totallideranimation != idx){
    #     #updateSliderInput(session, "totallideranimation", value = idx)
    #     updateSliderInput(session, "totallideranimation", value = idx, min = 1, max = length(Countries$LongName), step = 1)
    # }
    
    
    # observe({
    #     v <- input$CountrySelection
    #     Countries <- msglobalsf("COUNTRIES")
    #     idx <- which(Countries$LongName == v)
    #     
    #     # Control the value, min, max, and step.
    #     # Step size is 2 when input value is even; 1 when value is odd.
    #     
    #     updateSliderInput(session, "totallideranimation", value = idx, min = 1, max = length(Countries$LongName), step = 1)
    #     
    #     # updateSliderInput(session, "totallideranimation", value = 1,
    #     #                   min = 1, max = 5, step = 1)
    # })
    
    
    # # do nothing if not in the list
    # if(!any(LocID == LocIDs)){
    #   showNotification("Not emplemented: Empty plot")
    #   # empty plot
    #   output$plottotal <-  renderPlotly({
    #     plotly_empty() %>%
    #     config(staticPlot = TRUE)
    #   })
    #   return(NULL)
    # }
    
    # if(identical(input$CountrySelection, "Sweden")){
    #
    #   # LocID <<- 752
    #
    # }else if((identical(input$CountrySelection, "Denmark"))){
    #
    #   LocID <<- 208
    #
    # }else{
    #
    #   output$plottotal <- renderPlotly({
    #
    #     showNotification("Empty plot")
    #
    #     p <- plot_ly()
    #
    #   })
    #
    #   return(NULL)
    #
    # }
    
    # update emprical data for the plot
    # showNotification("Loading empirical data ... ")
    # Sys.sleep(3)
    # return(NULL)
    
    # browser() #!!!1
    
    # load data 
    print("Loading emprical data ... (input$CountrySelection)")
    
    df <- loadEmpiricalData("migrantstock_shiny", IncludeRevisions = TRUE)
    
    EmpiricalDataTotal <- msglobalsf("EmpiricalDataTotal")
#    write.csv(EmpiricalDataTotal, file="C:/temp/r/EmpiricalDataTotal.csv", quote=FALSE, row.names = FALSE)
    
    flt <- input$totalseriesfilter
    if(flt == "rev"){
        e <- EmpiricalDataTotal$source == msglobalsf("REVISION_NAME_PRV") | EmpiricalDataTotal$source == msglobalsf("REVISION_NAME")
        EmpiricalDataTotal <- EmpiricalDataTotal[e,]
        msglobalsf("EmpiricalDataTotal", EmpiricalDataTotal)
    }else
    if(flt == "unhcr" | flt == "unhcrexcl"){
        #!!! not implemented
        #df <- df %>% filter(!grepl("^unhcr", df$source, ignore.case = TRUE))

        # e <- grepl("^unhcr", EmpiricalDataTotal$source, ignore.case = TRUE)

        # Alternatively we can use factors or functions PU_TOTAL <- function() {return(0)}
        # neither looks good
        e <- is.element(EmpiricalDataTotal$PopulationUniverse, c(msglobalsf("PU_REFUGEES"), msglobalsf("PU_ASYLUM_SEEKERS"), msglobalsf("PU_VENEZUELANS_DISPLACED_ABROAD")))

        if(flt == "unhcrexcl"){e <- !e}

        EmpiricalDataTotal <- EmpiricalDataTotal[e,]
        if(nrow(EmpiricalDataTotal) == 0){
            EmpiricalDataTotal <- NULL
            
            msglobalsf("EmpiricalData", NULL)
            msglobalsf("EmpiricalDataTotal", NULL)
            msglobalsf("EmpiricalDataPopulationUniverse", NULL)
            msglobalsf("EmpiricalDataFileName", NULL)
            msglobalsf("EmpiricalDataTotalOrigin", NULL)
        }
        msglobalsf("EmpiricalDataTotal", EmpiricalDataTotal)
        # "Census_1990"
        # "Census_2020"
        # "Rev2020"
        # "Rev2023"
        # "Rev2023_2"
        # "Rev2023_3"
        # "Rev2023_6"
        # "Rev2023_9"
        # "Rev2023_9P"
        # "UNHCR"
        # "UNSDDYB_B09_CJC"
        # "UNSDDYB_B59_CFC"
        # "UNSDDYB_B59_CJC"

    } # end of filter 

    # update list of origins 
    #browser()
    UserData <- msglobalsf("UserData")
    LocID <- msglobalsf("LocID")
    e <- UserData$Countries$LocID == LocID
    CountriesOrg <- msglobalsf("COUNTRIESALL")
    
    # CountriesOrg <- CountriesOrg %>% filter(LocID %in% UserData$Countries$LocIDorg[e])
    # updateSelectInput(session, "CountrySelectionOrigin", choices = CountriesOrg$LongName, selected = CountriesOrg$LongName[1]) #@@@CountriesOrg
    # msglobalsf("LocIDorg", CountriesOrg$LocID[1])
    
    Ctr  <-  UserData$Countries[e, ]
    Ctr$LocID <- Ctr$LocIDorg
    Ctr$LocIDorg <- NULL
    Ctr <- Ctr %>% left_join(CountriesOrg)
    Ctr <- Ctr$LongName
    # Ctr <- c("Norway", "Denmark")
    Ctr <- factor(Ctr)
    updateSelectInput(session, "CountrySelectionOrigin", choices = Ctr, selected = Ctr[1]) #@@@CountriesOrg

    # if(LocID != 208){
    #     return(NULL)
    # }
    
    #!!!fixme
    # browser()
    
    df <- loadEmpiricalData("migrantstock_shiny", IncludeRevisions = TRUE)
    # df <- msglobalsf("EmpiricalData")
    df <- df[df$LocIDorg == 0, ]
    df <- df[df$sex == 0, ]
    df <- df[!(df$age == 0 & df$agelength == 0), ]
    df <- df %>% filter(PopulationUniverse %in% c(4,3,2,6,7,9,10,11))  # Foreigners, Foreign-born, Refugees, Asylum seekers, Venezuelans displaced abroad, INTERNATIONAL MIGRANTS	9, Other people in need of international protection
    df <- df[!(df$source %in% c("UNPD_2020", "UNPD_2024", "WorldBank")), ]
    
    vcat <- msglobalsf("EMPIRICAL_DATA_FIELDS_PK")
    vcat <- setdiff(vcat, list("age", "agelength"))
    dfiter <- df[,vcat]
    dfiter <- distinct(dfiter)
    dfiter <- dfiter %>% arrange(yearref, sex, source)
    
    dfn <- nrow(dfiter)
    agestruc <- list()
    agestruc[1] <- "No age compositions found"
    if(dfn >= 1){
        for(i in 1:dfn){
            # ifelse(i == 1, 1, length(agestruc)+1
            # if(i == 1) {idx <- 1}
            # agestruc[length(agestruc)+1] <- paste0(dfiter$yearref[i], "|", dfiter$PopulationUniverse[i], "|", dfiter$source[i])
            rftpu <- msglobalsf("rftPopulationUniverse")
            agestruc[i] <- paste0(dfiter$source[i], " | ", sprintf("%.2f", dfiter$yearref[i]), " | ", rftpu$Label[dfiter$PopulationUniverse[i] == rftpu$ID]) 
        }
    }
    
    updateSelectInput(session, "AgeEmpirical", choices = agestruc, selected = agestruc[1])
    
})

#@@@CountrySelection
#observeEvent(input$CountrySelection, {
#     
#     print("function called: observeEvent(input$CountrySelection")
#     # browser()
#     # showNotification("CountrySelection")
#     # Sys.sleep(1)
#     
#     if (input$CountrySelection == ""){
#         return(NULL) # we don't really return anything from observer
# #         # no country
# #         browser()
# #         if(nrow(msglobalsf("COUNTRIES")) >= 1){
# #             updateSelectInput(session, "CountrySelection",
# #                               # label = paste("Select input label", length(x)),
# #                               # choices = x,
# #                               # selected = tail(x, 1)
# #                               selected = msglobalsf("COUNTRIES")$LongName[[1]])
# #         }else{
# #             return(NULL)    
# #         }
#     }
#     
#     msg <- paste0("input$CountrySelection = ", input$CountrySelection); print(msg)
# 
#     # current user-specific countries
#     COUNTRIES <- msglobalsf("COUNTRIES")
#     
#     # get current LocID
#     df <- COUNTRIES %>% dplyr::filter(LongName == input$CountrySelection) %>% select(LocID)
#     
#     # print("My current countries:")          #fixme!!! need all 235 countries to run
#     # print(msglobalsf("COUNTRIES"))
#     # print("My selected dataset: ")
#     # print(df)
# 
#     if(nrow(df) > 0){
#         # update global LocID in msglobalsf()
#         msglobalsf("LocID", df$LocID)
#     } else {
#         # no LocID found
#         msglobals_clearall()
#         # # empty plot
#         # renderPlotly({
#         #   plotly_empty() %>%
#         #     config(staticPlot = TRUE)
#         # })
#         return(NULL)
#     }
# 
#     # index
#     idx <- which(COUNTRIES$LocID == df$LocID)
# 
#     # browser()
#     
#     if(input$totallideranimation != idx){
#         #updateSliderInput(session, "totallideranimation", value = idx)
#         updateSliderInput(session, "totallideranimation", value = idx, min = 1, max = length(Countries$LongName), step = 1)
#     }
#     
#     
#     # observe({
#     #     v <- input$CountrySelection
#     #     Countries <- msglobalsf("COUNTRIES")
#     #     idx <- which(Countries$LongName == v)
#     #     
#     #     # Control the value, min, max, and step.
#     #     # Step size is 2 when input value is even; 1 when value is odd.
#     #     
#     #     updateSliderInput(session, "totallideranimation", value = idx, min = 1, max = length(Countries$LongName), step = 1)
#     #     
#     #     # updateSliderInput(session, "totallideranimation", value = 1,
#     #     #                   min = 1, max = 5, step = 1)
#     # })
# 
#     
#   # # do nothing if not in the list
#   # if(!any(LocID == LocIDs)){
#   #   showNotification("Not emplemented: Empty plot")
#   #   # empty plot
#   #   output$plottotal <-  renderPlotly({
#   #     plotly_empty() %>%
#   #     config(staticPlot = TRUE)
#   #   })
#   #   return(NULL)
#   # }
# 
#   # if(identical(input$CountrySelection, "Sweden")){
#   #
#   #   # LocID <<- 752
#   #
#   # }else if((identical(input$CountrySelection, "Denmark"))){
#   #
#   #   LocID <<- 208
#   #
#   # }else{
#   #
#   #   output$plottotal <- renderPlotly({
#   #
#   #     showNotification("Empty plot")
#   #
#   #     p <- plot_ly()
#   #
#   #   })
#   #
#   #   return(NULL)
#   #
#   # }
# 
#   # update emprical data for the plot
#   # showNotification("Loading empirical data ... ")
#   # Sys.sleep(3)
#   # return(NULL)
# 
#     # browser()
#     
#     # load data 
#     print("Loading emprical data ... (input$CountrySelection)")
#     loadEmpiricalData("migrantstock_shiny", IncludeRevisions = TRUE)
#     EmpiricalDataTotal <- msglobalsf("EmpiricalDataTotal")
#     
#     # browser()
#     # flt <- input$totalseriesfilter
#     # if(flt == "rev"){
#     #     e <- EmpiricalDataTotal$source == "Rev2020" | EmpiricalDataTotal$source == msglobalsf("REVISION_NAME")
#     #     EmpiricalDataTotal <- EmpiricalDataTotal[e,]
#     # }else
#     # if(flt == "unhcr" | flt == "unhcrexcl"){
#     #     #!!! not implemented         
#     #     #df <- df %>% filter(!grepl("^unhcr", df$source, ignore.case = TRUE))
#     #     
#     #     # e <- grepl("^unhcr", EmpiricalDataTotal$source, ignore.case = TRUE)
#     #     
#     #     # Alternatively we can use factors or functions PU_TOTAL <- function() {return(0)}
#     #     # neither looks good
#     #     e <- is.element(EmpiricalDataTotal$PopulationUniverse, c(msglobalsf("PU_REFUGEES"), msglobalsf("PU_ASYLUM_SEEKERS"), msglobalsf("PU_VENEZUELANS_DISPLACED_ABROAD")))
#     #     
#     #     if(flt == "unhcrexcl"){e <- !e}
#     #     
#     #     EmpiricalDataTotal <- EmpiricalDataTotal[e,]
#     #     # "Census_1990"
#     #     # "Census_2020"
#     #     # "Rev2020"
#     #     # "Rev2023"
#     #     # "Rev2023_2"
#     #     # "Rev2023_3"
#     #     # "Rev2023_6"
#     #     # "Rev2023_9"
#     #     # "Rev2023_9P"
#     #     # "UNHCR"
#     #     # "UNSDDYB_B09_CJC"
#     #     # "UNSDDYB_B59_CFC"
#     #     # "UNSDDYB_B59_CJC"
#     #     
#     # }
#     
#     # browser()
#     
#     # # no data selected
#     # if(nrow(EmpiricalDataTotal) == 0){
#     #     # msglobals_clearall
#     #     EmpiricalDataTotal <- NULL 
#     # }
# 
#     # if(is.null(EmpiricalDataTotal)){
#     # 
#     #     # showNotification("EmpiricalData are NULL")
#     #     # Sys.sleep(3)
#     #     # output$textOutput <- renderText({"No empirical data found "})
#     #     
#     #     # empty plot
#     #     output$plottotal <- renderPlotly({
#     #       plotly_empty() %>%
#     #         config(staticPlot = TRUE)
#     #     })
#     #     
#     #     return(NULL)
#     # 
#     # }
# 
#   # if(nrow(EmpiricalDataTotal) == 0){
#   # 
#   #   # showNotification("nrow(EmpiricalData) == 0")
#   #   # Sys.sleep(3)
#   # 
#   #   # empty plot
#   #   renderPlotly({
#   #     plotly_empty() %>%
#   #     config(staticPlot = TRUE)
#   #   })
#   # 
#   #   return(NULL)
#   # }
# 
#   
#   # output$textStatusBar <- renderText({""})
#   # 
#   # EMPIRICALDATATOTAL <<- EmpiricalDataTotal
# 
# })  # end of observeEvent(input$CountrySelection


observeEvent(input$CountrySelectionOrigin, {
    
    print("function called: observeEvent(input$CountrySelectionOrigin")
    
    # browser()  # debugger
    
    # # showNotification("CountrySelection")
    # # Sys.sleep(1)
    # 
    # # showNotification(input$CountrySelection)
    # # Sys.sleep(1)
    
    
    if (input$CountrySelection == ""){
        # no country
        return(NULL)
    }
    
    if (input$CountrySelectionOrigin == ""){
        # no country
        return(NULL)
    }

    print("input$CountrySelectionOrigin: ")
    print(input$CountrySelectionOrigin)
    

    # update global LocID
    df <- msglobalsf("COUNTRIESALL")  %>%
        dplyr::filter(LongName == input$CountrySelectionOrigin) %>% select(LocID)

    # print(msglobalsf("COUNTRIES"))
    # print(df)

    if(nrow(df) > 0){

        # setup global
        msglobalsf("LocIDorg", df$LocID)

    } else {
        # no LocID found
        # empty plot
        renderPlotly({
            plotly_empty() %>%
                config(staticPlot = TRUE)
        })
        return(NULL)
    }
    
    # msglobalsf("LocIDorg", 356)  # 356 india
    
    # 
    # # browser()
    # 
    print("Loading emprical data ... (input$CountrySelectionOrigin)")
    loadEmpiricalData("migrantstock_shiny", IncludeRevisions = TRUE)
    EmpiricalDataTotalOrigin <- msglobalsf("EmpiricalDataTotalOrigin")
    
    # browser()
    
    # output$textStatusBar <- renderText({"No empirical data found on country of origin"})

    if(is.null(EmpiricalDataTotalOrigin)){

        # showNotification("EmpiricalData are NULL")
        # Sys.sleep(3)

        output$textStatusBar <- renderText({"No empirical data found on country of origin"})

        # empty plot
        renderPlotly({
            plotly_empty() %>%
                config(staticPlot = TRUE)
        })

        return(NULL)

    }

    if(nrow(EmpiricalDataTotalOrigin) == 0){

        # showNotification("nrow(EmpiricalData) == 0")
        # Sys.sleep(3)

        # empty plot
        renderPlotly({
            plotly_empty() %>%
                config(staticPlot = TRUE)
        })

        return(NULL)
    }

    # # render the plot with valid empirical data
    # output$plotorigin <- renderPlotly({
    # 
    #     series <- list()
    # 
    #     SeriesNames <- unique(EmpiricalDataTotalOrigin$source)
    #     
    #     
    #     for (i in 1:length(SeriesNames)) {
    #         
    #         # newname <- paste("v", i, sep="", collapse=NULL)
    #         
    #         seriesname <- SeriesNames[[i]]
    #         
    #         df <- EmpiricalDataTotalOrigin %>%
    #             dplyr::filter(source == seriesname) %>%
    #             select(yearref, value)
    #         
    #         # l <- list(x = df$yearref, y = df$value, name = SeriesNames[[i]])
    #         
    #         l <- list(x = df$yearref, y = df$value, name = seriesname, mode = 'lines+markers') # 
    #         
    #         l1 <- msplotlystyle(seriesname) # get styles for plotly
    #         l$marker  <- l1$marker
    #         l$line    <- l1$line
    #         l$mode    <- l1$mode
    #         
    #         series[[i]] <- l
    #         
    #     }
    #     
    #     # p <- plot_ly(EmpiricalDataTotal, x = ~yearref, y = ~value, split = ~source)
    # 
    #     # https://stackoverflow.com/questions/38828875/add-multiple-lines-to-a-plot-ly-graph-with-add-trace
    #     # evaluate = TRUE
    # 
    #     p <- plot_ly()
    #     # p
    #     for(seriesi in series) {
    # 
    #         # print(line)
    # 
    #         p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']], mode = seriesi[['mode']], name = seriesi[['name']], line = seriesi[['line']], marker = seriesi[['marker']]) # marker=list(color=line[['color']]),
    # 
    #         # p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']], mode = seriesi[['mode']], name = seriesi[['name']], line = seriesi[['line']])
    # 
    #     }
    # 
    #     # https://plotly.com/r/figure-labels/
    # 
    #     niso3locid <- msglobalsf("COUNTRIESALL")
    #     niso3locid <- niso3locid[niso3locid$LongName == input$CountrySelectionOrigin,]
    #     niso3locid <- paste0(niso3locid$LongName, "_", niso3locid$LocID, "_", niso3locid$ISO3)
    #     stitle <- paste0("Trends in total migrant stock by country or area of origin, ", niso3locid, " (dest. = ", input$CountrySelection, ")")
    # 
    #     p <- layout(p, title = stitle, xaxis = list(title = 'Year'), yaxis = list(title = 'Number of migrants')) # , plot_bgcolor = "#e5ecf6"
    # 
    # 
    # }) # end of renderPlotly()

}) # end of observeEvent(input$CountrySelectionOrigin 


output$about_ta <- renderImage({
    
    message("renderImage() called")
    
    fname <- normalizePath(file.path('./images/about_ta.jpg'))
    
    # Return a list containing the filename
    
    # # When input$n is 1, filename is ./images/image1.jpeg
    # filename <- normalizePath(file.path('./images',
    #                                     paste('image', input$n, '.jpeg', sep='')))
    #
    # # Return a list containing the filename
    list(src = fname)
}, deleteFile = FALSE) # end of renderImage

output$about_ka <- renderImage({
    
    message("renderImage() called")
    
    fname <- normalizePath(file.path('./images/about_ka.jpg'))
    
    # Return a list containing the filename
    
    # # When input$n is 1, filename is ./images/image1.jpeg
    # filename <- normalizePath(file.path('./images',
    #                                     paste('image', input$n, '.jpeg', sep='')))
    #
    # # Return a list containing the filename
    list(src = fname)
}, deleteFile = FALSE) # end of renderImage


# output$about_ka <- renderImage({
#     fname <- normalizePath(file.path('./images/about_ka.jpg'))
#     list(src = fname)
# }, deleteFile = FALSE)
# 


# @@@testButton
observeEvent(input$testButton, {

# loadEmpiricalData(IncludeRevisions = TRUE)
#     
# # all empirical and revision data 
# df <- msglobalsf("EmpiricalData")
# LocID <- msglobalsf("LocID")
# ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, ".csv")
# write.csv(df, ofname, row.names=FALSE)
# output$textOutput <- renderText(ofname)
#     
# # save EmpiricalDataTotal
# df <- msglobalsf("EmpiricalDataTotal")
# LocID <- msglobalsf("LocID")
# ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_total.csv")
# write.csv(df, ofname, row.names=FALSE)
# output$textOutput <- renderText(ofname)
# 
# SeriesNames <- unique(df$source)
# # SeriesNames <- paste(SeriesNames, sep = "<br>")
# # str1 <- paste("You have selected", input$var)
# # str2 <- paste("You have chosen a range that goes from",
# #               input$range[1], "to", input$range[2])
# # HTML(paste(str1, str2, sep = '<br/>'))
#    
# ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_series.txt")
# sink(ofname)
# sink()
# print(SeriesNames)
# 
# df <- msglobalsf("EmpiricalDataTotalOrigin")
# LocIDorg <- msglobalsf("LocIDorg")
# ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_", LocIDorg, "_origin.csv")
# write.csv(df, ofname, row.names=FALSE)
# 
# # output$textOutput <- renderText({ HTML(paste(SeriesNames, collapse  = '<br>')) })
    # output$textOutput <- renderText({ "Test button" })
    # 
    # output$textOutput <- renderText({ 
    #     
    #     paste0("Sys.getenv('SHINY_PORT') = ", Sys.getenv('SHINY_PORT'),
    #            
    #            "session$clientData$url_hostname = ", session$clientData$url_hostname, "<br>",
    #            
    #            "Sys.getenv(USERNAME) = ", Sys.getenv("USERNAME"), 
    #            
    #            "Sys.getenv(COMPUTERNAME) = ", Sys.getenv("COMPUTERNAME") 
    #            ) 
    #     
    #     })
    
    output$textOutput <- renderUI({
        HTML(paste("Sys.getenv('SHINY_PORT') = ", Sys.getenv('SHINY_PORT'), sep="<br/>", "session$clientData$url_hostname =", session$clientData$url_hostname), sep="<br/>", "USERNAME = ", Sys.getenv("USERNAME"), sep="<br/>", "COMPUTERNAME = ", Sys.getenv("COMPUTERNAME"), sep="<br/>")
        # HTML(paste(, , sep="<br/>")),
        # HTML(paste("USERNAME", Sys.getenv("USERNAME"), sep="<br/>")),
        # HTML(paste("COMPUTERNAME", Sys.getenv("COMPUTERNAME"), sep="<br/>"))
    })
    
    
    # EmpiricalDataTotalOrigin <- msglobalsf("EmpiricalDataTotalOrigin")
    # 
    # 
    # df <- msglobalsf("EmpiricalData")
    # df <- df %>% filter(df$source == "Rev2020")
    # df <- df[df$sex == 0 & df$age == 0 & df$agelength == 0,] 
    # df <- df[df$year == max(df$year),]
    # e <- df$LocIDorg ==0
    # dftot <- df[e,];
    # df <- df[!e,]
    # 
    # 
    # 
    # CountriesOrg <- msglobalsf("COUNTRIESALL")
    # CountriesOrg$LocIDorg <- CountriesOrg$LocID
    # # CountriesOrg <- CountriesOrg %>% select(LocIDorg, LongName)
    # CountriesOrg <- merge(CountriesOrg, df, by = c("LocIDorg"), all.x=TRUE)  # left join (uncomment code above to get file names)
    # 
    # CountriesOrg <- CountriesOrg %>% select(LocIDorg, LongName, value)
    # CountriesOrg$value[is.na(CountriesOrg$value)] <- 0  # NA to zeros (for the countries with no estimates)
    # 
    # CountriesOrg <- CountriesOrg %>% arrange(desc(value), LongName)
    # 
    # 
    # # 
    # # CountriesOrg <- CountriesOrg %>% filter(CountriesOrg$LocID %in% c(250, 276))
    # # 
    # updateSelectInput(session, "CountrySelectionOrigin", choices = CountriesOrg$LongName, selected = CountriesOrg$LongName[1]) #@@@CountriesOrg
    # 
    # 
    # browser()  
    #   
    # !!!!
#    
})

observeEvent(input$btnSaveDatasets, {
    
    # browser()
    
    loadEmpiricalData("migrantstock_shiny", IncludeRevisions = TRUE)
    
    # all empirical and revision data 
    df <- msglobalsf("EmpiricalData")
    LocID <- msglobalsf("LocID")
    ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, ".csv")
    write.csv(df, ofname, row.names=FALSE)
    output$textOutput <- renderText(ofname)
    
    # save EmpiricalDataTotal
    df <- msglobalsf("EmpiricalDataTotal")
    LocID <- msglobalsf("LocID")
    ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_total.csv")
    write.csv(df, ofname, row.names=FALSE)
    output$textOutput <- renderText(ofname)
    
    # Save Rev 2023
    dfrev <- df[df$source == msglobalsf("ESTIMATED_SERIES_NAME"),]
    ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_", msglobalsf("ESTIMATED_SERIES_NAME"), "_final.csv")
    write.csv(dfrev, ofname, row.names=FALSE)
    
    SeriesNames <- unique(df$source)
    # SeriesNames <- paste(SeriesNames, sep = "<br>")
    # str1 <- paste("You have selected", input$var)
    # str2 <- paste("You have chosen a range that goes from",
    #               input$range[1], "to", input$range[2])
    # HTML(paste(str1, str2, sep = '<br/>'))
    
    ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_series.txt")
    #sink(ofname)
    write.csv(SeriesNames, ofname, row.names=FALSE)
    #print(SeriesNames)
    
    dforg <- msglobalsf("EmpiricalDataTotalOrigin")
    LocIDorg <- msglobalsf("LocIDorg")
    ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_", LocIDorg, "_origin.csv")
    write.csv(dforg, ofname, row.names=FALSE)
    
    # sourceu <- unique(df$source);
    
    # output$textOutput <- renderText({ HTML(paste(SeriesNames, collapse  = '<br>')) })
    # output$textOutput <- renderText({ "xxxxxxx" })
    
    ofname <- paste0(msglobalsf("TEMPORARY_FOLDER"), LocID, "_.RData")
    save(df, dforg, dfrev, SeriesNames, file = ofname)
    
}) # end of observeEvent(input$btnSaveDatasets

# observe({
#     v <- input$CountrySelection
#     Countries <- msglobalsf("COUNTRIES")
#     idx <- which(Countries$LongName == v)
#     
#     # Control the value, min, max, and step.
#     # Step size is 2 when input value is even; 1 when value is odd.
#     
#     updateSliderInput(session, "totallideranimation", value = idx, min = 1, max = length(Countries$LongName), step = 1)
#     
#     # updateSliderInput(session, "totallideranimation", value = 1,
#     #                   min = 1, max = 5, step = 1)
# })


# render the plot with valid empirical data @@@plotly
# Get reactive expression to introduce dependency of  input$CountrySelection and input$totalseriesfilter
# this function runs each time a user changes any of these controls 
output$plottotal <- renderPlotly({
    
    # browser()
    
    
    # Using this reactive expression makes renderPlotly() function to be dependent on input$totalseriesfilter and 
    # input$CountrySelection.  As a result, the plot will be updated if any of the controls will change.
    ds <- empiricaltotaldataset()
    
    # req() for “required.”
    # When the input to req() is not true, it sends a special signal to tell Shiny
    # that the reactive does not have all the inputs that it requires, so it should be
    # “paused.”
    
    # Having input$CountrySelection in the function body makes 
    # it is reactive for any changes in the list of countries 
    # Requesting req(input$CountrySelection) to be initialized we avoid running this function with uninitialized controls
    req(input$CountrySelection)
    
    # In reactive context if we use input$totalseriesfilter here, the function renderPlotly will called each time input changes
    # v  <- input$totalseriesfilter
    # v <- input$CountrySelection

    # browser()
    
    # EmpiricalDataTotal <- EMPIRICALDATATOTAL

    LocID <- msglobalsf("LocID")
    
    df <- msglobalsf("EmpiricalDataTotal")  # the current dataset with the empirical data for totals 
    
    # LocID <- NULL
    if(is.null(LocID) | is.null(df)){
        p <- plot_ly()
        p <- layout(p, title = 'No data found')
        return(p)
    }

    df$sourcepu <- mssource2sourcepu(df)

    # vname <- paste("v", 1, sep="", collapse=NULL)
    #
    # quo_x <- enquo(vname)
    #
    # fig <- fig %>% add_trace(y = ~quo_x, name = 'v1', mode = 'lines+markers')
    
    # for (i in 1:length(SeriesNames)) {
    #
    #    vname <- paste("v", i, sep="", collapse=NULL)
    #
    #    vname_quo <- enquo(vname)
    #
    #    print(vname_quo)
    #
    #    fig <- fig %>% add_trace(y = ~vname_quo, name = 'xxx', mode = 'lines+markers')
    #
    # }
    
    # i <- 1
    # vname <- paste("v", i, sep="", collapse=NULL)
    # vname_quo <- enquo(vname)
    # print(vname_quo)
    # fig <- fig %>% add_trace(y = ~v1, name = 'xxx1', mode = 'lines+markers')
    #
    # i <- 2
    # vname2 <- paste("v", i, sep="", collapse=NULL)
    # vname_quo2 <- enquo(vname2)
    # print(vname_quo)
    # fig <- fig %>% add_trace(y = ~v2, name = 'xxx2', mode = 'lines+markers')
    
    # i <- 3
    # vname <- paste("v", i, sep="", collapse=NULL)
    # vname_quo <- enquo(vname)
    # print(vname_quo)
    # fig <- fig %>% add_trace(y = ~!!vname_quo, name = 'xxx', mode = 'lines+markers')
    
    
    # my_lines <- list(
    #   list(x=1:10, y=2:11, color='red'),
    #   list(x=1:10, y=0:9, color='blue'),
    #   list(x=1:2, y=3:4, color='green')
    # )
    
    
    # my_lines <- list(
    #   list(x=EmpiricalDataTotalWide$yearref, y=EmpiricalDataTotalWide$v1, color='red', name = 'v1'),
    #   list(x=EmpiricalDataTotalWide$yearref, y=EmpiricalDataTotalWide$v2, color='blue', name = 'v2'),
    #   list(x=EmpiricalDataTotalWide$yearref, y=EmpiricalDataTotalWide$v3, color='green', name = 'v3')
    # )
    
    # my_lines <- list()
    #
    #
    # SeriesNames <- unique(EmpiricalDataTotal$source)
    #
    # for (i in 1:length(SeriesNames)) {
    #
    #   newname <- paste("v", i, sep="", collapse=NULL)
    #
    #   df <- EmpiricalDataTotal  %>%
    #     filter(source == SeriesNames[[i]]) %>%
    #     select(yearref, value)
    #
    #     my_lines <- append(my_lines, list(x = df$yearref, y = df$value, name = SeriesNames[[i]]))
    #
    #   # if (i == 1) {
    #   #
    #   #   EmpiricalDataTotalWide <- df
    #   #
    #   # } else {
    #   #
    #   #   EmpiricalDataTotalWide <- EmpiricalDataTotalWide %>% full_join(df, by = "yearref")
    #   #
    #   # }
    #
    # }
    
    # browser()
    
    
    
###############################################################
# old code with double loop
    # # produce attributes for the series plotted 
    # series <- list()
    # SeriesNames <- unique(EmpiricalDataTotal$source)
    # 
    # # stl <- msplotlystyle(SeriesNames) # !!!!!
    # 
    # 
    # for (i in 1:length(SeriesNames)) {
    #     
    #     # newname <- paste("v", i, sep="", collapse=NULL)
    #     
    #     seriesname <- SeriesNames[[i]]
    #     
    #     # browser()
    #     
    #     df <- EmpiricalDataTotal %>%
    #         dplyr::filter(source == seriesname)
    #     
    #     pu <- unique(df$PopulationUniverse)
    #     
    #     # df <- df %>% select(yearref, value)
    #     
    #     # l <- list(x = df$yearref, y = df$value, name = SeriesNames[[i]])
    #     
    #     if(length(pu) > 1) {
    #         # multiple PopulationUniverse per series
    #         for (j in 1:length(pu)) {
    #             
    #             s <- paste0(seriesname, " | ", pu[j]);
    #             
    #             df1 <- df %>% dplyr::filter(PopulationUniverse == pu[j])
    #             
    #             l <- list(x = df1$yearref, y = df1$value, name = s, mode = 'lines+markers') # 
    #             
    #             l1 <- msplotlystyle(paste0(seriesname, '_', pu[j])) # get styles for plotly, e.g. UNHCR_2
    #             
    #             l$marker  <- l1$marker
    #             l$line    <- l1$line
    #             l$mode    <- l1$mode
    #             
    #             series[[length(series)+1]] <- l
    #         }
    #     }else{
    #         # single PopulationUniverse per series
    #         l <- list(x = df$yearref, y = df$value, name = seriesname, mode = 'lines+markers') # 
    #         
    #         l1 <- msplotlystyle(seriesname) # get styles for plotly
    #         l$marker  <- l1$marker
    #         l$line    <- l1$line
    #         l$mode    <- l1$mode
    #         
    #         series[[length(series)+1]] <- l
    #         # series[[i]] <- l
    #     }
    #     
    # }
##########################################################################################

    # produce attributes for the series plotted 
    series <- list()
    SeriesNames <- unique(df$sourcepu)
    
    # stl <- msplotlystyle(SeriesNames) # !!!!!
    
    for (i in 1:length(SeriesNames)) {
        
        # newname <- paste("v", i, sep="", collapse=NULL)
        
        seriesname <- SeriesNames[[i]]
        
        # dfi <- df %>% dplyr::filter(source == seriesname)
        dfi <- df %>% dplyr::filter(sourcepu == seriesname)
        
        # sourcepu
        
        # single PopulationUniverse per series
        l <- list(x = dfi$yearref, y = dfi$value, name = seriesname, mode = 'lines+markers') # 
        
        l1 <- msplotlystyle(seriesname) # get styles for plotly
        l$marker  <- l1$marker
        l$line    <- l1$line
        l$mode    <- l1$mode
        
        series[[length(series)+1]] <- l
        # series[[i]] <- l
    }
    
        
    # p <- plot_ly(EmpiricalDataTotal, x = ~yearref, y = ~value, split = ~source)
    
    # https://stackoverflow.com/questions/38828875/add-multiple-lines-to-a-plot-ly-graph-with-add-trace
    # evaluate = TRUE
    
    p <- plot_ly()
    # p <- plot_ly( height = 600)
    
    # p
    for(i in 1:length(series)){
        
        seriesi <- series[[i]]
        print(seriesi$name)
        
        if(seriesi$name == "UNHCR|2"){
            seriesi$name = seriesi$name
        }
        # print(line)
        
        p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']], mode = seriesi[['mode']], name = seriesi[['name']], line = seriesi[['line']], marker = seriesi[['marker']]) # marker=list(color=line[['color']]),
        # p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']])
        
        # p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']], mode = seriesi[['mode']], name = seriesi[['name']], line = seriesi[['line']])
        
    }

    # https://plotly.com/r/figure-labels/
    
    niso3locid <- msglobalsf("COUNTRIESALL")
    niso3locid <- niso3locid[niso3locid$LongName == input$CountrySelection,]
    niso3locid <- paste0(niso3locid$LongName, "_", niso3locid$LocID, "_", niso3locid$ISO3)
    stitle     <- paste("Total, ", niso3locid, sep="", collapse=NULL)  # need some space for the plot controls
    # stitle     <- paste("Trends in total migrant stock, ", niso3locid, sep="", collapse=NULL)
    
    # browser()
    p <- layout(p, title = stitle, xaxis = list(title = 'Year'), yaxis = list(title = 'Number of migrants')) # , plot_bgcolor = "#e5ecf6"
    
    #   series <- unique(EmpiricalDataTotal$source)
    #
    #   for (i in 1:length(series)) {
    #
    #     fig <- plot_ly()
    #
    #
    #     x0 = c(1, 2)
    #     y0 = c(1, 2)
    #
    #     fig <- fig %>% add_trace(x = ~x0, y = ~y0, name = 'trace 1', mode = 'lines+markers')
    #
    #     # print(series[[i]])
    #
    #
    #   }
    #   updateSelectInput
    #
    #
    # #  plot_ly(EmpiricalDataTotal, x = ~yearref, y = ~value, type = 'scatter', mode = 'markers')
    #
    # #  plot_ly(iris, x = ~get(input$choice), y = ~Sepal.Length, type = 'scatter', mode = 'markers')  %>%
    # # add_trace(type = "scatter", mode = "markers+lines")
    #
    
    # trace_0 <- rnorm(100, mean = 5)
    # trace_1 <- rnorm(100, mean = 0)
    # trace_2 <- rnorm(100, mean = -5)
    # x <- c(1:100)
    #
    # data <- data.frame(x, trace_0, trace_1, trace_2)
    #
    # fig <- plot_ly(data, x = ~x)
    #
    # fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines')
    # fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')
    # fig <- fig %>% add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')

    
    # msglobalsf("UserData", p)  #!!! used for countries 
    # export(p, file = "c:/temp/plot.png") 

}) # end of renderPlotly()


#   # render the sub plot
#   # LocIDorgs <- c(36, 156)
#   # LocIDorgs <- c(4,	8,	12,	16,	20,	24,	28,	31,	32,	36,	40,	44,	48,	50,	51,	52,	56,	60,	64,	68,	70,	72,	76,	84,	90,	92,	96,	100,	104,	108,	112,	116,	120,	124,	132,	136,	140,	144,	148,	152,	156,	170,	174,	175,	178,	180,	184,	188,	191,	192,	196,	203,	204,	208,	212,	214,	218,	222,	226,	231,	232,	233,	234,	238,	242,	246,	250,	254,	258,	262,	266,	268,	270,	275,	276,	288,	292,	296,	300,	304,	308,	312,	316,	320,	324,	328,	332,	336,	340,	344,	348,	352,	356,	360,	364,	368,	372,	376,	380,	384,	388,	392,	398,	400,	404,	408,	410,	414,	417,	418,	422,	426,	428,	430,	434,	438,	440,	442,	446,	450,	454,	458,	462,	466,	470,	474,	478,	480,	484,	492,	496,	498,	499,	500,	504,	508,	512,	516,	520,	524,	528,	531,	533,	534,	535,	540,	548,	554,	558,	562,	566,	570,	578,	580,	583,	584,	585,	586,	591,	598,	600,	604,	608,	616,	620,	624,	626,	630,	634,	638,	642,	643,	646,	654,	659,	660,	662,	666,	670,	674,	678,	682,	686,	688,	690,	694,	702,	703,	704,	705,	706,	710,	716,	724,	728,	729,	732,	740,	748,	752,	756,	760,	762,	764,	768,	772,	776,	780,	784,	788,	792,	795,	796,	798,	800,	804,	807,	818,	826,	830,	833,	834,	840,	850,	854,	858,	860,	862,	876,	882,	887,	894)
# 
#   LocIDorgs <- c(36, 156)
# 
#   # need lapply to update in  the loop
#   lapply(LocIDorgs, function(LocIDorgi) {
# 
#     # showNotification(paste0("Current country = ", LocIDorgi))
#     # Sys.sleep(0.1)
# 
#     df0 <- EmpiricalData  %>%
#       filter(sex == 0 & age == 0 & agelength == 0 & LocIDorg == LocIDorgi & PopulationUniverse == EmpiricalDataPopulationUniverse) %>%
#       select(yearref, value, source)  %>%
#       arrange(yearref, source)
# 
#     # empty plot
#     # if (LocIDorgi == 4){
#     #     renderPlotly({
#     #         plotly_empty() %>%
#     #         config(staticPlot = TRUE)
#     #     })
#     #
#     #   return(NULL)
#     # }
# 
#     # if (LocIDorgi == 4){
#     #   renderPlotly({NULL})
#     #   return(NULL)
#     # }
# #
# #   id <- paste0("dt", i)
# #   print(id)
# #
# 
#     # output[[id]] <- renderTable(table[[i]])
# 
#     plotname <- paste0("plotLocIDorg", LocIDorgi);
# 
#     output[[plotname]] <- renderPlotly({
# 
#       # p <- plot_ly()
#       # # l <- my_LocIDorgs[[i]]
#       # # p <- layout(p, title = l$plotname)
#       #
#       #
#       # # # write log file
#       # fh <- file(LOGFILE, open = "at")
#       # msg <- RV$LocIDorgSelect
#       # # # cat(format(Sys.time(), "%Y-%m-%d %X"), ":", msg, "\n", append = T, file = fh)
#       # cat(msg, "\n", append = T, file = fh)
#       #
#       # # msg <- i
#       # # # # cat(format(Sys.time(), "%Y-%m-%d %X"), ":", msg, "\n", append = T, file = fh)
#       # # cat(msg, "\n", append = T, file = fh)
#       #
#       # close(fh)
#       #
#       #
#       # return(p)
# 
#       # LocIDorgSelect <- 156
#       # LocIDorgSelect <- -1
# 
#       # df0 <- EmpiricalData  %>%
#       #   filter(sex == 0 & age == 0 & agelength == 0 & LocIDorg == LocIDorgSelect & PopulationUniverse == EmpiricalDataPopulationUniverse) %>%
#       #   select(yearref, value, source)  %>%
#       #   arrange(yearref, source)
#       #
#       # # summary(df0)
#       #
#       #
#       # if (nrow(df0) == 0){
#       #   p <- plot_ly()
#       #   p <- layout(p, title = 'No data found')
#       #   return(p)
#       # }
#       #
# 
#       my_lines <- list()
# 
#       SeriesNames <- unique(df0$source)
# 
#       for (i in 1:length(SeriesNames)) {
# 
#         # newname <- paste("v", i, sep="", collapse=NULL)
# 
#         seriesname <- SeriesNames[[i]]
# 
# 
#         df <- df0 %>%
#           filter(source == seriesname) %>%
#           select(yearref, value)
# 
#         # l <- list(x = df$yearref, y = df$value, name = SeriesNames[[i]])
# 
#         l <- list(x = df$yearref, y = df$value, name = seriesname)
# 
# 
#         if (identical(seriesname, "NSO")){
# 
#           # triangle square
#           l$marker <- list(symbol = 'triangle', color = 'rgb(0, 255, 255)', size = 5, line = list(color = 'rgb(0, 0, 0)', width = 1))
# 
#         }else{
# 
#           l$marker <- list()
# 
#         }
# 
#         my_lines[[i]] <- l
# 
#       }
# 
#       p <- plot_ly()
#       for(line in my_lines) {
#         p <- add_trace(p, y=line[['y']], x=line[['x']], mode = 'lines+markers', name = line[['name']], marker = line[['marker']]) # marker=list(color=line[['color']]),
#       }
# 
#       df <- msglobalsf("COUNTRIES")  %>% filter(LocID == LocIDorgi)
# 
#       stitle <- paste("Country of origin, ", df$LongName, '_', LocIDorgi, sep="", collapse=NULL)
# 
#       p <- layout(p, title = stitle, xaxis = list(title = 'Year'), yaxis = list(title = '# of migrants')) # , plot_bgcolor = "#e5ecf6"
# 
#     })
# 
# 
#   })


# shinyjs::hide(id = "plotLocIDorg4")

# shinyjs::show(id = "myBox")

# my_LocIDorgs <- list()

# for (i in 1:length(LocIDorgs)) {
#
#   df <- EmpiricalData  %>%
#     filter(sex == 0 & age == 0 & agelength == 0 & LocIDorg == LocIDorgs[i] & PopulationUniverse == EmpiricalDataPopulationUniverse) %>%
#     select(yearref, value, source)  %>%
#     arrange(yearref, source)
#
#   plotname <- paste("plotLocIDorg", LocIDorgs[i], sep="", collapse=NULL)
#
#
#   l <- list(df = df, plotname = plotname)
#
#   my_LocIDorgs[[i]] <- l
#
#   # LocIDorgSelect <<- LocIDorgs[i]
#   #
#   # plotname <- paste("plotLocIDorg", LocIDorgSelect, sep="", collapse=NULL)
#   #
#   # # write log file
#   # fh <- file(LOGFILE, open = "at")
#   # msg <- plotname
#   # # cat(format(Sys.time(), "%Y-%m-%d %X"), ":", msg, "\n", append = T, file = fh)
#   # cat(msg, "\n", append = T, file = fh)
#   # close(fh)
#   #
#   # showNotification(plotname)
#
#
#
# } # end of LocIDorgs loop

# render the plot with valid empirical data @@@plotly
# Get reactive expression to introduce dependency of  input$CountrySelection and input$totalseriesfilter
# this function runs each time a user changes any of these controls 
output$plotorigin <- renderPlotly({
    
    # Using this reactive expression makes renderPlotly() function to be dependent on input$totalseriesfilter and 
    # input$CountrySelection.  As a result, the plot will be updated if any of the controls will change.
    # ds <- empiricaltotaldataset()
    
    # req() for “required.”
    # When the input to req() is not true, it sends a special signal to tell Shiny
    # that the reactive does not have all the inputs that it requires, so it should be
    # “paused.”
    
    # Having input$CountrySelection in the function body makes 
    # it is reactive for any changes in the list of countries 
    # Requesting req(input$CountrySelection) to be initialized we avoid running this function with uninitialized controls
    req(input$CountrySelectionOrigin)
    
    # In reactive context if we use input$totalseriesfilter here, the function renderPlotly will called each time input changes
    # v  <- input$totalseriesfilter
    # v <- input$CountrySelection
    
    # browser()
    
    # EmpiricalDataTotal <- EMPIRICALDATATOTAL
    
    LocID <- msglobalsf("LocIDorg")
    
    df <- msglobalsf("EmpiricalDataTotalOrigin")  # the current dataset with the empirical data for totals 
    
    # LocID <- NULL
    if(is.null(LocID) | is.null(df)){
        p <- plot_ly()
        p <- layout(p, title = 'No data found')
        return(p)
    }
    
    df$sourcepu <- mssource2sourcepu(df)

    # produce attributes for the series plotted 
    series <- list()
    SeriesNames <- unique(df$sourcepu)
    
    # stl <- msplotlystyle(SeriesNames) # !!!!!
    
    for (i in 1:length(SeriesNames)) {
        
        # newname <- paste("v", i, sep="", collapse=NULL)
        
        seriesname <- SeriesNames[[i]]
        
        # dfi <- df %>% dplyr::filter(source == seriesname)
        dfi <- df %>% dplyr::filter(sourcepu == seriesname)
        
        # sourcepu
        
        # single PopulationUniverse per series
        l <- list(x = dfi$yearref, y = dfi$value, name = seriesname, mode = 'lines+markers') # 
        
        l1 <- msplotlystyle(seriesname) # get styles for plotly
        l$marker  <- l1$marker
        l$line    <- l1$line
        l$mode    <- l1$mode
        
        series[[length(series)+1]] <- l
        # series[[i]] <- l
    }
    
    
    # p <- plot_ly(EmpiricalDataTotal, x = ~yearref, y = ~value, split = ~source)
    
    # https://stackoverflow.com/questions/38828875/add-multiple-lines-to-a-plot-ly-graph-with-add-trace
    # evaluate = TRUE
    
    p <- plot_ly()
    # p
    for(i in 1:length(series)){
        
        seriesi <- series[[i]]
        print(seriesi$name)
        
        if(seriesi$name == "UNHCR|2"){
            seriesi$name = seriesi$name
        }
        # print(line)
        
        p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']], mode = seriesi[['mode']], name = seriesi[['name']], line = seriesi[['line']], marker = seriesi[['marker']]) # marker=list(color=line[['color']]),
        # p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']])
        
        # p <- add_trace(p, x = seriesi[['x']], y = seriesi[['y']], mode = seriesi[['mode']], name = seriesi[['name']], line = seriesi[['line']])
        
    }

    # https://plotly.com/r/figure-labels/
    
    niso3locid <- msglobalsf("COUNTRIESALL")
    niso3locid <- niso3locid[niso3locid$LongName == input$CountrySelectionOrigin,]
    niso3locid <- paste0(niso3locid$LongName, "_", niso3locid$LocID, "_", niso3locid$ISO3)
    # stitle     <- paste("Total, ", niso3locid, sep="", collapse=NULL)  # need some space for the plot controls
    stitle     <-paste0("Trends in number of migrants by country of origin, ", niso3locid, " (", input$CountrySelection, ")")
    # stitle     <- paste("Trends in total migrant stock, ", niso3locid, sep="", collapse=NULL)
    
    # browser()
    p <- layout(p, title = stitle, xaxis = list(title = 'Year'), yaxis = list(title = 'Number of migrants')) # , plot_bgcolor = "#e5ecf6"
    
    #   series <- unique(EmpiricalDataTotal$source)
    #
    #   for (i in 1:length(series)) {
    #
    #     fig <- plot_ly()
    #
    #
    #     x0 = c(1, 2)
    #     y0 = c(1, 2)
    #
    #     fig <- fig %>% add_trace(x = ~x0, y = ~y0, name = 'trace 1', mode = 'lines+markers')
    #
    #     # print(series[[i]])
    #
    #
    #   }
    #   updateSelectInput
    #
    #
    # #  plot_ly(EmpiricalDataTotal, x = ~yearref, y = ~value, type = 'scatter', mode = 'markers')
    #
    # #  plot_ly(iris, x = ~get(input$choice), y = ~Sepal.Length, type = 'scatter', mode = 'markers')  %>%
    # # add_trace(type = "scatter", mode = "markers+lines")
    #
    
    # trace_0 <- rnorm(100, mean = 5)
    # trace_1 <- rnorm(100, mean = 0)
    # trace_2 <- rnorm(100, mean = -5)
    # x <- c(1:100)
    #
    # data <- data.frame(x, trace_0, trace_1, trace_2)
    #
    # fig <- plot_ly(data, x = ~x)
    #
    # fig <- fig %>% add_trace(y = ~trace_0, name = 'trace 0',mode = 'lines')
    # fig <- fig %>% add_trace(y = ~trace_1, name = 'trace 1', mode = 'lines+markers')
    # fig <- fig %>% add_trace(y = ~trace_2, name = 'trace 2', mode = 'markers')
    
    
}) # end of renderPlotly()


observe({
    
    # shinyjs::hide("testTotalButton")
    
    # Hide this button for the web application and display of run locally 
    
    # if(session$clientData$url_hostname == "migrantstock.shinyapps.io"){
    if(!(session$clientData$url_hostname == "127.0.0.1")){
        # hide debug buttons and panel if running online
            # showNotification("Server function is called")
        shinyjs::hide("testTotalButton")
        shinyjs::hide("testAgeButton")
        # hideTab(inputId = "maintabsetPanel", target = "4") # hide Debug panel
    }else{
        shinyjs::show("testTotalButton")
        shinyjs::show("testAgeButton")
    }
    
    # showNotification(session$clientData$url_hostname)
    # Sys.sleep(3)
    #    
    # browser()
    # print(reactiveValuesToList(session$clientData))
})


# session$clientData$url_hostname =
#     

# @@@ Map, migrants by country of origin 
output$msmap <- renderPlotly({
    
    # make this function  reactive to changes in the country selection 
    tmp <- input$CountrySelection
    
    # type = "choropleth",
    # http://plotly.com/r/choropleth-maps/

    # browser()
     
    # data on distribution by country of origin
    df <- msglobalsf("EmpiricalData") %>% filter(source == "UNPD_2024" & age == 0 & agelength == 0 & LocIDorg > 0 & LocIDorg < 900 & sex == 0) 
    
    # current year
    curyear <- input$msslider + 0.5
    df <- df %>% filter(yearref == curyear)
    if(nrow(df) == 0){
        # select last year
        curyear <- tail(msglobalsf("YEARS_PUBLISHED"))
        df <- df %>% filter(yearref == curyear)    
    }

    # get ISO3 coding    
    # browser()
    
    niso3locid <- locid2longname(df$LocIDorg, niso3locid = TRUE)
    niso3locid <- str_split(niso3locid, "_")
    ISO3 <- NULL
    for(i in 1:length(niso3locid)){
        ISO3 <- c(ISO3, niso3locid[[i]][[2]])
    }
    df$iso_alpha <- ISO3
    
    #browser()
    
    # rftLocation <- msglobalsf("rftLocation")
    # rftLocation <- rftLocation[match(df$LocIDorg, rftLocation$LocID), ]
    # df$iso_alpha <- rftLocation$ISO3
    
    # choropleth map
    p <- plot_ly(
        data = df,
        type = "choropleth",
        locations = ~iso_alpha,
        z = ~value,
        text = ~iso_alpha,
        #colors = "Purples",
        colors = "YlGnBu",  # https://plotly.com/r/builtin-colorscales/
        #colors = "RdPu",
        locationmode = "ISO-3"
    ) 

    # no zoom on mouse scroll    
    p <- config(p, scrollZoom = FALSE)

    p <- p %>% colorbar(title = "Migrants")
     
    p <-  layout(p,
            title = paste0("Migrants by country of origin"),
            geo = list(
              showframe = F, 
              showcoastlines = TRUE, 
              showland = F,
              # landcolor = toRGB("grey90"),
              projection = list(type = "mercator"), 
              showcountries = T,
              # bgcolor = toRGB("white", alpha = 0),
              # list(domain = list(x = c(0, .6), y = c(0, .6))),
              
              # Limit the range of latitudes 
              lataxis = list(range = c(-50, 90)) # Latitude range: 20°N to 60°N Remove Antarctica 
            ),
            margin = list(
              l = 0,  # Left margin
              r = 0,  # Right margin
              t = 40,  # Top margin
              b = 0   # Bottom margin
            )
    )

     # Highlight the current country on the map
     niso3locid <- locid2longname(msglobalsf("LocID"), niso3locid = TRUE)
     niso3locidarr <- unlist(str_split(niso3locid, "_"))
     
     df1 <- data.table(
         country = niso3locidarr[1],
         iso_alpha = niso3locidarr[2],
         value = c(1)
     )
     
     # cnumbers <- c(0, 0.05, 0.10, 0.15, 0.20, 0.25)
     # ccolors <- c("#EDF8E9", "#C7E9C0", "#A1D99B", "#74C476", "#31A354", "#006D2C")
     
     cnumbers <- c(-1, 0, 1)
     ccolors <- c("#FF0000", "#FF0000", "#FF0000")
     
     p <- add_trace(p,         
                    data = df1,
                    type = "choropleth",
                    #type="choroplethmapbox",
                    locations = ~iso_alpha,
                    z = 1, 
                    showscale = F,
                    #colors = "Reds",
                    colorscale=mapply(c, cnumbers, ccolors, SIMPLIFY = FALSE),   # custom color scale
                    #colorscale = c(c(-1, 'rgb(255,0,0)'), c(1, 'rgb(255,0,0)')),
                    locationmode = "ISO-3",
                    #reversescale = T,
                    zmin = -1,
                    zmax = 1
                    # hoverlabel = NULL
     )
     
     # https://plotly.com/r/reference/choropleth/
     
})


# bar, top ten countries of origin 
output$msbar10 <- renderPlotly({
    
    # make this function  reactive to changes in the country selection 
    tmp <- input$CountrySelection
    # http://plotly.com/r/choropleth-maps/
    

    df <- msglobalsf("EmpiricalData") %>% filter(source == "UNPD_2024" & age == 0 & agelength == 0 & LocIDorg > 0 & LocIDorg < 900 & sex == 0) 
    
    # df <- df %>% filter(yearref == 1990.5)
    curyear <- input$msslider + 0.5
    df <- df %>% filter(yearref == curyear)
    
    if(nrow(df) == 0){
        curyear <- tail(msglobalsf("YEARS_PUBLISHED"))
        df <- df %>% filter(yearref == curyear)    
    }

    niso3locid <- locid2longname(msglobalsf("LocID"), niso3locid = TRUE)
    
    output$mstitle <- renderText({paste0(niso3locid, ", year = ", curyear)})
    
    df <- df %>% arrange(value)
    df <- df[rev(1:nrow(df)), ] # display the largest first

    if(nrow(df) > 10){df <- df[1:10,]} # select top 10 only

    xvalues <- df$value
    yvalues <- locid2longname(df$LocIDorg)
    
    # Create Choropleth Map
    p <- plot_ly(
        type = "bar",
        x = ~xvalues,
        y = ~reorder(yvalues, xvalues), 
        name = "TopTen",
        #colors = "Purples",
        #colors = "YlGnBu",
        #colors = "RdPu",
        orientation = 'h'
    ) 
    
    # https://plotly.com/r/builtin-colorscales/
    # fig <- fig %>% layout(title = 'Household savings & net worth for eight OECD countries',
    #                       legend = list(x = 0.029, y = 1.038,
    #                                     font = list(size = 10)),
    #                       margin = list(l = 100, r = 20, t = 70, b = 70),
    #                       paper_bgcolor = 'rgb(248, 248, 255)',
    #                       plot_bgcolor = 'rgb(248, 248, 255)')
    
    p <-  layout(p,
                 title = paste0("Top sending countries or areas of origin"),
                 xaxis = list(title = "Number of migrants"), 
                 yaxis = list(title = ""),
                 margin = list(l = 100, r = 20, t = 70, b = 70,  pad = 10)  # pad adds space between y-axis and labels
    )
    
})


###############################################################################
# Postscript, the end block of server() function, runs on initialization 


# Initialization code 

# browser()

#  print(msglobalsf("COUNTRIES")$LongName)
# updateSelectInput(session, "CountrySelection", choices = msglobalsf("COUNTRIES")$LongName, selected = "Canada"")
# updateSelectInput(session, "CountrySelectionOrigin", choices = msglobalsf("COUNTRIESALL")$LongName, selected = "India"")

# # Get 3 integers from 0 to 100
# # Use max=101 because it will never actually equal 101
# floor(runif(3, min=0, max=101))
# #> [1] 11 67  1

fname <- "data/countries.RData"
load(fname)
# print(df)
UserData <- NULL
UserData[["Countries"]] <- df
msglobalsf("UserData", UserData) 

# browser()

Countries <- msglobalsf("COUNTRIESALL")

if(length(Countries) == 0) {
    stop("invalid msglobalsf(\"COUNTRIESALL\")")
}

Countries <- Countries %>% filter(LocID %in% UserData$Countries$LocID)
msglobalsf("COUNTRIES", Countries)
    
# DataFolder <- "C:/akf/MUS/MS/scripts/migrantstock/data/"
# 
# df <- data.frame(LocID = c(380, 380), LocIDorg = c(642, 8))
# 
# ofname <- paste0(DataFolder, "countries")
# save(df, file = ofname)


# Countries <- msglobalsf("COUNTRIES")

if(nrow(Countries) < 1){
    stop("Number of countries must be > 1")
}

# get current LocID

LocID <- msglobalsf("LocID")

e <- nrow(msglobalsf("COUNTRIESALL")) == nrow(Countries) # complete list

if(is.null(LocID) & e){
    # get random country, use nrow because it will never actually equal nrow
    idx <- floor(runif(1, min=0, max=nrow(Countries)+1))
    LocID <- Countries$LocID[idx]
    msglobalsf("LocID", LocID)
}else{
    
    if(is.null(LocID)){
        LocID <- Countries$LocID[1]
    }

    idx <- which(Countries$LocID == LocID)
    if(length(idx) == 0){
        idx <- 1
    }
}

updateSelectInput(session, "CountrySelection", choices = Countries$LongName, selected = Countries$LongName[idx])

# update list of origins 
e <- UserData$Countries$LocID == LocID
CountriesOrg <- msglobalsf("COUNTRIESALL")

# CountriesOrg <- CountriesOrg %>% filter(LocID %in% UserData$Countries$LocIDorg[e])
# updateSelectInput(session, "CountrySelectionOrigin", choices = CountriesOrg$LongName, selected = CountriesOrg$LongName[1]) #@@@CountriesOrg
# msglobalsf("LocIDorg", CountriesOrg$LocID[1])

Ctr  <-  UserData$Countries[e, ]
Ctr$LocID <- Ctr$LocIDorg
Ctr$LocIDorg <- NULL
Ctr <- Ctr %>% left_join(CountriesOrg)
Ctr <- Ctr$LongName
Ctr <- factor(Ctr)
updateSelectInput(session, "CountrySelectionOrigin", choices = Ctr, selected = Ctr[1]) #@@@CountriesOrg

# e <- UserData$Countries$LocID == LocID
# CountriesOrg <- msglobalsf("COUNTRIESALL")
# CountriesOrg <- CountriesOrg %>% filter(LocID %in% UserData$Countries$LocIDorg[e])
# updateSelectInput(session, "CountrySelectionOrigin", choices = CountriesOrg$LongName, selected = CountriesOrg$LongName[1]) #@@@CountriesOrg

msglobalsf("LocIDorg", CountriesOrg$LocID[1])

# age groups 
agegrp <- factor(c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+"))
updateSelectInput(session, "AgeGroupSelection", choices = agegrp, selected = agegrp[1]) #@@@AgeGroupSelection

# Empirical age distributions


# updateSliderInput(session, "totallideranimation", value = 1,

# updateSliderInput(session, "totalslideranimation", value = idx, min = 1, max = length(Countries$LongName), step = 1)

# updateSliderInput(session, "totalslideranimation", value = 1, min = 1, max = 10, step = 1)
# updateSliderInput(session, "totalslideranimation", value = idx, min = 1, max = nrow(Countries), step = 1)

# updateSliderInput(session, "receive", value = val, min = floor(val/2), max = val+4, step = (val+1)%%2 + 1)

# cat(file=stderr(), paste0("Username: ",userdata$name, "\n"))

# cat(file=stderr(), paste0("Datum: ",Sys.time(), "\n")

# # autoInvalidate <- reactiveTimer(2000)
# for(i in 1:3){
#     click("testTotalButton")
#     Sys.sleep(3)
#     reactive(invalidateLater(1000))
# }

} # end of  server <- function(input, output, session){
################################################################################


######### not used 
# # Run in a dialog within R Studio
# runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1200, height = 600))
# 
# # Run in Viewer pane
# runGadget(ui, server, viewer = paneViewer(minHeight = 500))
# 
# # Run in browser
# runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))

# source("ui.r", local = TRUE)
# runGadget(shinyApp(ui = ui, server = server))

