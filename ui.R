library(shiny)
library(plotly)
library(shinyjs)

source("msglobalsf.r", local = TRUE)

# Set initial window size
# shinyOptions(height = 1600, width = 1800) -- doesn't work

ui = fluidPage(

 # includeCSS("cerulean.css"),
 
  tags$head(includeHTML("google-analytics.html")),
  useShinyjs(),
  wellPanel(
        #debugon #######################################################################      
        # h1(HTML("<span style=\"color:red\">The app is currently being updated. Please bear with us ...</span>")),  # style="color:red"
        #debugoff #######################################################################    
        h4("International Migrant Stock at a Glance"),
        HTML('<p>Draft application, not to be cited or reproduced | Version: 16 March, 2025</p>'),
  ),
  
  tabsetPanel(  id = "maintabsetPanel",  # Wickham_2021_Mastering-Shiny.pdf
    
    tabPanel("Total",
       fluidPage(
           
           # tags$style(HTML("#total {border: 4px double red;}")),
           # some padding between panel and controls
           tags$style(HTML("#total {padding-top: 1em;}")), 

         div(
           fluidRow(id = "totalrow1",
             
            column(
                selectInput("CountrySelection", NULL, choices = NULL, selected = NULL),
                # selectInput("CountrySelection", "Country or Area", choices = NULL, selected = NULL),
                width = 5  # max 12
                # align = "left"
             ),
            
            column(
                actionButton(inputId = "prevButton", label = "<<"),
                width = 1
            ),
            
            column(
                actionButton(inputId = "nextButton", label = ">>"),
                width = 1
            ),
            
            column(
                selectInput("totalseriesfilter", NULL, c("All" = "all")),
                # selectInput("totalseriesfilter", NULL, c("All" = "all", "Revisions" = "rev", "UNHCR(-)" = "unhcrexcl", "UNHCR" = "unhcr")),
                width = 2
            ),
            
           ),
           style = "position:relative;z-index:10000;"
         ),
         
         #div(fluidRow(column(width = 10, style='padding:20px;'),style = "position:relative;z-index:10000;")),
         # plotlyOutput("plottotal"),
         plotlyOutput("plottotal", width = "100%", height = "600px"),
         
         #div(fluidRow(column(width = 10, style='padding:20px;'),style = "position:relative;z-index:10000;")),
         
         fluidRow(id = "totalrow2",
                  
                  column(
                      # Need to do it hidden so it is flashes in the online application
                      # I unhide this button if run locally 
                      hidden(actionButton(inputId = "testTotalButton", label = "testTotalButton")), 
                      width = 1
                  )
         )
         
         
       ), # end of fluid page
       value = 1
    ),
    
    
    tabPanel("Origin",
             fluidPage(
                 
                 div(
                     fluidRow(
                         column(
                             selectInput("CountrySelectionOrigin", "Country or Area of Origin", choices = NULL, selected = NULL),
                             # actionButton(inputId = "prevButtonOrigin", label = "<<"),
                             # actionButton(inputId = "nextButtonOrigin", label = ">>"),
                             width = 5
                         ),
                         
                     ),
                     style = "position:relative;z-index:10000;"
                 ),
                 
                 #div(fluidRow(column(width = 10, style='padding:20px;'),style = "position:relative;z-index:10000;")),
                 plotlyOutput("plotorigin", width = "100%", height = "600px")
                 
                 #div(fluidRow(column(width = 10, style='padding:20px;'),style = "position:relative;z-index:10000;")),
                 
             ), 
             value = 2
    ),

    tabPanel("Age",
             fluidPage(
                 
                 div(
                     fluidRow(
                         column(
                             selectInput("AgeGroupSelection", "Age Group", choices = NULL, selected = NULL),
                             # actionButton(inputId = "prevButtonOrigin", label = "<<"),
                             # actionButton(inputId = "nextButtonOrigin", label = ">>"),
                             width = 5
                         ),
                         
                     ),
                     
                     style = "position:relative;z-index:10000;" 
                 ),
                 
                 #div(fluidRow(column(width = 10, style='padding:20px;'),style = "position:relative;z-index:10000;")),
                 plotlyOutput("plotage", width = "100%", height = "600px")
                 
                 #div(fluidRow(column(width = 10, style='padding:20px;'),style = "position:relative;z-index:10000;")),
                 
             ), 
             value = 3
    ),
    
    
    tabPanel("Age Composition",
             fluidPage(

                 div(
                     fluidRow(
                         column(
                             selectInput("AgeEmpirical", "", choices = NULL, selected = NULL),
                             # actionButton(inputId = "prevButtonOrigin", label = "<<"),
                             # actionButton(inputId = "nextButtonOrigin", label = ">>"),
                             width = 5
                         ),

                     ),

                     fluidRow(id = "ageempiricalrow2",
                              column(
                                  # Need to do it hidden so it is flashes in the online application
                                  # I unhide this button if run locally
                                  hidden(actionButton(inputId = "testAgeButton", label = "testAgeButton")),
                                  width = 1
                              )
                     ),

                     style = "position:relative;z-index:10000;"
                 ),

                 #div(fluidRow(column(width = 10, style='padding:20px;'),style = "position:relative;z-index:10000;")),
                 plotlyOutput("plotageempirical", width = "100%", height = "600px")

             ),
             value = 4
    ),
    

    tabPanel("Migrant Stock",
             fluidPage(
                 
                 fluidRow(
                     column(
                         sliderInput("msslider", "Select year: ", min = 1990, max = 2024, value = 2024, step = 5, sep = ""), # https://stackoverflow.com/questions/26636335/formatting-number-output-of-sliderinput-in-shiny
                         width = 6
                     )
                 ),
                 
                 tags$hr(),  # This inserts a horizontal line
                 
                 
                 fluidRow(
                     column(
                         h4(span(textOutput("mstitle"), style="text-align:center")),  # style="color:red"
                         width = 12
                     )
                 ),
                 

                 
                 fluidRow(
                     column(
                         plotlyOutput("msbar10", width = "100%", height = "300px"),  # viewport percentage
                         width = 12
                     )
                 ),
                 
                 
                 fluidRow(
                     column(
                         plotlyOutput("msmap", width = "95vw", height = "95vh"),  # viewport percentage
                         width = 12
                     )
                 )
                 
                # plotlyOutput("msmap", width = "1600px", height = "1200px")
             ), value = 5
    ),
    
    # tabPanel("Age",
    #          fluidPage(
    #              
    #              tags$style(HTML("#age {padding-top: 1em;}")), 
    #              
    #              div(
    #                  fluidRow(id = "agerow1",
    #                           
    #                           column(
    #                               plotlyOutput("plotage0", width = "100%", height = "400px"),
    #                               width = 4
    #                           ),
    # 
    #                           column(
    #                               plotlyOutput("plotage5", width = "100%", height = "400px"),
    #                               width = 4
    #                           ),
    #                           
    #                           
    #                           
    #                           # column(
    #                           #     actionButton(inputId = "nextButton", label = ">>"),
    #                           #     width = 1
    #                           # ),
    #                           # 
    #                           # column(
    #                           #     selectInput("totalseriesfilter", NULL, c("All" = "all")),
    #                           #     # selectInput("totalseriesfilter", NULL, c("All" = "all", "Revisions" = "rev", "UNHCR(-)" = "unhcrexcl", "UNHCR" = "unhcr")),
    #                           #     width = 2
    #                           # ),
    #                           
    #                           # 
    #                           # column(
    #                           #     sliderInput("totalslideranimation", NULL, min = 1, max = 235, value = 1, step = 1, animate = animationOptions(interval = 5000, loop = TRUE)),
    #                           #     width = 3
    #                           # ),
    #                           
    #                           
    #                           # column(
    #                           #   selectInput("selected_country",
    #                           #               "Select a country",
    #                           #               choices = NULL
    #                           #   ),
    #                           #   width = 4
    #                           # ),
    #                           # column(
    #                           #   selectInput("selected_indicator",
    #                           #               "Select an indicator",
    #                           #               choices = NULL,
    #                           #               width = "100%"
    #                           #   ),
    #                           #   width = 5
    #                           # )
    #                  ),
    #                  style = "position:relative;z-index:10000;"
    #              ),
    #              
    #              #div(fluidRow(column(width = 10, style='padding:20px;'),style = "position:relative;z-index:10000;")),
    #              # plotlyOutput("plottotal"),
    #              
    #              
    #              
    #              
    #              #div(fluidRow(column(width = 10, style='padding:20px;'),style = "position:relative;z-index:10000;")),
    #              
    #              fluidRow(id = "agerow2",
    #                       
    #                       column(
    #                           # Need to do it hidden so it is flashes in the online application
    #                           # I unhide this button if run locally 
    #                           hidden(actionButton(inputId = "testAgeButton", label = "testAgeButton")), 
    #                           width = 1
    #                       )
    #              )
    #              
    #              
    #          ), # end of fluid page
    #          value = 1
    # ),
    
    tabPanel('About', 

             h3('Project team'),
             
             fluidPage(
                 div(HTML('<p><h4><a href="http://kirillandreev.com">Kirill Andreev</a>  <a href="https://www.linkedin.com/in/kirill-andreev-8bb12362">(LinkedIn)</a></h4></p>')),
                 fluidRow(
                     column(
                         plotOutput("about_ka",  height=300),
                         width = 4
                     ),
                 )#,style = "position:relative;z-index:10000;"
             ),
             
             fluidPage(
                 div(HTML('<p><h4>Yu Han <a href="https://www.linkedin.com/in/dr-eng-yu-han-85353176">(LinkedIn)</a></h4></p>')),
                 # fluidRow(
                 #     column(
                 #         plotOutput("about_01"),
                 #         width = 4
                 #     ),
                 # )# ,style = "position:relative;z-index:10000;"
             ),
             
             fluidPage(
                 div(HTML('<p><h4>Mun Sim Lai (Nicole)</h4></p>')),
                 # fluidRow(
                 #     column(
                 #         plotOutput("about_ka"),
                 #         width = 4
                 #     ),
                 # )# ,style = "position:relative;z-index:10000;"
             ),
             
             tags$hr(),  # This inserts a horizontal line
             
             # fluidPage(
             #     HTML('<p>Yu Han <a href="https://www.linkedin.com/in/dr-eng-yu-han-85353176">(LinkedIn)</a></p>'),
             #     fluidRow(
             #         column(
             #             plotOutput("about_yh"),
             #             width = 4
             #         ),
             #     )# ,style = "position:relative;z-index:10000;"
             # ),
             # 
             
             h3("Emotional Support Crew"),
             
             fluidPage(
                 # HTML('<hr style="width:100%"><p><h3>Emotional Support Crew</h3></p>'),  # CESO
                 div(HTML('<p><h4>Toby A</h4></p>')),
                 fluidRow(
                    column(
                        plotOutput("about_ta"),
                        width = 4
                    ),
                 )  # , style = "position:relative;z-index:10000;"
             ),
             
             tags$hr(),  
             
             h3("Suggested citation"),
             
             
             fluidPage(
                 div(HTML('<p>Andreev, K., Han Y., and Lai, M. (2025). <i>International Migrant Stocks at a Glance Application</i>. The paper presented at the 2025 annual meeting of Population Association of America, Washington D.C., United States</p>')),
                 
                 # fluidRow(
                 #     column(
                 #         plotOutput("about_ka"),
                 #         width = 4
                 #     ),
                 # )# ,style = "position:relative;z-index:10000;"
             )

    ) # comma is below so we can comment tghe block 

    # tabPanel("About",
    #          fluidPage(
    #             div(fluidRow(column(width = 10, style='padding:0px;'),style = "position:relative;z-index:10000;"), 
    #                 HTML('<p><h3>Emotional Support Team</h3></p>')  # CESO
    #             ),
    #                
    #             fluidRow(
    #                 column(
    #                     plotOutput("about_ta"),
    #                     width = 4
    #              ),
    #                    # column(
    #                    #   selectInput("selected_indicator",
    #                    #               "Select an indicator",
    #                    #               choices = NULL,
    #                    #               width = "100%"
    #                    #   ),
    #                    #   width = 5
    #                    # )
    #              ),
    #              style = "position:relative;z-index:10000;"
    #                
    #             )
    #          ), # end of fluidRow
    #          value = 3
    # ),
    
#debugon #######################################################################    
    # , tabPanel(title = "Debug",
    #          fluidPage(
    #              div(
    #                  fluidRow(
    #                      column(
    #                          actionButton(inputId = "btnSaveDatasets", label = "Save datasets"),
    #                          actionButton(inputId = "testButton", label = "test"),
    #                          width = 5
    #                      ),
    # 
    #                  ),
    # 
    #                  fluidRow(
    #                      column(
    #                          # https://stackoverflow.com/questions/26368192/how-to-insert-new-line-in-r-shiny-string
    #                          # textOutput("textOutput"),
    #                          htmlOutput("textOutput"),
    #                          width = 5
    #                      ),
    # 
    #                  ),
    # 
    #                  style = "position:relative;z-index:10000;"
    #              ),
    #          ), 
    #          value = 4
    # )
#debugoff ######################################################################    


  ), # tabsetPanel
  
  textOutput("textStatusBar"),
  
) # fluidPage
