library(shiny)
library(sp)
library(rgdal)
library(raster)
library(lattice)
library(plotGoogleMaps)
library(RColorBrewer)
library(shinyjs)
library(leaflet)
library(leafletGoogle)
library(shinyBS)

apikey <- "AIzaSyAQvaBc5_RruTllCvOxy3i9YNFYlaDzaJ8"

shinyUI(bootstrapPage(
  useShinyjs(),
  # Hidden input boxes to save the variable to 
  HTML(' <input type="text" id="userID" name="userID" style="display: none;"> '), 
  HTML(' <input type="text" id="username" name="username" style="display: none;"> '), 
  
  theme="cascade.css",
  # include the js code 
  #includeScript("get_user_id.js"),  
  useLeafletGoogle(apikey = apikey),
  tags$head(tags$link(rel="stylesheet", type="text/css", href = "style.css")),
  div(
    id = "loading_page",
    h1("Loading...")
  ),
  hidden(div(id = "main_content",
             navbarPage(id = "navbar",
                        title="MCDA Map Application",
                        windowTitle = "MCDA Map Application",
                        
                        
                        #####################################################################################################################
                        ########################### Panel 1 : Welcome #######################################################################
                        #####################################################################################################################
                        
                        tabPanel("Welcome",
                                 useShinyjs(),
                                 
                                 mainPanel(
                                   tags$div(id="welcome",
                                            uiOutput("welcome"),
                                            tags$br(),
                                            uiOutput("testdata"),
                                            includeHTML("./Help/aimOfApplication.html"), #HTML file with information about this page
                                            tags$br(),
                                            checkboxInput(inputId ="advanced",label = "Go as a Advanced user", value = FALSE),
                                            tags$br(),
                                            actionButton("pann2",label = "Let's select the project!"),
                                            tags$br(),
                                            tags$br(),
                                            tags$p("The application was realized with Shiny, R, CSS, HTML and JavaScript by the Information and Computational Sciences department of the James Hutton Institute"),
                                            img(src="Huttonlogo345.jpg", height = 200, width = 345, align = "center")
                                   ),
                                   tags$br(),tags$br(),tags$br()
                                 )
                        ),
                        
                        #####################################################################################################################
                        ######################### Panel 2 : Select the project ##############################################################
                        #####################################################################################################################
                        
                        tabPanel("Select the project",
                                 value = "tab2",
                                 sidebarLayout(
                                   sidebarPanel(
                                     actionButton("helpProject", "Help",icon = icon("question-circle")),
                                     tags$h3("Select a project"),
                                     tags$p(" "), #text
                                     uiOutput("renderProject"),
                                     fileInput('file', 'Use my own data'), 
                                     tags$div(id="BewareOwnData",
                                              tags$strong("Beware:"),
                                              tags$p("Nickname, Descriptions or Names must not contain spaces , use _"),
                                              tags$p("Use a different name than the already known Project for your folder"),
                                              tags$br(),
                                              downloadButton('downloadData', 'Download an example')),
                                     tags$br(),
                                     tags$br(),
                                     tags$strong(id="instructionselectdata","After you select My Data and upload your file you can press ok and then Select the project Area."),
                                     textOutput("erreur"),
                                     tags$head(tags$style("#erreur{color: red;}")),
                                     textOutput("uploadok"),
                                     tags$head(tags$style("#uploadok{color: blue;}")),
                                     actionButton('uploaded', 'ok'),
                                     tags$br(),
                                     tags$br(),
                                     actionButton('pann3',"Go to the Next Step")
                                   ),
                                   
                                   mainPanel(
                                       tags$div(id="InformationMyOwnData", includeHTML("./Help/InformationMyOwnData.html")), #HTML file with information about this page
                                       tags$div(id="InformationForest", includeHTML("./Help/InformationForest.html")), #HTML file with information about this page
                                       tags$div(id="InformationAgriculture", includeHTML("./Help/InformationAgriculture.html")) #HTML file with information about this page
                                              )
                                              )
                                     ),
                        
                        ######################################################################################################################
                        ################################ Panel 3 : Select the project area ###################################################
                        ######################################################################################################################
                        
                        tabPanel("Select subset of the project area",
                                 value = "tab3",
                                 sidebarLayout(
                                   sidebarPanel( 
                                     actionButton("helpArea", "Help",icon = icon("question-circle")),
                                     tags$br(),
                                     tags$h3("Select an area"),
                                     tags$div(id="divAllArea",
#USELESS#                                             #tags$p("You can choose to use all of the area or just a subset"),
                                              radioButtons("allareaselect",label = "Choose between the full area or a subset selection :",
                                                           choices = list(`All of the area`=0,
                                                                          `A subset area`=1))
                                     ),
                                      tags$div(id="divSubset",
#USELESS#                                                tags$p("You can choose a specific working area by drawing a polygon, a rectangle on the map or by a shapefile."),
                                                radioButtons("inputselect",label = "Choose between a drawn polygon, a rectangle and a shapefile selection : ",
                                                choices = list(`Draw a polygon selection`=0,
                                                                `Rectangle selection`=2,
                                                               `ShapeFile selection`=1)),
                                                actionButton('undoarea', 'Undo'),
                                                actionButton('cleararealeaf', 'Clear area')
                                      ),
                                      tags$br(),
                                     
                                      tags$i(id="sareazip","The file needs to be a .zip with the shapefile in it."),
                                      tags$br(),
                                      tags$i(id="sareaupload","Please upload ALL the files composing a shapefile (shp, dbf...)."),
                                      fileInput(inputId='sarea', label='Use with a shapefile', multiple=TRUE),         
                                      actionButton('selecta', 'Select'),
                                      actionButton('pann4', "Go to the next step")
                                     ),
                                   mainPanel(    
                                     leafletOutput("selectarealeaf", width = "100%", height = 500)
                                     )
                        )),
                        
                        #######################################################################################################################
                        ############################## Panel 4 : Select hard constraints ######################################################
                        #######################################################################################################################
                        
                        
                        tabPanel("Select hard constraints",
                                 value = "tab4",
                                 
                                 sidebarLayout(
                                   sidebarPanel(
                                     actionButton("helpHC", "Help",icon = icon("question-circle")),
                                     tags$h3("List of hard constraints"),
                                     tableOutput("matrixHC"),
                                     uiOutput("renderSelectHC"),
                                     tags$h3("Hard constraints chosen by the User"),
                                     tags$p("You can choose the hards constraints you want (four maximum)."),
                                     uiOutput("renderChoiceHC"),
                                     actionButton("HCbutton", "Ok")

                                   ),
                                   mainPanel(
                                     leafletOutput("mapShowHC", width = "100%", height = 600),
                                     tags$div(id="mapsidebarhc", class= "mycontainer commonwells", 
                                              checkboxInput(inputId="mapshowhcS",label = "Hide & Show the legend",
                                                            value = TRUE),
                                              numericInput(inputId="mapOpacityhc", "Opacity :",
                                                           min = 0,max = 100,value = 50),
                                              selectInput(inputId="mapHCSelect",label = "Base layer :",
                                                          choices = list(`Satellite (default)`=0,
                                                                         `Roadmap`=1,
                                                                         `Terrain`=2))
                                     )
                                     
                                   )
                                 )
                        ),
                        
                        ######################################################################################################################
                        ########################## Panel 5 : Select Criteria #################################################################
                        ######################################################################################################################
                        
                        tabPanel("Select criteria",
                                 value = "tab5",
                                 sidebarLayout( 
                                   sidebarPanel(
                                     actionButton(inputId = "helpCrit" ,label = "Help" ,icon = icon("question-circle")),
                                     div(style="text-align: center;", h2("List of criteria")),
                                     div(style="text-align: center;", bsButton(inputId = "positiveCriteria", label="Make change", size = "extra-small")),
                                     br(),
                                     uiOutput("matrixPN"),
                                     div(style="font-family: chalkduster; ", uiOutput("POSorNEG")),
                                     uiOutput("legendCriteria"),
                                     actionButton("changeCriteria", "Change done"),
                                     br(),
                                     uiOutput("renderSelectCriteria")
                                   ),
                                   mainPanel(
                                     leafletOutput("mapShowCriteria", width = "100%", height = 700),
                                     tags$div(id="mapsidebarCrit", class= "mycontainer commonwells",
                                              checkboxInput(inputId="mapshowcrit","Hide & Show the legend",
                                                            value = TRUE),
                                              numericInput(inputId="mapOpacitycrit", "Opacity :",
                                                           min = 0,max = 100,value = 50),
                                              selectInput(inputId="mapCritSelect",label = "Base layer :",
                                                          choices = list(`Satellite (default)`=0,
                                                                         `Roadmap`=1,
                                                                         `Terrain`=2))
                                     )
                                     )
                                   
                                 ),
                                 tags$div(id="choicecrit",
                                          sidebarLayout(
                                            sidebarPanel(
                                              tags$h3("Criteria chosen by the User"),
                                              tags$br(),
                                              tags$p("You can choose the criteria you want. 
                                                     Just put them in the right window. 
                                                     1 criterion is already selected."),
                                              tags$br(),
                                              tags$h4("Electre and LWC Method used"),
                                              tags$p("The Linear Weight Combination can be used : 
                                                     It combines maps of criteria in a single score map. 
                                                     You are able to modify the weight of this linear combination"),
                                              tags$br(),
                                              tags$p(tags$b("LWC :"),
                                                     tags$br(),
                                                     "Positive and negative criteria are normalized ",
                                                     tags$b("together")," to a total weight of 1.")
                                              ),
                                            mainPanel(tags$h4("Criteria"),
                                                      uiOutput("chooserpdisplayed"),
                                                      actionButton('pann6', "Go to the Next Step"))
                                              )    
                                              )    
                                 
                              
                                              ),
                        
                        ######################################################################################################################
                        ######################### Panel 6 : Select weights Advanced ##########################################################
                        ######################################################################################################################
                        
                        tabPanel("Select weights (adv)",
                                 value = "tab6b",
                                 sidebarLayout(
                                   
                                   
                                   #Left side
                                   sidebarPanel(
                                     actionButton("helpWeight", "Help",icon = icon("question-circle")),
                                     tags$h3("Criteria Weights"),
                                     tags$p("For each selected criteria, choose its weight between 0 and 1 and the method 
                                              you want. Then press Go!. If you want to store a combination, press Save!. 
                                              If you want to store your final combination press Save for real"),
                                     tags$h4("Criterias"),
                                     uiOutput("slidersp"),
                                     
                                     #div method selection
                                     tags$div(id="metsel",
                                              radioButtons("methodsmain",
                                                           label = h3("Method selection"),
                                                           choices = list("Method LWC" = 2, "Electre" = 3, "Method LWC-G" = 4),
                                                           selected = 2),
                                              tags$div(id="criterGroup",
                                                       uiOutput('nbGroup'),
                                                       uiOutput('GroupChoice'))
                                     ),
                                     tags$br(),
                                     actionButton("goButtonAdvanced", "Go"),
                                     tags$br(),
                                     tags$div(id='savePannel',
                                              # tags$br(),
                                              actionButton("saveButton", "Save weights in this session"),
                                              checkboxInput("bothMethods","I want to save the weights permanently for the both methods :"),
                                              actionButton("savePermanentlyButton", "Save weights permanently"),
                                              actionButton("CancelSave", "Cancel your save"),
                                              
                                              tags$br(),
                                              tags$p(""),
                                              tags$p(id="saveRasterButtonText","Update the saved raster by clicking on the button below"),
                                              actionButton("saveRasterButton", "Save raster"),
                                              tags$br(),
                                              tags$p(""),
                                              tags$p(id="downloadRasterbuttonText","Download the saved raster by clicking on the button below :"),
                                              uiOutput('downloadRasterbutton'),
                                              tags$br(),
                                              tags$div(actionButton("thresholdPannel", "Go to the next step"))
                                     )),
                                   
                                   #Right side
                                   mainPanel(tags$div(id="maparea1",
                                                      tags$h3("Mapping area"),
                                                      tags$p("The best value is 1 (blue) and the worst 0 (red) for the project chosen Expansion considering these criteria, the weights selected and the chosen method")
                                   ),
                                   leafletOutput("mapMain"),
                                   tags$div(id="mapsidebarmain", class= "mycontainer commonwells",
                                            checkboxInput(inputId="mapshowmain","Hide & Show the legend",
                                                          value = TRUE),
                                            numericInput(inputId="mapOpacitymain", "Opacity :",
                                                         min = 0,max = 100,value = 50),
                                            selectInput(inputId="mapMainSelect",label = "Base layer :",
                                                        choices = list(`Satellite (default)`=0,
                                                                       `Roadmap`=1,
                                                                       `Terrain`=2))),
                                   
                                   #div 2
                                   tags$div(id="histogar",
                                            tags$h3("Histogram"),
                                            tags$p("An histogram of the above map")),
                                   tags$br(),
                                   plotOutput("histo")
                                   )
                                 )
                        ),
                        ####################################################################################################################       
                        ############################# Panel 6 : Select weights basic #######################################################
                        ####################################################################################################################
                        
                        #### Something to fix is to create every output and everything which as been duplicated
                        tabPanel("Select weights",
                                 value = "tab6a",
                                 sidebarLayout(
                                   #Left side
                                   sidebarPanel(
                                     actionButton("helpWeighta", "Help", icon = icon("question-circle")),
                                     tags$h3("Criteria Weights"),
                                     tags$p("For each selected criteria, choose its weight between 0 and 1 and 
                                            the method you want. Then press Go!. If you want to store a combination, 
                                            press Save!. If you want to store your final combination press Save for real"),
                                     tags$h4("Criterias"),
                                     uiOutput("sliderspa"),
                                     
                                     #div method selection
                                     tags$div(id="metsela",
                                              radioButtons("methodsmaina",
                                                           label = h3("Method selection"),
                                                           choices = list("Method LWC" = 2, "Electre" = 3),
                                                           selected = 2)
                                     ),  
                                     tags$br(),
                                     actionButton("goButtonBasic", "Go"),
                                     tags$br(),
                                     uiOutput('downloadRasterbuttona'),
                                     tags$br(),
                                     tags$div(actionButton("thresholdPannela", "Go to the next step"))
                                     ),
                                   
                                   #Right side
                                   mainPanel(tags$div(id="maparea1a",
                                                      tags$h3("Mapping area"),
                                                      tags$p("The best value is 1 (blue) and the worst 0 (red) for the 
                                                                    project chosen Expansion considering these criteria, the weights 
                                                                    selected and the chosen method")
                                   ),
                                   leafletOutput("mapMaina"),
                                   tags$div(id="mapsidebarmaina", class= "mycontainer commonwells",
                                            checkboxInput(inputId="mapshowmaina","Hide & Show the legend",
                                                          value = TRUE),
                                            numericInput(inputId="mapOpacitymaina", "Opacity :",
                                                         min = 0,max = 100,value = 50),
                                            selectInput(inputId="mapMainaSelect",label = "Base layer :",
                                                        choices = list(`Satellite (default)`=0,
                                                                       `Roadmap`=1,
                                                                       `Terrain`=2)))
                                   )
                                   )
                        ),

                        
                        #######################################################################################################################
                        ############################### Panel 7 : Select Threshold ############################################################
                        #######################################################################################################################
                        
                          tabPanel("Threshold",
                                   value = "tab7",
                                   sidebarLayout(
                                     #Left side
                                     sidebarPanel(
                                       actionButton("helpThr", "Help", icon = icon("question-circle")),
                                       tags$h3("Threshold"),
                                       
                                       uiOutput("renderThr"),
                                      
                                       uiOutput("renderThrQ"),
                                       actionButton("goButtonThr", "Go"),
                                       tags$br(),tags$br(),
                                       uiOutput("stata")
                                     ),
                                     #Right side          
                                     mainPanel(
                                       tags$div(id="mapareathr",
                                                tags$h3("Mapping area"),
                                                tags$p("Best pixels under the threshold are in blue, the others are in red.")),
                                       # uiOutput("mapThr"),
                                       leafletOutput("mapThr"),
                                       tags$div(id="mapsidebarThr", class= "mycontainer commonwells",
                                                checkboxInput(inputId="mapshowThr","Hide & Show the legend",
                                                              value = TRUE),
                                                numericInput(inputId="mapOpacityThr", "Opacity :",
                                                             min = 0,max = 100,value = 50),
                                                selectInput(inputId="mapThrSelect",label = "Base layer :",
                                                            choices = list(`Satellite (default)`=0,
                                                                           `Roadmap`=1,
                                                                           `Terrain`=2)))
                                     )
                                   )
                          ),
                        
                        
                        
                        ###############################################################################################################     
                        ################################# Panel for advanced user #####################################################
                        ###############################################################################################################       
                        
                        
                        tabPanel("Advanced User",
                                 value = "tabAU",
                                 tabsetPanel(id="advanceduserpannel",
                                             
                                             ##############################################################
                                             ########## Panel Compare methods #############################
                                             ##############################################################
                                             
                                             tabPanel("Compare methods",
                                                      sidebarLayout(
                                                        #Left side
                                                        sidebarPanel(
                                                          actionButton("helpCmpMth", "Help", icon = icon("question-circle")),
                                                          tags$br(),
                                                          tags$br(),
                                                          checkboxGroupInput("checkGroupCM",
                                                                             label = "Choose the methods to be compared:",
                                                                             choices = list("LWC" = 2, "ELECTRE" = 3),
                                                                             selected = NULL),
                                                          actionButton("comparebutton","Ok"),tags$br(),uiOutput("renderSelectlayerCM"),
                                                          tags$h3("Scatterplot"),
                                                          tags$p("A graph of plotted points that show the relationship between the values obtained with each method chosen."),
                                                          plotOutput("scatter"),
                                                          actionButton("ShowThr","Show the compared threshold"),
                                                          tags$h3("Compare threshold"), 
                                                          uiOutput("renderSelectlayerThrCM"),
                                                          tags$p("Select the number of ha you want"),
                                                          uiOutput("renderThrCM"),
                                                          tags$p("Select the percentage of area you want"),
                                                          uiOutput("renderThrCMQ")
                                                        ),
                                                        #Right side
                                                        mainPanel(
                                                          tags$div(id="mapareaCM",
                                                                   tags$h3("Mapping area"),textOutput("txtmapCM")
                                                          ),
                                                          leafletOutput("mapCM"),
                                                          tags$div(id="mapsidebarCM", class= "mycontainer commonwells",
                                                                   checkboxInput(inputId="mapshowCM","Hide & Show the legend",
                                                                                 value = TRUE),
                                                                   numericInput(inputId="mapOpacityCM", "Opacity :",
                                                                                min = 0,max = 100,value = 50),
                                                                   selectInput(inputId="mapCMSelect",label = "Base layer :",
                                                                               choices = list(`Satellite (default)`=0,
                                                                                              `Roadmap`=1,
                                                                                              `Terrain`=2))),
                                                          tags$br(),
                                                          tags$div(id="mapareaCM",
                                                                   tags$h3("Threshold map")
                                                          ),
                                                          leafletOutput("mapThrCM"),
                                                          textOutput("txtgoodarea"),
                                                          tags$div(id="mapsidebarThrCM", class= "mycontainer commonwells",
                                                                   checkboxInput(inputId="mapshowThrCM","Hide & Show the legend",
                                                                                 value = TRUE),
                                                                   numericInput(inputId="mapOpacityThrCM", "Opacity :",
                                                                                min = 0,max = 100,value = 50),
                                                                   selectInput(inputId="mapThrCMSelect",label = "Base layer :",
                                                                               choices = list(`Satellite (default)`=0,
                                                                                              `Roadmap`=1,
                                                                                              `Terrain`=2))),
                                                          
                                                          tags$br(),tags$br(),tags$br()
                                                        )
                                                      )
                                             ),
                                             
                                             ######################################################  
                                             ################### Panel Stats ######################
                                             ######################################################
                                             
                                             tabPanel("Stats",
                                                      #div 1
                                                      tags$div(id="weista", 
                                                               actionButton("helpStats", "Help", icon = icon("question-circle")),
                                                               tags$br(),
                                                               tags$h3("Weights"),
                                                               tags$br(),
                                                               # tags$h4("Weights P"),
                                                               tags$div(id="matrixCurrentWeight",
                                                                        tags$style(type="text/css",HTML(".mymatrix td { max-width: 250px; white-space: nowrap; text-overflow: ellipsis; overflow-x: hidden;}")),
                                                                        class="mymatrix",
                                                                        tableOutput(outputId="matrixCurrentWeightsP"))
                                                               
                                                               
                                                      ),
                                                      # tags$br(),
                                                      # tags$h4("Weights N"),
                                                      # tableOutput("matrixCurrentWeightsN")),
                                                      tags$br(),
                                                      
                                                      tags$div(id="weihi",
                                                               tags$h3("Criteria History Matrix"),
                                                               # tags$h4("Method LWC"),
                                                               # tableOutput("christomatLWC1"),
                                                               # conditionalPanel(condition = "input.saveButton != 0",
                                                               #                  downloadButton('downloadCWH_LWC1', 'Download')),
                                                               tags$h4("Method LWC"),
                                                               tableOutput("christomatLWC2"),
                                                               conditionalPanel(condition = "input.saveButton != 0",
                                                                                downloadButton('downloadCWH_LWC2', 'Download')),
                                                               tags$h4("Method Electre"),
                                                               tableOutput("christomatElectre"),
                                                               conditionalPanel(condition = "input.saveButton != 0",
                                                                                downloadButton('downloadCWH_Electre', 'Download'))
                                                      )            
                                             ),
                                             
                                             ##################################################                      
                                             ############### Panel Average ####################
                                             ##################################################
                                             
                                             tabPanel("Average",
                                                      sidebarPanel(
                                                        actionButton("helpAverage", "Help", icon = icon("question-circle")),
                                                        tags$br(),
                                                        tags$h3("Average"),
                                                        tags$br(),
                                                        tags$p(id="tabAV_LWC2_desc","Criteria and the associated weight chosen for this study :"),
                                                        uiOutput("tabAV_LWC2"),
                                                        tags$p(id="tabAV_Electre_desc","Criteria and the associated weight chosen for this study :"),
                                                        uiOutput("tabAV_Electre")
                                                      ),
                                                      mainPanel(
                                                        # tags$div(id="mapareaAV",
                                                        #          tags$h3("Average User Map Method LWC"),
                                                        #          tags$p("It corresponds to the map with the geometric mean of each criteria selected by the user with method LWC. The best value is 1 (blue) and the worst 0 (red)")),
                                                        # uiOutput("mapAV_LWC1"),
                                                        # tags$br(),
                                                        # uiOutput("tabAV_LWC1"),
                                                        # img(src="bigorb.jpg", height = 40, width = 40, align = "center"),
                                                        
                                                        tags$div(id="mapareaAV",
                                                                 # actionButton("helpAverage", "Help"),
                                                                 tags$h3("Average User Map Method LWC"),
                                                                 tags$p("It corresponds to the map with the geometric mean of each criteria selected by the user with method LWC. The best value is 1 (blue) and the worst 0 (red)")),
                                                        leafletOutput("mapAV_LWC2"),
                                                        tags$div(id="mapsidebarAv", class= "mycontainer commonwells",
                                                                 checkboxInput(inputId="mapshowAv","Hide & Show the legend",
                                                                               value = TRUE),
                                                                 numericInput(inputId="mapOpacityAv", "Opacity :",
                                                                              min = 0,max = 100,value = 50),
                                                                 selectInput(inputId="mapAvSelect",label = "Base layer :",
                                                                             choices = list(`Satellite (default)`=0,
                                                                                            `Roadmap`=1,
                                                                                            `Terrain`=2))),
                                                        
                                                        
                                                        tags$br(),
                                                        # uiOutput("tabAV_LWC2"),
                                                        img(src="bigorb.jpg", height = 40, width = 40, align = "center"),
                                                        
                                                        tags$div(id="mapareaAV",
                                                                 tags$h3("Average User Map Method Electre"),
                                                                 tags$p("It corresponds to the map with the geometric mean of each criteria selected by the user with method Electre. The best value is 1 (blue) and the worst 0 (red)")),
                                                        leafletOutput("mapAV_Electre"),
                                                        tags$div(id="mapsidebarAv2", class= "mycontainer commonwells",
                                                                 checkboxInput(inputId="mapshowAv2","Hide & Show the legend",
                                                                               value = TRUE),
                                                                 numericInput(inputId="mapOpacityAv2", "Opacity :",
                                                                              min = 0,max = 100,value = 50),
                                                                 selectInput(inputId="mapAvSelect2",label = "Base layer :",
                                                                             choices = list(`Satellite (default)`=0,
                                                                                            `Roadmap`=1,
                                                                                            `Terrain`=2))),
                                                        
                                                        
                                                        
                                                        tags$br(),
                                                        # uiOutput("tabAV_Electre"),
                                                        img(src="bigorb.jpg", height = 40, width = 40, align = "center")
                                                      )
                                             ),
                                             
                                             #################################################
                                             ############### Panel Consensus #################
                                             #################################################
                                             
                                             tabPanel("Consensus",
                                                      sidebarPanel(
                                                        actionButton("helpConsensus", label = "Help", icon = icon("question-circle")),
                                                        tags$br(),
                                                        uiOutput("tabCON_LWC2"),
                                                        tags$br(),
                                                        uiOutput("tabCON_Electre")
                                                        
                                                      ),
                                                      mainPanel(
                                                        ##Div 1#
                                                        # tags$div(id="mapareaCON",
                                                        #          tags$h3("Users Consensus Map Method LWC"),
                                                        #          tags$p("It corresponds to geometric mean of the final combinations stored on the website with method LWC. The best value is 1 (blue) and the worst 0 (red)")),
                                                        # uiOutput("mapCON_LWC1"),
                                                        # tags$br(),
                                                        # uiOutput("tabCON_LWC1"),
                                                        # img(src="bigorb.jpg", height = 40, width = 40, align = "center"),
                                                        
                                                        ##Div 2#
                                                        tags$div(id="mapareaCON",
                                                                 
                                                                 
                                                                 tags$h3("Users Consensus Map Method LWC"),
                                                                 
                                                                 
                                                                 tags$p("It corresponds to geometric mean of the final combinations stored on the website with method LWC. 
                                                                        The best value is 1 (blue) and the worst 0 (red)")),
                                                        # uiOutput("mapCON_LWC2"),
                                                        leafletOutput("mapCON_LWC2"),
                                                        tags$div(id="mapsidebarCON", class= "mycontainer commonwells",
                                                                 checkboxInput(inputId="mapshowCON","Hide & Show the legend",
                                                                               value = TRUE),
                                                                 numericInput(inputId="mapOpacityCON", "Opacity :",
                                                                              min = 0,max = 100,value = 50),
                                                                 selectInput(inputId="mapCONSelect",label = "Base layer :",
                                                                             choices = list(`Satellite (default)`=0,
                                                                                            `Roadmap`=1,
                                                                                            `Terrain`=2))),
                                                        
                                                        tags$br(),
                                                        
                                                        #img(src="bigorb.jpg", height = 40, width = 40, align = "center"),
                                                        
                                                        ##Div 3#
                                                        tags$div(id="mapareaCON",
                                                                 tags$h3("Users Consensus Map Method Electre"),
                                                                 tags$p("It corresponds to geometric mean of the final combinations stored on the website with method Electre.
                                                       The best value is 1 (blue) and the worst 0 (red)")),
                                                        # uiOutput("mapCON_Electre"),
                                                        leafletOutput("mapCON_Electre"),
                                                        tags$div(id="mapsidebarCON2", class= "mycontainer commonwells",
                                                                 checkboxInput(inputId="mapshowCON2","Hide & Show the legend",
                                                                               value = TRUE),
                                                                 numericInput(inputId="mapOpacityCON2", "Opacity :",
                                                                              min = 0,max = 100,value = 50),
                                                                 selectInput(inputId="mapCONSelect2",label = "Base layer :",
                                                                             choices = list(`Satellite (default)`=0,
                                                                                            `Roadmap`=1,
                                                                                            `Terrain`=2))),
                                                        tags$br()#,
                                                        #img(src="bigorb.jpg", height = 40, width = 40, align = "center")
                                                                 )
                                             ),
                                             
                                             ##########################################################
                                             ############### Panel Compare projects ###################
                                             ##########################################################
                                             
                                          #   tabPanel("Compare projects",
                                           #           sidebarLayout(
                                            #            
                                             #           
                                              #          #Left side
                                                #        sidebarPanel(
                                               #           actionButton("helpCmpProj", "Help", icon = icon("question-circle")),
                                                #          uiOutput("selectrastersaved"),
                                                #          actionButton("CPrbutton", "Ok"),
                                              #            tags$br(),
                                              #            uiOutput("downloadCPrbutton"), 
                                              #            uiOutput("downloadCPrLegendbutton"),
                                              #            plotOutput("Legend"),
                                              #            tags$br(),
                                              #            tags$br(),
                                              ##            tags$h3("Threshold map"),
                                               #           uiOutput("selectlayerCPrcompared"),
                                             #             tags$br(),
                                              #            tags$br(),
                                            #  #            uiOutput("renderThrCPr"), 
                                              #            uiOutput("renderThrCPrP"),
                                            #              uiOutput("textthr")
                                              #          ),
                                                        
                                                        #Right side
                                              #          mainPanel(
                                             #             tags$div(id="mapareaCPr",
                                            #                       tags$h3("Mapping area"),
                                              #                     tags$p("For each pixel, its values in the rasters selected are compared. The final value corresponds to the layer which has the higher value for the pixel.")
                                              #            ),
                                              #            # uiOutput("mapCPr"),
                                              #            leafletOutput("mapCPr"),
                                             #             tags$div(id="mapsidebarCpr", class= "mycontainer commonwells",
                                            #                       checkboxInput(inputId="mapshowCPr","Hide & Show the legend",
                                              #                                   value = TRUE),
                                              #                     numericInput(inputId="mapOpacityCPr", "Opacity :",
                                              #                                  min = 0,max = 100,value = 50),
                                              #                     selectInput(inputId="mapCPrSelect",label = "Base layer :",
                                              #                                 choices = list(`Satellite (default)`=0,
                                              #                                                `Roadmap`=1,
                                              #                                                `Terrain`=2))),
                                              #            tags$br(),
                                              #            tags$br(),
                                                          
                                              #            tags$div(id="mapareaCPr",
                                             #                      tags$h3("Threshold map"),
                                            #                       tags$p("Best pixels under the threshold are in blue, the others are in red.")
                                              #            ),
                                              #            # uiOutput("mapCPrThr"),
                                              #            leafletOutput("mapCPrThr"),
                                              #            tags$div(id="mapsidebarThr2", class= "mycontainer commonwells",
                                               #                    checkboxInput(inputId="mapshowCPr2","Hide & Show the legend",
                                              #                                   value = TRUE),
                                              #                     numericInput(inputId="mapOpacityCPr2", "Opacity :",
                                              #                                  min = 0,max = 100,value = 50),
                                              #                     selectInput(inputId="mapCPrSelect2",label = "Base layer :",
                                              #                                 choices = list(`Satellite (default)`=0,
                                              #                                                `Roadmap`=1,
                                              #                                                `Terrain`=2)))
                                              #          )
                                              #        )
                                             #),
                                             
                                             #########################################################  
                                             ############# Panel Sensitivity Analysis ################
                                             #########################################################
                                             
                                             tabPanel("Sensitivity Analysis",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          actionButton("helpSensitivAnalysis", "Help", icon = icon("question-circle")),
                                                          tags$br(),
                                                          tags$h3("Sensitivity Analysis"),
                                                          # tags$p("Please choose the disruption :"),
                                                          tags$p("Select the number of ha you want"),
                                                          uiOutput("renderSA1"),
                                                          tags$p("Select the percentage you want"),
                                                          uiOutput("renderSA2"),
                                                          uiOutput(outputId="renderThrSA"),
                                                          uiOutput(outputId="renderThrQSA"),
                                                          actionButton(inputId="goButtonThrSA", label = "Run"),
                                                          downloadButton("downloadSA", "Download the data!"),
                                                          tags$br(),
                                                          tags$br()
                                                        ),
                                                        mainPanel(
                                                          
                                                          tags$div(id="DisplayOfSA",class="commonwells2",
                                                                   tags$h3("Display"),
                                                                   uiOutput("renderSelectCriteriaToShow"),
                                                                   uiOutput("renderChangeOnWeight")),
                                                          leafletOutput("mapThrSA"),
                                                          tags$div(id="mapsidebarThrSA", class= "mycontainer commonwells",
                                                                   checkboxInput(inputId="mapshowThrSA","Hide & Show the legend",
                                                                                 value = TRUE),
                                                                   numericInput(inputId="mapOpacityThrSA", "Opacity :",
                                                                                min = 0,max = 100,value = 50),
                                                                   selectInput(inputId="mapThrSelectSA",label = "Base layer :",
                                                                               choices = list(`Satellite (default)`=0,
                                                                                              `Roadmap`=1,
                                                                                              `Terrain`=2))),
                                                          tags$div(id="histogar",
                                                                   tags$h3("Pie Chart"),
                                                                   tags$p("This pie shows the distribution of value in the raster.These values represent the evolution in relation to the threshold after disturbing the weights. ")),
                                                          tags$br(),
                                                          plotOutput("histoSA")
                                                        )
                                                         
                                                        )
                                                      )   
                                             )
                                 )

                        )       

                                 ))))
